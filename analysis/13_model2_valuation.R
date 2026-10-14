# 13_model2_valuation.R -- MODEL 2 (market side): what does a profile's composition signal?
#
# Worker-level multilevel model of the (log) asking rate:
#
#   log rate_j = b1 AuthoredSpecific_j + b2 SharedGeneric_j + b3 SharedSpecific_j      (P2a, P2b)
#              + [b1', b4'] (AuthoredSpecific_j, Shared_j) x VerifiedRecord_j          (P3, market side)
#              + controls_j + country FE + occupation FE
#              + u_city(j) + sharer_j * v_family(j) + e_j                              (P2c: provenance)
#
# Shares are proportions of the profile's codable sentences (reference: authored generic).
# v_family is a random intercept for the template family that covers most of the worker's
# shared sentences (only for workers who share); u_city a random intercept for the city.
# Var(v)/(Var(v)+Var(e)) measures how informative a worker's template provenance is about
# their price, net of everything else in the model.
#
# Output: tables/model2_*.csv, numbers/13_model2.csv

source("analysis/00_setup.R")
suppressPackageStartupMessages({ library(fixest); library(lme4) })
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0]
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, wid, maxJ, cid)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, specific, macro)]
SA <- as.data.table(read_parquet(file.path(DERIVED, "script_attrs.parquet")))[, .(cid, family, adopters)]
X  <- merge(SR, C, by = "sid")
comp <- X[, .(authored_spec = mean(maxJ < 1 & specific), shared_gen = mean(maxJ >= 1 & !specific), shared_spec = mean(maxJ >= 1 & specific),
              shared_any = mean(maxJ >= 1), shared07 = mean(maxJ >= 0.7),
              shared_assure = mean(maxJ >= 1 & macro %in% c("assurance", "encounter formula")), shared_capab = mean(maxJ >= 1 & macro == "capability-bearing"),
              authored_capab = mean(maxJ < 1 & macro == "capability-bearing")), by = wid]
# template family covering most of a worker's shared sentences
fam <- merge(X[maxJ >= 1 & !is.na(cid), .(wid, cid)], SA, by = "cid")[, .(n = .N, size = max(adopters)), by = .(wid, family)]
setorder(fam, wid, -n, -size); fam <- fam[, .SD[1], by = wid][, .(wid, family = paste0("f", family))]
D <- merge(merge(W, comp, by = "wid"), fam, by = "wid", all.x = TRUE)
D[, sharer := as.integer(shared_any > 0)]
D[is.na(family), family := "none"]
big_c <- D[, .N, by = country][N >= 200 & !is.na(country), country]
D[, `:=`(country_f = factor(fifelse(country %in% big_c, country, "Other")), occ_f = factor(fifelse(is.na(occ1), "None", occ1)),
         city_f = factor(fifelse(is.na(city_key), paste0("nocity::", country), city_key)), lchars = log(desc_chars))]
D[, record := factor(fcase(totalJobsWorked == 0, "0 jobs", totalJobsWorked < 10, "1-9 jobs", totalJobsWorked < 50, "10-49 jobs", default = "50+ jobs"), levels = c("0 jobs", "1-9 jobs", "10-49 jobs", "50+ jobs"))]
D[, rating_f := factor(fcase(!rated, "unrated", rating >= 4.95, ">=4.95", rating >= 4.8, "4.8-4.95", default = "<4.8"), levels = c("unrated", ">=4.95", "4.8-4.95", "<4.8"))]
R <- D[!is.na(lrate) & !is.na(tenure_yrs)]
rec("m2_workers", nrow(R), "workers in the model frame (asking rate 3-500 USD, tenure observed)")
rec("m2_sharers", R[sharer == 1, .N], "... of whom share at least one sentence verbatim")
rec("m2_families", R[sharer == 1, uniqueN(family)], "template families represented among sharers")
rec("m2_cities", uniqueN(R$city_f), "city groups")
save_table(R[, .(mean_authored_spec = mean(authored_spec), mean_shared_gen = mean(shared_gen), mean_shared_spec = mean(shared_spec), sd_lrate = sd(lrate))], "model2_descriptives")

ctrl <- "record + ljobs + lhours + rating_f + splines::ns(tenure_yrs, 5) + agency + n_occ + lchars + log(n_c)"
# ---- Model 2 in its fixed-effects form (country-clustered SEs) ---------------------------
m2a <- feols(as.formula(paste("lrate ~ authored_spec + shared_gen + shared_spec +", ctrl, "| country_f + occ_f")), data = R, cluster = ~country_f)
R[, `:=`(d1 = as.integer(record == "1-9 jobs"), d2 = as.integer(record == "10-49 jobs"), d3 = as.integer(record == "50+ jobs"))]
int_terms <- "authored_spec:d1 + authored_spec:d2 + authored_spec:d3 + shared_any:d1 + shared_any:d2 + shared_any:d3"
m2b <- feols(as.formula(paste("lrate ~ authored_spec + shared_gen + shared_spec +", int_terms, "+", ctrl, "| country_f + occ_f")), data = R, cluster = ~country_f)
pc <- function(b) 100 * (exp(b) - 1)
tidyf <- function(m, label) { ct <- coeftable(m); data.table(model = label, term = rownames(ct), est = ct[, 1], se = ct[, 2], pct = pc(ct[, 1])) }
T2 <- rbind(tidyf(m2a, "M2 composition (FE, country-clustered)"), tidyf(m2b, "M2 x verified record (FE, country-clustered)"))
T2 <- T2[grepl("authored|shared", term)]
save_table(T2, "model2_fixed_effects_clustered"); print(T2, digits = 3)
b <- coef(m2a)
rec("m2_pct_authored_spec", pc(b["authored_spec"]), "% rate difference, profile entirely authored-specific vs entirely authored-generic")
rec("m2_pct_shared_gen", pc(b["shared_gen"]), "% rate difference, entirely shared-generic vs authored-generic")
rec("m2_pct_shared_spec", pc(b["shared_spec"]), "% rate difference, entirely shared-specific vs authored-generic")
# record-specific slopes (delta method): level 0 = main effects; level k adds the dk interaction
V <- vcov(m2b); bb <- coef(m2b); lv <- levels(R$record)
fixnm <- function(g) vapply(g, function(x) { if (x %in% names(bb)) x else { p <- strsplit(x, ":")[[1]]; paste(rev(p), collapse = ":") } }, "")
lin <- function(g) { g <- fixnm(g); w <- rep(1, length(g)); c(est = sum(bb[g]), se = sqrt(as.numeric(t(w) %*% V[g, g, drop = FALSE] %*% w))) }
sl <- rbindlist(lapply(seq_along(lv), function(k) {
  add_sh <- if (k == 1) character(0) else paste0("shared_any:d", k - 1)
  add_au <- if (k == 1) character(0) else paste0("authored_spec:d", k - 1)
  sg <- lin(c("shared_gen", add_sh)); ss <- lin(c("shared_spec", add_sh)); au <- lin(c("authored_spec", add_au))
  data.table(record = lv[k], shared_gen_est = sg["est"], shared_gen_se = sg["se"], shared_spec_est = ss["est"], shared_spec_se = ss["se"],
             authored_spec_est = au["est"], authored_spec_se = au["se"])
}))
sl[, `:=`(shared_gen_pct = pc(shared_gen_est), shared_spec_pct = pc(shared_spec_est), authored_spec_pct = pc(authored_spec_est))]
save_table(sl, "model2_slopes_by_record"); print(sl, digits = 3)
for (i in seq_len(nrow(sl))) {
  tag <- gsub("[^0-9a-z]", "_", sl$record[i])
  rec(paste0("m2_shared_gen_pct_", tag), sl$shared_gen_pct[i], "% rate difference, entirely shared-generic vs authored-generic, within record group")
  rec(paste0("m2_shared_spec_pct_", tag), sl$shared_spec_pct[i], "% rate difference, entirely shared-specific vs authored-generic, within record group")
  rec(paste0("m2_authored_spec_pct_", tag), sl$authored_spec_pct[i], "% rate difference, entirely authored-specific vs authored-generic, within record group")
}

# ---- Model 2 in its multilevel form: provenance (template family) and city random effects --
f_fix <- paste("lrate ~ authored_spec + shared_gen + shared_spec +", int_terms, "+", ctrl, "+ country_f + occ_f")
m2_full <- lmer(as.formula(paste(f_fix, "+ (1 | city_f) + (0 + sharer | family)")), data = R, REML = FALSE,
                control = lmerControl(calc.derivs = FALSE))
m2_nofam <- lmer(as.formula(paste(f_fix, "+ (1 | city_f)")), data = R, REML = FALSE, control = lmerControl(calc.derivs = FALSE))
vc <- as.data.table(VarCorr(m2_full))[, .(grp, var = vcov)]
print(vc); save_table(vc, "model2_variance_components")
v_fam <- vc[grp == "family", var]; v_city <- vc[grp == "city_f", var]; v_res <- vc[grp == "Residual", var]
rec("m2_var_family", v_fam, "variance of template-family random intercepts (sharers)")
rec("m2_var_city", v_city, "variance of city random intercepts")
rec("m2_var_resid", v_res, "residual variance")
rec("m2_icc_provenance", v_fam / (v_fam + v_res), "provenance share: family variance / (family + residual), among sharers, net of all fixed effects and city")
lr <- anova(m2_nofam, m2_full)
rec("m2_lr_chisq_family", lr$Chisq[2], "likelihood-ratio chi-square, adding template-family random intercepts")
rec("m2_lr_p_family", lr$`Pr(>Chisq)`[2], "p-value (boundary; conservative)")
fe <- summary(m2_full)$coefficients
fe <- data.table(term = rownames(fe), est = fe[, 1], se = fe[, 2])[grepl("authored|shared", term)][, pct := pc(est)]
save_table(fe, "model2_fixed_effects_multilevel"); print(fe, digits = 3)
rec("m2ml_pct_authored_spec", fe[term == "authored_spec", pct], "multilevel model: authored-specific share (0 jobs reference for interactions)")
rec("m2ml_pct_shared_gen", fe[term == "shared_gen", pct], "multilevel model: shared-generic share")
rec("m2ml_pct_shared_spec", fe[term == "shared_spec", pct], "multilevel model: shared-specific share")
# families with the largest shrunken price effects (for interpretation)
re <- ranef(m2_full)$family
re <- data.table(family = rownames(re), effect = re[, 1])[family != "none"]
re <- merge(re, R[sharer == 1, .(adopters_in_frame = .N), by = family], by = "family")
lead <- merge(SA[, .(family = paste0("f", family), cid, adopters)], as.data.table(read_parquet(file.path(DERIVED, "scripts.parquet")))[, .(cid, text)], by = "cid")[order(-adopters)][, .SD[1], by = family]
re <- merge(re, lead[, .(family, lead = substr(text, 1, 100))], by = "family", all.x = TRUE)
save_table(re[adopters_in_frame >= 30][order(effect)][c(1:12, (.N - 11):.N)], "model2_extreme_families")
save_numbers("13_model2")
