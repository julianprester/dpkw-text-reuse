# 14_robustness.R -- One robustness table for both models (cf. Safadi et al. 2024, Table 7).
# Each check re-estimates Model 1 (FE logit) and Model 2 (FE OLS, country-clustered); the
# provenance component of Model 2 (template-family random intercepts) is re-estimated for the
# checks flagged with ml = TRUE.
#
# Output: tables/robustness_summary.csv, tables/robustness_estimates.csv, numbers/14_robustness.csv

source("analysis/00_setup.R")
suppressPackageStartupMessages({ library(fixest); library(lme4) })
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, wid, pos_c, n_c, nwords, maxJ, cid)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, move, macro, spec_n, spec_max, has_number)]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0]
SA <- as.data.table(read_parquet(file.path(DERIVED, "script_attrs.parquet")))[, .(cid, family, adopters)]
X  <- merge(merge(SR, C, by = "sid"), W[, .(wid, totalJobsWorked, agency, country, text_dated)], by = "wid")
X[, func := factor(fcase(move %in% c("identity", "credential", "capability"), "individuating claim", move %in% c("character", "process"), "assurance",
                         move == "encounter", "encounter formula", default = "other"), levels = c("individuating claim", "assurance", "encounter formula", "other"))]
X[, record := factor(fcase(totalJobsWorked == 0, "0 jobs", totalJobsWorked < 10, "1-9 jobs", totalJobsWorked < 50, "10-49 jobs", default = "50+ jobs"), levels = c("0 jobs", "1-9 jobs", "10-49 jobs", "50+ jobs"))]
X[, `:=`(number = as.integer(has_number), lwords = log(nwords), first = as.integer(pos_c == 1), last = as.integer(pos_c == n_c & n_c > 1))]
top10 <- W[!is.na(country), .N, by = country][order(-N)][1:10, country]
big_c <- W[, .N, by = country][N >= 200 & !is.na(country), country]
W[, `:=`(country_f = factor(fifelse(country %in% big_c, country, "Other")), occ_f = factor(fifelse(is.na(occ1), "None", occ1)),
         city_f = factor(fifelse(is.na(city_key), paste0("nocity::", country), city_key)), lchars = log(desc_chars))]
W[, record := factor(fcase(totalJobsWorked == 0, "0 jobs", totalJobsWorked < 10, "1-9 jobs", totalJobsWorked < 50, "10-49 jobs", default = "50+ jobs"), levels = c("0 jobs", "1-9 jobs", "10-49 jobs", "50+ jobs"))]
W[, rating_f := factor(fcase(!rated, "unrated", rating >= 4.95, ">=4.95", rating >= 4.8, "4.8-4.95", default = "<4.8"), levels = c("unrated", ">=4.95", "4.8-4.95", "<4.8"))]
W[, `:=`(d1 = as.integer(record == "1-9 jobs"), d2 = as.integer(record == "10-49 jobs"), d3 = as.integer(record == "50+ jobs"))]
# Experience controls: jobs and work hours; no timestamp-derived age spline.
ctrl <- "record + ljobs + lhours + rating_f + agency + n_occ + lchars + log(n_c) + text_dated"
int_terms <- "authored_spec:d1 + authored_spec:d2 + authored_spec:d3 + shared_any:d1 + shared_any:d2 + shared_any:d3"

run_check <- function(label, thr = 1, spec_rule = "kl05", sent_filter = NULL, worker_filter = NULL, m1_spec = "lspec", ml = FALSE, m1_lpm = FALSE, dv = "lrate") {
  cat("\n=====", label, "\n")
  Y <- copy(X)
  if (!is.null(sent_filter)) Y <- Y[eval(sent_filter)]
  Y[, shared := as.integer(maxJ >= thr)]
  Y[, specific := if (spec_rule == "kl05") spec_n > 0 else spec_max >= 1]
  Y[, lspec := if (m1_spec == "lspec") log1p(spec_n) else spec_max]
  wf <- W; if (!is.null(worker_filter)) wf <- wf[eval(worker_filter)]
  Y <- Y[wid %in% wf$wid]
  # ---- Model 1
  f1 <- shared ~ lspec + number + func + first + last + lwords | wid
  f1i <- shared ~ lspec + number + func + first + last + lwords + lspec:record | wid
  m1 <- if (m1_lpm) feols(f1, data = Y[n_c >= 2], cluster = ~wid) else feglm(f1, data = Y[n_c >= 2], family = binomial, cluster = ~wid)
  m1i <- if (m1_lpm) feols(f1i, data = Y[n_c >= 2], cluster = ~wid) else feglm(f1i, data = Y[n_c >= 2], family = binomial, cluster = ~wid)
  b1 <- coef(m1); s1 <- se(m1); bi <- coef(m1i)
  # ---- Model 2: composition without interactions (P2a, P2b) and with record interactions (P3)
  comp <- Y[, .(authored_spec = mean(maxJ < thr & specific), shared_gen = mean(maxJ >= thr & !specific), shared_spec = mean(maxJ >= thr & specific), shared_any = mean(maxJ >= thr)), by = wid]
  R <- merge(wf, comp, by = "wid")[!is.na(lrate)]
  if (dv == "lowrating") { R <- R[totalJobsWorked >= 5 & rated == TRUE]; R[, lowrating := as.integer(rating < 4.8)] }
  ctrl_dv <- switch(dv, lrate = ctrl, lowrating = sub("rating_f \\+ ", "", ctrl), lhours = paste("lrate +", sub("lhours \\+ ", "", ctrl)))
  f2a <- as.formula(paste(dv, "~ authored_spec + shared_gen + shared_spec +", ctrl_dv, "| country_f + occ_f"))
  m2a <- if (dv == "lowrating") feglm(f2a, data = R, family = binomial, cluster = ~country_f) else feols(f2a, data = R, cluster = ~country_f)
  b2 <- coef(m2a); s2 <- se(m2a)
  out <- data.table(check = label,
    m1_n = nobs(m1), m1_lspec = b1["lspec"], m1_lspec_se = s1["lspec"], m1_number = b1["number"], m1_number_se = s1["number"], m1_assurance = b1["funcassurance"],
    m1_encounter = b1["funcencounter formula"], m1_first = b1["first"], m1_last = b1["last"], m1_lspec_50plus = bi["lspec"] + bi["lspec:record50+ jobs"],
    dv = dv, m2_n = nobs(m2a), m2_authored_spec = b2["authored_spec"], m2_authored_spec_se = s2["authored_spec"], m2_shared_gen = b2["shared_gen"], m2_shared_gen_se = s2["shared_gen"],
    m2_shared_spec = b2["shared_spec"], m2_shared_spec_se = s2["shared_spec"], m2_shared_gen_0jobs = NA_real_, m2_shared_gen_50plus = NA_real_, m2_shared_gen_50plus_se = NA_real_)
  if (dv == "lrate") {
    f2b <- as.formula(paste("lrate ~ authored_spec + shared_gen + shared_spec +", int_terms, "+", ctrl, "| country_f + occ_f"))
    m2b <- feols(f2b, data = R, cluster = ~country_f); bb <- coef(m2b); V <- vcov(m2b)
    g3 <- intersect(c("d3:shared_any", "shared_any:d3"), names(bb)); g <- c("shared_gen", g3)
    out[, `:=`(m2_shared_gen_0jobs = bb["shared_gen"], m2_shared_gen_50plus = sum(bb[g]), m2_shared_gen_50plus_se = sqrt(sum(V[g, g])))]
  }
  out
}

ml_part <- function(i, a) {                    # provenance component (multilevel), cached separately
  f <- file.path(parts, "ml", sprintf("ml_%02d.csv", i))
  if (file.exists(f)) return(fread(f))
  thr <- if (is.null(a$thr)) 1 else a$thr
  wf <- W; if (!is.null(a$worker_filter)) wf <- wf[eval(a$worker_filter)]
  Y <- X[wid %in% wf$wid]; Y[, specific := spec_n > 0]
  comp <- Y[, .(authored_spec = mean(maxJ < thr & specific), shared_gen = mean(maxJ >= thr & !specific), shared_spec = mean(maxJ >= thr & specific), shared_any = mean(maxJ >= thr)), by = wid]
  R <- merge(wf, comp, by = "wid")[!is.na(lrate)]
  fam <- merge(Y[maxJ >= thr & !is.na(cid), .(wid, cid)], SA, by = "cid")[, .(n = .N, size = max(adopters)), by = .(wid, family)]
  setorder(fam, wid, -n, -size); fam <- fam[, .SD[1], by = wid][, .(wid, family = paste0("f", family))]
  R <- merge(R, fam, by = "wid", all.x = TRUE); R[, sharer := as.integer(shared_any > 0)]; R[is.na(family) | sharer == 0, family := "none"]
  fx <- paste("lrate ~ authored_spec + shared_gen + shared_spec +", int_terms, "+", ctrl, "+ country_f + occ_f")
  mm <- lmer(as.formula(paste(fx, "+ (1 | city_f) + (0 + sharer | family)")), data = R, REML = FALSE, control = lmerControl(calc.derivs = FALSE))
  vc <- as.data.table(VarCorr(mm)); vf <- vc[grp == "family", vcov]; vr <- vc[grp == "Residual", vcov]
  lr <- NA_real_
  if (nrow(R) < 150000) { m0 <- lmer(as.formula(paste(fx, "+ (1 | city_f)")), data = R, REML = FALSE, control = lmerControl(calc.derivs = FALSE)); lr <- anova(m0, mm)$Chisq[2] }
  r <- data.table(check = a$label, m2_icc_provenance = vf / (vf + vr), m2_lr_chisq = lr); fwrite(r, f); r
}

checks <- list(
  list(label = "Main specification", ml = FALSE),
  list(label = "Shared = near-copy (J >= 0.7)", thr = 0.7, ml = TRUE),
  list(label = "Long sentences only (>= 15 words)", sent_filter = quote(nwords >= 15)),
  list(label = "Excluding agency members", worker_filter = quote(agency == FALSE), ml = TRUE),
  list(label = "Stricter specificity (max word KL >= 1)", spec_rule = "kl1", m1_spec = "klmax"),
  list(label = "Never-edited profiles (text written at creation)", worker_filter = quote(text_dated == TRUE), ml = TRUE),
  list(label = "Ten largest countries", worker_filter = quote(country %in% top10)),
  list(label = "Excluding fully templated profiles (>= 90% shared)", worker_filter = quote(!(wid %in% X[, .(s = mean(maxJ >= 1)), by = wid][s >= 0.9, wid]))),
  list(label = "Linear probability model for Model 1", m1_lpm = TRUE),
  list(label = "Alternative outcome: rating < 4.8 (>= 5 jobs)", dv = "lowrating"),
  list(label = "Alternative outcome: log hours at a given rate", dv = "lhours")
)
# Namespace caches by specification AND input content: a changed control must never
# silently reuse estimates from the previous timestamp-age specification.
cache_inputs <- c("analysis/00_setup.R", "analysis/14_robustness.R",
                  file.path(DERIVED, c("sent_reuse.parquet", "sent_content.parquet", "workers.parquet", "script_attrs.parquet")))
fingerprint <- c(paste0("controls: ", ctrl), paste0("interactions: ", int_terms),
                 paste(names(tools::md5sum(cache_inputs)), tools::md5sum(cache_inputs)))
tmp <- tempfile(); writeLines(fingerprint, tmp)
cache_key <- substr(unname(tools::md5sum(tmp)), 1, 12); unlink(tmp)
parts <- file.path(OUT, "tables", "robustness_parts", paste0("experience_", cache_key))
dir.create(file.path(parts, "ml"), showWarnings = FALSE, recursive = TRUE)
writeLines(fingerprint, file.path(parts, "specification.txt"))
cat("Cache namespace:", parts, "\n")
res <- rbindlist(lapply(seq_along(checks), function(i) {
  f <- file.path(parts, sprintf("check_%02d.csv", i))
  r <- if (file.exists(f)) fread(f) else { r <- do.call(run_check, checks[[i]][setdiff(names(checks[[i]]), "ml")], quote = TRUE); fwrite(r, f); r }
  if (isTRUE(checks[[i]]$ml)) { m <- ml_part(i, checks[[i]]); r[, `:=`(m2_icc_provenance = m$m2_icc_provenance, m2_lr_chisq = m$m2_lr_chisq)] }
  else r[, `:=`(m2_icc_provenance = NA_real_, m2_lr_chisq = NA_real_)]
  r
}), fill = TRUE)
save_table(res, "robustness_estimates")

# ---- summary: does each check reproduce each proposition's signature? --------------------
sig <- function(b, s, dir) ifelse(is.na(b) | is.na(s), NA, if (dir < 0) b + 1.96 * s < 0 else b - 1.96 * s > 0)
main_icc <- as.numeric(fread(file.path(OUT, "numbers", "13_model2.csv"))[name == "m2_icc_provenance", value])
S <- res[, .(check, dv,
  P1_authorship_allocation = sig(m1_lspec, m1_lspec_se, -1) & sig(m1_number, m1_number_se, -1) & m1_assurance > 0 & m1_encounter > m1_assurance & m1_first < 0,
  P2a_authorship_premium = sig(m2_authored_spec, m2_authored_spec_se, 1),
  P2b_commons_discount_generic = sig(m2_shared_gen, m2_shared_gen_se, -1),
  P2b_shared_specific_discount = sig(m2_shared_spec, m2_shared_spec_se, -1),
  P2c_provenance = fifelse(is.na(m2_icc_provenance), NA, m2_icc_provenance),
  P2c_lr_chisq = m2_lr_chisq,
  P3_non_substitution = sig(m2_shared_gen_50plus, m2_shared_gen_50plus_se, -1) & (m2_shared_gen_50plus <= m2_shared_gen_0jobs),
  sender_relaxation = m1_lspec_50plus > m1_lspec)]
S[check == "Main specification", P2c_provenance := main_icc]
lpm_row <- S$check == "Linear probability model for Model 1"
# alternative outcomes: "discount" analogues flip sign (more low ratings); hours test "segments, not lemons"
S[dv == "lowrating", `:=`(P2a_authorship_premium = res[dv == "lowrating", sig(m2_authored_spec, m2_authored_spec_se, -1)],
                          P2b_commons_discount_generic = res[dv == "lowrating", sig(m2_shared_gen, m2_shared_gen_se, 1)],
                          P2b_shared_specific_discount = res[dv == "lowrating", sig(m2_shared_spec, m2_shared_spec_se, 1)], P3_non_substitution = NA)]
S[dv == "lhours", `:=`(P2a_authorship_premium = NA, P2b_commons_discount_generic = res[dv == "lhours", !sig(m2_shared_gen, m2_shared_gen_se, -1)],
                       P2b_shared_specific_discount = NA, P3_non_substitution = NA)]
S[dv != "lrate", `:=`(P1_authorship_allocation = NA, sender_relaxation = NA)]
save_table(S, "robustness_summary"); print(S)
rec("robustness_checks", nrow(S), "robustness checks run")
save_numbers("14_robustness")
