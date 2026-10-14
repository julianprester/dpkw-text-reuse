# 08_signaling.R -- Worker-level patterns through a signaling lens. All models are
# descriptive associations (cross-section), with country-clustered standard errors.
#  M1 who carries shared text?
#  M2 dose-response: asking rate by how much of the profile is shared
#  M3 signal substitution: does the rate gap shrink once a verified record exists?
#  M4 which text: shared vs authored x generic vs occupation-specific
#  M5 commons: widely circulating vs rare scripts
#  M6 ex-post quality (ratings) and M7 access (first job)
#  M8 informativeness: do adopters of the same script resemble each other?
#
# Output: tables/sig_*.csv, numbers/08_signaling.csv

source("analysis/00_setup.R")
set.seed(88)
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0]
WR <- as.data.table(read_parquet(file.path(DERIVED, "worker_reuse.parquet")))
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, wid, maxJ, cid, script_adopters)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, specific)]
AS <- as.data.table(read_parquet(file.path(DERIVED, "worker_assembly.parquet")))
X  <- merge(SR, C, by = "sid")
comp <- X[, .(sh_gen = mean(maxJ >= 1 & !specific), sh_spec = mean(maxJ >= 1 & specific), or_spec = mean(maxJ < 1 & specific),
              sh_pop = mean(maxJ >= 1 & script_adopters >= 20), sh_mid = mean(maxJ >= 1 & script_adopters %between% c(6, 19)), sh_rare = mean(maxJ >= 1 & script_adopters <= 5)), by = wid]
D <- merge(merge(W, WR[, .(wid, n1, n07, share1, share07)], by = "wid"), comp, by = "wid")
D <- merge(D, AS[, .(wid, best_partner_cover)], by = "wid", all.x = TRUE)
big_c <- D[, .N, by = country][N >= 200 & !is.na(country), country]
D[, `:=`(country_f = factor(fifelse(country %in% big_c, country, "Other")), occ_f = factor(fifelse(is.na(occ1), "None", occ1)))]
D[, type := factor(fcase(n1 == 0, "authored", share1 < 1/3, "light", share1 < 0.9, "heavy", default = "full"), levels = c("authored", "light", "heavy", "full"))]
D[, record := factor(fcase(totalJobsWorked == 0, "0 jobs", totalJobsWorked < 10, "1-9 jobs", totalJobsWorked < 50, "10-49 jobs", default = "50+ jobs"), levels = c("0 jobs", "1-9 jobs", "10-49 jobs", "50+ jobs"))]
D[, rating_f := factor(fcase(!rated, "unrated", rating >= 4.95, ">=4.95", rating >= 4.8, "4.8-4.95", default = "<4.8"), levels = c("unrated", ">=4.95", "4.8-4.95", "<4.8"))]
D[, `:=`(lchars = log(desc_chars), any1 = n1 > 0)]
rec("model_frame_workers", nrow(D), "reuse-frame workers in the model frame")

# cluster-robust (country) variance for lm/glm
vc_cl <- function(m, cl) {
  X <- model.matrix(m); u <- if (inherits(m, "glm")) residuals(m, "working") * m$weights else residuals(m)
  keep <- !is.na(coef(m)); X <- X[, keep, drop = FALSE]
  bread <- if (inherits(m, "glm")) summary(m)$cov.unscaled else chol2inv(qr.R(m$qr))[seq_len(sum(keep)), seq_len(sum(keep))]
  cl <- if (length(cl) == nrow(X)) cl else cl[as.integer(rownames(X))]
  stopifnot(!anyNA(cl))
  meat <- crossprod(rowsum(X * u, cl))
  G <- length(unique(cl)); n <- nrow(X); k <- ncol(X)
  V <- bread %*% meat %*% bread * (G / (G - 1)) * ((n - 1) / (n - k))
  dimnames(V) <- list(colnames(X), colnames(X)); V
}
tidy_cl <- function(m, terms, cl, label) {
  V <- vc_cl(m, cl); b <- coef(m)[terms]; se <- sqrt(diag(V)[terms])
  data.table(model = label, term = terms, est = b, se = se, pct = 100 * (exp(b) - 1), n = nobs(m))
}
ctrl <- "country_f + occ_f + splines::ns(tenure_yrs, 5) + ljobs + lhours + rating_f + agency + n_occ + lchars + log(n_c) + text_dated"
R <- D[!is.na(lrate) & !is.na(tenure_yrs)]
R[, row := .I]; cl <- as.character(R$country_f)
setDF(R); rownames(R) <- NULL

# ---- M0 how much of the raw rate gap is country composition? ------------------------------
m0a <- lm(lrate ~ any1, data = R); m0b <- lm(lrate ~ any1 + country_f, data = R)
rec("M0_any_shared_raw", 100 * (exp(coef(m0a)["any1TRUE"]) - 1), "% raw difference in asking rate, any shared sentence vs none")
rec("M0_any_shared_country_fe", 100 * (exp(coef(m0b)["any1TRUE"]) - 1), "... with country fixed effects only")
m0c <- lm(as.formula(paste("lrate ~ any1 +", ctrl)), data = R)
rec("M0_any_shared_full", 100 * (exp(coef(m0c)["any1TRUE"]) - 1), "... with full controls")

# ---- M1 who carries shared text? ------------------------------------------------------
m1 <- glm(as.formula(paste("any1 ~", sub("rating_f \\+ ", "rating_f + ", ctrl))), data = R, family = binomial)
t1 <- tidy_cl(m1, c("ljobs", "lhours", "agencyTRUE", "n_occ", "lchars", "log(n_c)", "text_datedTRUE", "rating_f<4.8"), cl, "M1 logit any shared sentence")
t1[, odds_ratio := exp(est)]; print(t1); save_table(t1, "sig_M1_who_copies")
cty <- setDT(R)[, .(workers = .N, any_shared = mean(any1), med_rate = med(exp(lrate))), by = country][workers >= 2000][order(-any_shared)]
save_table(cty, "sig_country_prevalence"); setDF(R)

# ---- M2 dose-response ------------------------------------------------------------------
m2 <- lm(as.formula(paste("lrate ~ type +", ctrl)), data = R)
t2 <- tidy_cl(m2, c("typelight", "typeheavy", "typefull"), cl, "M2 log rate ~ profile type"); print(t2)
for (i in seq_len(nrow(t2))) rec(paste0("M2_", t2$term[i]), t2$pct[i], "% difference in asking rate vs fully authored profiles (controls)")
R$share_bin <- factor(cut(R$share1, c(-Inf, 0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = c("0", "(0,.1]", "(.1,.2]", "(.2,.3]", "(.3,.4]", "(.4,.5]", "(.5,.6]", "(.6,.7]", "(.7,.8]", "(.8,.9]", "(.9,1]")))
m2c <- lm(as.formula(paste("lrate ~ share_bin +", ctrl)), data = R)
terms2 <- grep("^share_bin", names(coef(m2c)), value = TRUE)
t2c <- tidy_cl(m2c, terms2, cl, "M2 dose-response bins")
t2c[, bin := sub("share_bin", "", term)][, workers := as.integer(table(R$share_bin)[bin])]
print(t2c); save_table(t2c, "sig_M2_dose_response_bins"); save_table(t2, "sig_M2_types")

# ---- M3 signal substitution: rate gap by verified record -----------------------------------
m3 <- lm(as.formula(paste("lrate ~ share1 * record +", ctrl)), data = R)
V3 <- vc_cl(m3, cl); b3 <- coef(m3)
lev <- levels(R$record); rows <- list()
for (l in lev) {
  g <- c("share1", if (l != lev[1]) paste0("share1:record", l)); w <- setNames(rep(1, length(g)), g)
  est <- sum(b3[g]); se <- sqrt(as.numeric(t(w) %*% V3[g, g, drop = FALSE] %*% w))
  rows[[l]] <- data.table(record = l, share1_effect = est, se = se, pct_full_vs_authored = 100 * (exp(est) - 1), workers = sum(R$record == l))
}
t3 <- rbindlist(rows); print(t3); save_table(t3, "sig_M3_substitution_by_record")
for (i in seq_len(nrow(t3))) rec(paste0("M3_share1_", gsub("[^0-9a-z]", "_", t3$record[i])), t3$pct_full_vs_authored[i], "% rate difference, fully shared vs fully authored profile, within record group")
m3r <- lm(as.formula(paste("lrate ~ share1 * rating_f +", sub("rating_f \\+ ", "", ctrl))), data = R)
b <- coef(m3r); V <- vc_cl(m3r, cl)
t3r <- rbindlist(lapply(levels(R$rating_f), function(l) { g <- c("share1", if (l != "unrated") paste0("share1:rating_f", l)); w <- rep(1, length(g)); data.table(rating = l, est = sum(b[g]), se = sqrt(as.numeric(t(w) %*% V[g, g, drop = FALSE] %*% w))) }))
t3r[, pct := 100 * (exp(est) - 1)]; print(t3r); save_table(t3r, "sig_M3_substitution_by_rating")

# ---- M4 which text? composition of the profile ----------------------------------------------
m4 <- lm(as.formula(paste("lrate ~ sh_gen + sh_spec + or_spec +", ctrl)), data = R)
t4 <- tidy_cl(m4, c("sh_gen", "sh_spec", "or_spec"), cl, "M4 log rate ~ composition (ref: authored generic)"); print(t4); save_table(t4, "sig_M4_composition")
for (i in seq_len(nrow(t4))) rec(paste0("M4_", t4$term[i]), t4$pct[i], "% rate difference for a profile made entirely of this kind of sentence vs entirely authored-generic")

# ---- M5 commons: popular vs rare scripts -------------------------------------------------
m5 <- lm(as.formula(paste("lrate ~ sh_pop + sh_mid + sh_rare +", ctrl)), data = R)
t5 <- tidy_cl(m5, c("sh_pop", "sh_mid", "sh_rare"), cl, "M5 log rate ~ shared share by script popularity"); print(t5); save_table(t5, "sig_M5_popularity")
for (i in seq_len(nrow(t5))) rec(paste0("M5_", t5$term[i]), t5$pct[i], "% rate difference per unit share of profile in shared sentences of this popularity")

# ---- M6 ex-post quality: ratings among workers with >= 5 jobs -------------------------------
Q <- setDT(copy(R))[totalJobsWorked >= 5 & rated == TRUE]; Q[, low := rating < 4.8]; clq <- as.character(Q$country_f); setDF(Q); rownames(Q) <- NULL
ctrl_q <- "country_f + occ_f + splines::ns(tenure_yrs, 5) + ljobs + lhours + agency + n_occ + lchars + log(n_c) + text_dated + lrate"
m6 <- glm(as.formula(paste("low ~ share1 +", ctrl_q)), data = Q, family = binomial)
t6 <- tidy_cl(m6, "share1", clq, "M6 logit rating < 4.8 (jobs >= 5)"); t6[, odds_ratio := exp(est)]; print(t6); save_table(t6, "sig_M6_ratings")
rec("M6_or_low_rating_share1", t6$odds_ratio, "odds ratio of a rating < 4.8 for a fully shared vs fully authored profile (>= 5 jobs)")

# ---- M7 access: first job among workers in their first year -------------------------------
A <- setDT(copy(D))[tenure_yrs < 1 & !is.na(tenure_yrs)]; A[, anyjob := totalJobsWorked > 0]; cla <- as.character(A$country_f); setDF(A); rownames(A) <- NULL
m7 <- glm(anyjob ~ share1 + country_f + occ_f + tenure_yrs + agency + n_occ + lchars + log(n_c) + text_dated, data = A, family = binomial)
t7 <- tidy_cl(m7, "share1", cla, "M7 logit >=1 job (tenure < 1 yr)"); t7[, odds_ratio := exp(est)]; print(t7); save_table(t7, "sig_M7_access")
rec("M7_or_first_job_share1", t7$odds_ratio, "odds ratio of having >=1 job, fully shared vs fully authored, first-year workers")
m7b <- lm(lrate ~ share1 + country_f + occ_f + tenure_yrs + agency + n_occ + lchars + log(n_c) + text_dated + ljobs, data = A[!is.na(A$lrate), ])
t7b <- tidy_cl(m7b, "share1", cla, "M7b log rate, first-year workers"); print(t7b)
rec("M7b_rate_share1_newcomers", t7b$pct, "% rate difference, fully shared vs authored, first-year workers")

# ---- M8 informativeness of scripts: do co-adopters resemble each other? -------------------
setDT(R)
fr <- lm(lrate ~ country_f + occ_f + splines::ns(tenure_yrs, 5) + ljobs + lhours, data = R)
R[, res := resid(fr)]
AD <- unique(merge(SR[!is.na(cid) & script_adopters >= 20, .(cid, wid)], R[, .(wid, res, country_f)], by = "wid"), by = c("cid", "wid"))
icc <- function(d) { v_tot <- var(d$res); m <- d[, .(mu = mean(res), n = .N), by = g]; v_b <- sum(m$n * (m$mu - mean(d$res))^2) / nrow(d); v_b / v_tot }
obs <- icc(AD[, .(g = cid, res)])
cop <- R[share1 > 0, res]
null <- replicate(50, { x <- copy(AD); x[, res := sample(cop, .N, TRUE)]; icc(x[, .(g = cid, res)]) })
rec("M8_icc_scripts_obs", obs, "share of variance in adjusted log rate between scripts (adopters of scripts with >=20 adopters)")
rec("M8_icc_scripts_null", mean(null), "same statistic for random groups of copiers of identical sizes")
# within-country version (does a script sort workers beyond the country it circulates in?)
R[, res_c := res - mean(res), by = country_f]
AD2 <- merge(AD[, .(cid, wid)], R[, .(wid, res = res_c)], by = "wid")
obs2 <- icc(AD2[, .(g = cid, res)])
null2 <- replicate(50, { x <- copy(AD2); x[, res := sample(R[share1 > 0, res_c], .N, TRUE)]; icc(x[, .(g = cid, res)]) })
rec("M8_icc_scripts_within_country_obs", obs2, "between-script variance share after removing country means")
rec("M8_icc_scripts_within_country_null", mean(null2), "... random groups")
# scripts whose adopters are unusually high- or low-priced
sc <- merge(AD2[, .(adj = mean(res), n = .N), by = cid], as.data.table(read_parquet(file.path(DERIVED, "scripts.parquet")))[, .(cid, text)], by = "cid")
save_table(sc[order(adj)][c(1:15, (.N - 14):.N)], "sig_M8_extreme_scripts")

# ---- M9 cloning vs assembling --------------------------------------------------------------
setDF(R)
m9 <- lm(as.formula(paste("lrate ~ I(best_partner_cover == 1) + share1 +", ctrl)), data = R[R$n1 >= 2, ])
t9 <- tidy_cl(m9, "I(best_partner_cover == 1)TRUE", cl, "M9 single-source (cloned) vs assembled, copiers with >=2 shared"); print(t9); save_table(t9, "sig_M9_cloning")
rec("M9_cloned_vs_assembled", t9$pct, "% rate difference, single-source vs multi-source copiers (same shared share)")
all <- rbindlist(list(t1[, 1:7], t2, t4, t5, t6[, 1:7], t7[, 1:7], t7b, t9), fill = TRUE)
save_table(all, "sig_all_models")
save_numbers("08_signaling")
