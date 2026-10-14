# 12_model1_authorship.R -- MODEL 1 (sender side): authorship allocation.
#
# Which sentences of a profile are drawn from the signal commons (shared verbatim with
# at least one other worker) and which are authored?  Sentence-level logit with worker
# fixed effects, so that everything about the worker (country, skill, language, experience,
# motivation) is held constant and only the sentence's content and position vary:
#
#   logit P(shared_ij = 1) = a_j + b1 log(1+specific words_ij) + b2 number_ij
#                            + b3 function_ij + b4 relpos_ij + b5 log(words_ij)
#   (relpos = 0 for the first qualifying sentence of the profile, 1 for the last)
#
# Output: tables/model1_*.csv, numbers/12_model1.csv

source("analysis/00_setup.R")
suppressPackageStartupMessages(library(fixest))
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, wid, pos_c, n_c, nwords, maxJ)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, move, spec_n, spec_max, has_number)]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[, .(wid, totalJobsWorked, agency, country)]
X  <- merge(merge(SR, C, by = "sid"), W, by = "wid")
X[, `:=`(shared = as.integer(maxJ >= 1), shared07 = as.integer(maxJ >= 0.7),
         lspec = log1p(spec_n), number = as.integer(has_number), lwords = log(nwords),
         rel = (pos_c - 1) / (n_c - 1))]   # relative position: 0 = first qualifying sentence, 1 = last (profiles with n_c >= 2)
X[, func := factor(fcase(move %in% c("identity", "credential", "capability"), "individuating claim",
                         move %in% c("character", "process"), "assurance",
                         move == "encounter", "encounter formula", default = "other"),
                   levels = c("individuating claim", "assurance", "encounter formula", "other"))]
X <- X[n_c >= 2]
rec("m1_sentences_all", nrow(X), "codable sentences in profiles with >= 2 codable sentences")

# ---- main specification -------------------------------------------------------------
f_main <- shared ~ lspec + number + func + rel + lwords | wid
m1 <- feglm(f_main, data = X, family = binomial, cluster = ~wid)
rec("m1_sentences_used", nobs(m1), "sentences in profiles with within-profile variation in sharing (identifying sample)")
rec("m1_profiles_used", length(unique(X$wid[obs(m1)])), "profiles in the identifying sample")
# linear probability version (fixed-effects LPM), same identifying variation
m1l <- feols(shared ~ lspec + number + func + rel + lwords | wid, data = X, cluster = ~wid)

tidyf <- function(m, label) { ct <- coeftable(m); data.table(model = label, term = rownames(ct), est = ct[, 1], se = ct[, 2], z = ct[, 3], odds_ratio = exp(ct[, 1])) }
T1 <- rbind(tidyf(m1, "M1 main (FE logit)"), tidyf(m1l, "M1 LPM (FE OLS)")[, odds_ratio := NA])
save_table(T1, "model1_coefficients"); print(T1, digits = 3)
b <- coef(m1)
rec("m1_or_lspec", exp(b["lspec"]), "odds ratio of being shared per log-unit of occupation-informative words (within profile)")
rec("m1_or_number", exp(b["number"]), "odds ratio, sentence contains a number (quantified claim)")
rec("m1_or_assurance", exp(b["funcassurance"]), "odds ratio, assurance vs individuating claim")
rec("m1_or_encounter", exp(b["funcencounter formula"]), "odds ratio, encounter formula vs individuating claim")
rec("m1_or_rel", exp(b["rel"]), "odds ratio, last vs first qualifying sentence of the same profile (relative position 1 vs 0)")
save_numbers("12_model1")
