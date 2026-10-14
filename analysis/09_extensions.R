# 09_extensions.R -- Robustness and extension probes for the patterns in 04-08.
#  E1 informativeness (ICC) with disjoint groups and net of city
#  E2 is the widening rate gap just wider rate dispersion? (standardised effects)
#  E3 which shared moves go with the rate gap (formulae vs assurances vs capability)?
#  E4 copying the opening sentence
#  E5 excluding agency members
#  E6 anatomy of the Kenya cross-border ties
#
# Output: tables/ext_*.csv, numbers/09_extensions.csv

source("analysis/00_setup.R")
set.seed(99)
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0]
WR <- as.data.table(read_parquet(file.path(DERIVED, "worker_reuse.parquet")))
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, wid, pos_c, n_c, maxJ, cid, script_adopters)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, macro, move, specific)]
X  <- merge(SR, C, by = "sid")
comp <- X[, .(sh_enc = mean(maxJ >= 1 & macro == "encounter formula"), sh_ass = mean(maxJ >= 1 & macro == "assurance"),
              sh_cap = mean(maxJ >= 1 & macro == "capability-bearing"), sh_oth = mean(maxJ >= 1 & macro %in% c("other", "agency voice")),
              first_shared = any(pos_c == 1 & maxJ >= 1)), by = wid]
D <- merge(merge(W, WR[, .(wid, n1, share1)], by = "wid"), comp, by = "wid")
big_c <- D[, .N, by = country][N >= 200 & !is.na(country), country]
D[, `:=`(country_f = factor(fifelse(country %in% big_c, country, "Other")), occ_f = factor(fifelse(is.na(occ1), "None", occ1)), lchars = log(desc_chars))]
D[, record := factor(fcase(totalJobsWorked == 0, "0 jobs", totalJobsWorked < 10, "1-9 jobs", totalJobsWorked < 50, "10-49 jobs", default = "50+ jobs"), levels = c("0 jobs", "1-9 jobs", "10-49 jobs", "50+ jobs"))]
D[, rating_f := factor(fcase(!rated, "unrated", rating >= 4.95, ">=4.95", rating >= 4.8, "4.8-4.95", default = "<4.8"), levels = c("unrated", ">=4.95", "4.8-4.95", "<4.8"))]
ctrl <- "country_f + occ_f + splines::ns(tenure_yrs, 5) + ljobs + lhours + rating_f + agency + n_occ + lchars + log(n_c) + text_dated"
R <- D[!is.na(lrate) & !is.na(tenure_yrs)]
pc <- function(m, t) 100 * (exp(coef(m)[t]) - 1)

# ---- E1 ICC robustness -----------------------------------------------------------------------
fr <- lm(lrate ~ country_f + occ_f + splines::ns(tenure_yrs, 5) + ljobs + lhours, data = R)
R[, res := resid(fr)]
big_city <- R[!is.na(city_key), .N, by = city_key][N >= 30, city_key]
R[, res_city := NA_real_]
R[city_key %in% big_city, res_city := res - mean(res), by = city_key]
AD <- unique(merge(SR[!is.na(cid) & script_adopters >= 20, .(cid, wid)], R[, .(wid, res, res_city)], by = "wid"), by = c("cid", "wid"))
icc <- function(g, y) { ok <- !is.na(y); g <- g[ok]; y <- y[ok]; m <- tapply(y, g, mean); n <- tapply(y, g, length); sum(n * (m - mean(y))^2) / sum((y - mean(y))^2) }
one <- AD[, .SD[sample(.N, 1)], by = wid]          # each worker in exactly one group
icc_disjoint <- icc(one$cid, one$res)
null_disjoint <- mean(replicate(50, icc(one$cid, sample(R[share1 > 0, res], nrow(one), TRUE))))
icc_city <- icc(AD$cid, AD$res_city)
null_city <- mean(replicate(50, icc(AD$cid[!is.na(AD$res_city)], sample(R[share1 > 0 & !is.na(res_city), res_city], sum(!is.na(AD$res_city)), TRUE))))
rec("E1_icc_disjoint", icc_disjoint, "between-script share of adjusted log-rate variance, each worker assigned to one random script")
rec("E1_icc_disjoint_null", null_disjoint, "... random groups")
rec("E1_icc_net_of_city", icc_city, "between-script share after also removing city means (cities >= 30 workers)")
rec("E1_icc_net_of_city_null", null_city, "... random groups")

# ---- E2 standardised rate gap by record ------------------------------------------------------
sdg <- R[, .(sd_lrate = sd(lrate), workers = .N), by = record][order(record)]
m3 <- lm(as.formula(paste("lrate ~ share1 * record +", ctrl)), data = R)
b <- coef(m3); eff <- c(b["share1"], b["share1"] + b[paste0("share1:record", levels(R$record)[-1])])
sdg[, `:=`(effect = eff, effect_sd = eff / sd_lrate)]
print(sdg); save_table(sdg, "ext_E2_standardised_gap_by_record")
for (i in seq_len(nrow(sdg))) rec(paste0("E2_effect_in_sd_", gsub("[^0-9a-z]", "_", sdg$record[i])), sdg$effect_sd[i], "rate gap (fully shared vs authored) in SDs of log rate within record group")

# ---- E3 which shared moves? ----------------------------------------------------------------
m_e3 <- lm(as.formula(paste("lrate ~ sh_enc + sh_ass + sh_cap + sh_oth +", ctrl)), data = R)
e3 <- data.table(term = c("sh_enc", "sh_ass", "sh_cap", "sh_oth"), pct = pc(m_e3, c("sh_enc", "sh_ass", "sh_cap", "sh_oth")))
print(e3); save_table(e3, "ext_E3_shared_moves")
for (i in seq_len(nrow(e3))) rec(paste0("E3_", e3$term[i]), e3$pct[i], "% rate difference per unit share of profile in shared sentences of this move")

# ---- E4 copying the opening -----------------------------------------------------------------
m_e4 <- lm(as.formula(paste("lrate ~ first_shared + share1 +", ctrl)), data = R[n_c >= 3])
rec("E4_first_sentence_shared", pc(m_e4, "first_sharedTRUE"), "% rate difference when the opening sentence is shared, holding the shared share constant")
rec("E4_first_shared_prevalence", R[n_c >= 3, mean(first_shared)], "share of profiles (>=3 sentences) whose opening sentence is shared")

# ---- E5 excluding agency members -------------------------------------------------------------
m_e5 <- lm(as.formula(paste("lrate ~ share1 +", sub("agency \\+ ", "", ctrl))), data = R[agency == FALSE])
m_e5b <- lm(as.formula(paste("lrate ~ share1 +", ctrl)), data = R)
rec("E5_share1_no_agency", pc(m_e5, "share1"), "% rate difference fully shared vs authored, excluding agency members")
rec("E5_share1_all", pc(m_e5b, "share1"), "... all workers")

# ---- E6 Kenya's cross-border ties ------------------------------------------------------------
P <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[score >= 1, .(w1, w2)]
P <- unique(P)
P[, `:=`(c1 = W$country[match(w1, W$wid)], c2 = W$country[match(w2, W$wid)], o1 = W$occ1[match(w1, W$wid)], o2 = W$occ1[match(w2, W$wid)])]
K <- P[(c1 == "Kenya") != (c2 == "Kenya") & !is.na(c1) & !is.na(c2)]
K[, `:=`(partner = fifelse(c1 == "Kenya", c2, c1), k_occ = fifelse(c1 == "Kenya", o1, o2), p_occ = fifelse(c1 == "Kenya", o2, o1))]
k1 <- K[, .N, by = partner][order(-N)][1:12]
k2 <- K[partner %in% c("Venezuela", "Spain", "Germany", "Serbia", "Italy", "South Africa"), .N, by = .(k_occ, p_occ)][order(-N)][1:10]
print(k1); print(k2); save_table(k2, "ext_E6_kenya_partner_occupations")
rec("E6_kenya_writing_share", K[partner %in% c("Venezuela", "Spain", "Germany", "Serbia", "Italy", "South Africa"), mean(k_occ == "Writing" & p_occ == "Writing", na.rm = TRUE)], "share of Kenya-(VE/ES/DE/RS/IT/ZA) sharing pairs where both list Writing first")

save_numbers("09_extensions")
