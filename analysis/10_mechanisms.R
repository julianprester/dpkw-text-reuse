# 10_mechanisms.R -- Probes that discriminate between readings of the rate gap.
#  K1 does the gap widen with time on the platform (tenure), not only with jobs?
#  K2 at a given price, do templated workers do more and smaller jobs (commodity segment)?
#  K3 is the gap largest where the profile text is itself a work sample (Writing, Translation)?
#
# Output: tables/mech_*.csv, numbers/10_mechanisms.csv

source("analysis/00_setup.R")
set.seed(1010)
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0]
WR <- as.data.table(read_parquet(file.path(DERIVED, "worker_reuse.parquet")))
D  <- merge(W, WR[, .(wid, n1, share1)], by = "wid")
big_c <- D[, .N, by = country][N >= 200 & !is.na(country), country]
D[, `:=`(country_f = factor(fifelse(country %in% big_c, country, "Other")), occ_f = factor(fifelse(is.na(occ1), "None", occ1)), lchars = log(desc_chars))]
D[, rating_f := factor(fcase(!rated, "unrated", rating >= 4.95, ">=4.95", rating >= 4.8, "4.8-4.95", default = "<4.8"), levels = c("unrated", ">=4.95", "4.8-4.95", "<4.8"))]
D[, ten_f := cut(tenure_yrs, c(-Inf, 1, 2, 4, 7, Inf), labels = c("<1 yr", "1-2 yrs", "2-4 yrs", "4-7 yrs", "7+ yrs"))]
R <- D[!is.na(lrate) & !is.na(tenure_yrs)]
pc <- function(b) 100 * (exp(b) - 1)

# ---- K1 gap by tenure band -----------------------------------------------------------------
m1 <- lm(lrate ~ share1 * ten_f + country_f + occ_f + ten_f + ljobs + lhours + rating_f + agency + n_occ + lchars + log(n_c) + text_dated, data = R)
b <- coef(m1); lv <- levels(R$ten_f)
k1 <- data.table(tenure = lv, pct = pc(c(b["share1"], b["share1"] + b[paste0("share1:ten_f", lv[-1])])), workers = as.integer(table(R$ten_f)))
k1[, sd_lrate := R[, sd(lrate), by = ten_f][match(lv, ten_f), V1]]
print(k1); save_table(k1, "mech_K1_gap_by_tenure")
for (i in seq_len(nrow(k1))) rec(paste0("K1_gap_", gsub("[^0-9a-z]", "_", k1$tenure[i])), k1$pct[i], "% rate difference fully shared vs authored, within tenure band")
# same without conditioning on jobs/hours (total association over careers)
m1b <- lm(lrate ~ share1 * ten_f + country_f + occ_f + ten_f + agency + n_occ + lchars + log(n_c) + text_dated, data = R)
b <- coef(m1b)
k1b <- data.table(tenure = lv, pct = pc(c(b["share1"], b["share1"] + b[paste0("share1:ten_f", lv[-1])])))
print(k1b); save_table(k1b, "mech_K1b_gap_by_tenure_no_record_controls")

# ---- K2 volume and gig size at a given price ---------------------------------------------------
m2 <- lm(ljobs ~ share1 + lrate + splines::ns(tenure_yrs, 5) + country_f + occ_f + agency + n_occ + lchars + log(n_c) + text_dated, data = R)
rec("K2_jobs_share1", pc(coef(m2)["share1"]), "% difference in (1+jobs), fully shared vs authored, same asking rate and tenure")
H <- R[totalJobsWorked >= 3 & totalHours > 0]
m2b <- lm(log(hours_per_job) ~ share1 + lrate + splines::ns(tenure_yrs, 5) + ljobs + country_f + occ_f + agency + n_occ + lchars + log(n_c) + text_dated, data = H)
rec("K2_hours_per_job_share1", pc(coef(m2b)["share1"]), "% difference in hours per job, fully shared vs authored (>=3 jobs), same rate, tenure and job count")
m2c <- lm(lhours ~ share1 + lrate + splines::ns(tenure_yrs, 5) + country_f + occ_f + agency + n_occ + lchars + log(n_c) + text_dated, data = R)
rec("K2_hours_share1", pc(coef(m2c)["share1"]), "% difference in (1+hours), fully shared vs authored, same rate and tenure")

# ---- K3 text as a work sample: gap by occupation ----------------------------------------------
m3 <- lm(lrate ~ share1 * occ_f + country_f + splines::ns(tenure_yrs, 5) + ljobs + lhours + rating_f + agency + n_occ + lchars + log(n_c) + text_dated, data = R)
b <- coef(m3); ol <- levels(R$occ_f)
k3 <- data.table(occupation = ol, pct = pc(c(b["share1"], b["share1"] + b[paste0("share1:occ_f", ol[-1])])), workers = as.integer(table(R$occ_f)[ol]))
k3 <- k3[workers >= 1000][order(pct)]
print(k3); save_table(k3, "mech_K3_gap_by_occupation")
for (o in c("Writing", "Translation", "Admin Support", "Web, Mobile & Software Dev", "Design & Creative")) rec(paste0("K3_gap_", gsub("[^A-Za-z]", "_", o)), k3[occupation == o, pct], paste("% rate gap, fully shared vs authored:", o))

save_numbers("10_mechanisms")
