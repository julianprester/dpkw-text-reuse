# 04_anatomy.R -- The anatomy of a profile: how much of it is shared text, where in
# the profile shared sentences sit, whether they arrive in blocks, and whether a
# profile's shared sentences come from one source or are assembled from many.
#
# Output: tables/anatomy_*.csv, numbers/04_anatomy.csv

source("analysis/00_setup.R")
set.seed(44)
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, move, macro, specific, spec_n)]
S  <- as.data.table(read_parquet(file.path(DERIVED, "sentences.parquet")))[, .(sid, pos_all, n_all, offset)]
WR <- as.data.table(read_parquet(file.path(DERIVED, "worker_reuse.parquet")))
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))
X  <- merge(merge(SR, C, by = c("sid")), S, by = "sid")
X[, copied := maxJ >= 1]
setorder(X, wid, pos_c)

# ---- 1. how much of a profile is shared ------------------------------------------------
WR[, type := fcase(n1 == 0, "authored (0%)", share1 < 1/3, "lightly templated (<1/3)", share1 < 0.9, "heavily templated (1/3-0.9)", default = "fully templated (>=0.9)")]
WR[, type := factor(type, levels = c("authored (0%)", "lightly templated (<1/3)", "heavily templated (1/3-0.9)", "fully templated (>=0.9)"))]
tt <- WR[, .(profiles = .N, med_sent = med(n_c)), by = type][order(type)][, share := profiles / sum(profiles)]
print(tt); save_table(tt, "anatomy_profile_types")
for (i in seq_len(nrow(tt))) rec(paste0("type_share_", i), tt$share[i], as.character(tt$type[i]))
# conservative prevalence: shared sentences that are long and not encounter formulae
core <- X[copied & move != "encounter"]
rec("prev_shared_nonformula_12w", core[nwords >= 12, uniqueN(wid)] / nrow(WR), "share of reuse-frame workers with >=1 exact-shared sentence of >=12 words that is not an encounter formula")
rec("prev_shared_nonformula_15w", core[nwords >= 15, uniqueN(wid)] / nrow(WR), "... >=15 words")
cp <- WR[n1 > 0]
rec("copier_median_copied_sentences", med(cp$n1), "median number of exact-shared sentences per copier")
rec("copier_median_share", med(cp$share1), "median share of a copier's codable sentences that are exact-shared")
rec("copier_median_word_share", med(cp$copied_words / cp$words_c), "median share of a copier's codable words in exact-shared sentences")
h <- WR[n_c >= 5 & n1 > 0, .N, by = .(bin = cut(share1, seq(0, 1, .1), include.lowest = TRUE))][order(bin)][, share := N / sum(N)]
save_table(h, "anatomy_share_histogram_nc5"); print(h)
# who are the fully templated? (descriptive)
ft <- merge(WR[, .(wid, type)], W[, .(wid, totalJobsWorked, hourlyRate, tenure_yrs, agency, text_dated, n_c, country)], by = "wid")
ftd <- ft[, .(profiles = .N, zero_jobs = mean(totalJobsWorked == 0), med_jobs = med(totalJobsWorked), med_rate = med(hourlyRate, na.rm = TRUE),
              med_tenure = med(tenure_yrs, na.rm = TRUE), never_edited = mean(text_dated, na.rm = TRUE), agency = mean(agency, na.rm = TRUE), med_sent = med(n_c)), by = type][order(type)]
print(ftd); save_table(ftd, "anatomy_profile_types_covariates")

# ---- 2. where in the profile are shared sentences? ------------------------------------
Y <- X[n_c >= 3]
Y[, slot := fcase(pos_c == 1, "first", pos_c == n_c, "last", pos_c == 2, "second", pos_c == n_c - 1, "second-to-last", default = "middle")]
Y[, slot := factor(slot, levels = c("first", "second", "middle", "second-to-last", "last"))]
pos <- Y[, .(sentences = .N, exact = mean(copied), exact_generic = mean(copied[!specific]), exact_specific = mean(copied[specific]),
            generic_share = mean(!specific), encounter_share = mean(move == "encounter")), by = slot][order(slot)]
print(pos); save_table(pos, "anatomy_position_slots")
for (i in seq_len(nrow(pos))) rec(paste0("pos_exact_", pos$slot[i]), pos$exact[i], "exact-copy rate of sentences in this slot (profiles with >=3 codable sentences)")
Y[, rel := (pos_c - 1) / (n_c - 1)]
dec <- Y[, .(exact = mean(copied), generic = mean(!specific)), by = .(decile = pmin(floor(rel * 10) + 1, 10))][order(decile)]
save_table(dec, "anatomy_position_deciles")
# within-profile contrast: relative position of copied vs original sentences in mixed profiles
mix <- Y[, if (any(copied) && !all(copied)) .SD, by = wid]
dd <- mix[, .(rel_copied = mean(rel[copied]), rel_orig = mean(rel[!copied])), by = wid]
rec("within_profile_relpos_copied", mean(dd$rel_copied), "mean relative position (0=first,1=last) of shared sentences, mixed profiles")
rec("within_profile_relpos_original", mean(dd$rel_orig), "mean relative position of authored sentences, same profiles")
rec("within_profile_relpos_diff_share_later", mean(dd$rel_copied > dd$rel_orig), "share of mixed profiles whose shared text sits later than their authored text")
# same contrast restricted to generic sentences (is position about content or about slot?)
mg <- Y[specific == FALSE][, if (any(copied) && !all(copied)) .SD, by = wid]
dg <- mg[, .(rc = mean(rel[copied]), ro = mean(rel[!copied])), by = wid]
rec("within_profile_relpos_generic_diff", mean(dg$rc - dg$ro), "mean(relpos shared - relpos authored) among generic sentences only")
# openings: first sentence of the profile, by specificity
rec("first_sentence_specific_share", Y[slot == "first", mean(specific)], "share of opening sentences with occupation-informative words")
rec("last_sentence_encounter_share", Y[slot == "last", mean(move == "encounter")], "share of closing sentences coded as encounter formulae")

# ---- 2b. absolute offset (an interface 'fold') vs relative position (end of text) -------
# If authorship effort follows what is visible first, the copy rate should rise with the
# character offset at which a sentence starts, irrespective of how long the profile is.
Z <- merge(X, W[, .(wid, desc_chars)], by = "wid")
Z[, len_band := cut(desc_chars, c(0, 800, 1600, 2400, 7000), labels = c("<800", "800-1600", "1600-2400", ">2400"))]
Z[, off_bin := pmin(floor(offset / 100) * 100, 2000)]
fold <- Z[, .(sentences = .N, exact = mean(copied), generic = mean(!specific)), by = .(len_band, off_bin)][order(len_band, off_bin)]
save_table(fold, "anatomy_offset_by_length")
print(dcast(fold[sentences >= 500], off_bin ~ len_band, value.var = "exact"), digits = 3)
# logistic model: absolute offset vs relative position, holding content constant
Z[, rel := fifelse(n_c > 1, (pos_c - 1) / (n_c - 1), 0)]
Z[, off_k := offset / 1000]
set.seed(9); zs <- Z[sample(.N, 600000)]
m <- glm(copied ~ off_k + rel + specific + factor(move) + log(desc_chars), data = zs, family = binomial)
co <- summary(m)$coefficients
save_table(data.table(term = rownames(co), co), "anatomy_offset_logit")
rec("logit_offset_per1000chars", co["off_k", 1], "log-odds change in P(shared) per 1,000 characters of offset (holding relative position, content, length)")
rec("logit_relpos", co["rel", 1], "log-odds change from first to last relative position (holding offset, content, length)")
rec("logit_specific", co["specificTRUE", 1], "log-odds difference for sentences with occupation-informative words")

# ---- 2c. robustness: long sentences only (>= 15 words), where independent re-invention is implausible
C2 <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, spec_max)]
L <- merge(X[nwords >= 15], C2, by = "sid")
lg <- L[, .(sentences = .N, exact = mean(copied)), by = .(kl_bin = cut(spec_max, c(-1, .25, .5, 1, 1.5, 2, 10)))][order(kl_bin)]
save_table(lg, "anatomy_long_sentences_specificity"); print(lg)
rec("long_exact_most_generic", lg[1, exact], "exact-copy rate, sentences >= 15 words, max token KL <= 0.25")
rec("long_exact_most_specific", lg[.N, exact], "exact-copy rate, sentences >= 15 words, max token KL > 2")
ag <- merge(X, C2, by = "sid")[, .(sentences = .N, exact = mean(copied)), by = .(kl_bin = cut(spec_max, c(-1, .25, .5, 1, 1.5, 2, 10)))][order(kl_bin)]
save_table(ag, "anatomy_specificity_gradient"); print(ag)
rec("exact_most_generic", ag[1, exact], "exact-copy rate, all sentences, max token KL <= 0.25")
for (i in 2:nrow(ag)) rec(paste0("exact_kl_bin_", i), ag[i, exact], paste("exact-copy rate, all sentences, max token KL in", ag[i, kl_bin]))
lm_ <- L[, .(sentences = .N, exact = mean(copied)), by = move][order(-exact)]
save_table(lm_, "anatomy_long_sentences_moves")
for (i in seq_len(nrow(lm_))) rec(paste0("long_exact_move_", lm_$move[i]), lm_$exact[i], "exact-copy rate by move, sentences >= 15 words")
lp <- L[n_c >= 3][, slot := fcase(pos_c == 1, "first", pos_c == n_c, "last", default = "middle")][, .(sentences = .N, exact = mean(copied)), by = slot]
save_table(lp, "anatomy_long_sentences_position")
for (i in seq_len(nrow(lp))) rec(paste0("long_exact_", lp$slot[i]), lp$exact[i], "exact-copy rate by slot, sentences >= 15 words")

# ---- 3. do shared sentences arrive in blocks? (runs test vs random placement) ----------
rt <- X[, .(n = .N, k = sum(copied), runs = { r <- rle(copied); sum(r$values) }), by = wid][k >= 1 & k < n]
rt[, `:=`(exp_runs = k * (n - k + 1) / n, p_single = (n - k + 1) / choose(n, k))]
rec("runs_profiles", nrow(rt), "mixed profiles (>=1 shared and >=1 authored codable sentence)")
rec("runs_obs_over_exp", sum(rt$runs) / sum(rt$exp_runs), "observed/expected number of runs of shared sentences (random placement = 1)")
rec("single_block_obs", rt[k >= 2, mean(runs == 1)], "share of profiles (k>=2 shared) whose shared sentences form one contiguous block")
rec("single_block_exp", rt[k >= 2, mean(p_single)], "... expected under random placement")
bl <- rt[k >= 2, .(profiles = .N, single_block_obs = mean(runs == 1), single_block_exp = mean(p_single)), by = .(k = pmin(k, 6))][order(k)]
print(bl); save_table(bl, "anatomy_blocks_by_k")

# ---- 4. cloned or assembled? source structure of each copier's shared sentences --------
P <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[score >= 1, .(sa, sb, wa, wb)]
L <- unique(rbind(P[, .(w = wa, v = wb, s = sa)], P[, .(w = wb, v = wa, s = sb)]))
cov <- L[, .(shared_with_v = uniqueN(s)), by = .(w, v)]
cov <- cov[, .(best = max(shared_with_v), partners = .N), by = w]
cov <- merge(cov, WR[, .(w = wid, n1, n_c, n_scripts)], by = "w")
cov[, coverage := best / n1]
m2 <- cov[n1 >= 2]
rec("assembly_copiers_k2", nrow(m2), "copiers with >=2 exact-shared sentences")
rec("assembly_single_source_share", m2[, mean(coverage == 1)], "... whose shared sentences are all shared with one single other profile")
rec("assembly_multi_source_share", m2[, mean(coverage <= 0.5)], "... for whom no single other profile covers more than half of their shared sentences")
asb <- m2[, .(copiers = .N, single_source = mean(coverage == 1), multi_source = mean(coverage <= .5), med_partners = med(partners)), by = .(k = pmin(n1, 8))][order(k)]
print(asb); save_table(asb, "anatomy_assembly_by_k")
write_parquet(cov[, .(wid = w, best_partner_cover = coverage, partners, n1)], file.path(DERIVED, "worker_assembly.parquet"))
save_numbers("04_anatomy")
