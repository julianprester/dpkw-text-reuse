# 02_pairs.R -- Link scored sentence pairs to workers, validate the id mapping,
# build exact-match script clusters and worker-level reuse measures.
#
# Output: data/derived/xpairs.parquet   cross-worker sentence pairs
#         data/derived/sent_reuse.parquet  sentence-level reuse (maxJ, script id, script size)
#         data/derived/scripts.parquet     exact-match clusters ("scripts")
#         data/derived/worker_reuse.parquet

source("analysis/00_setup.R")
suppressPackageStartupMessages(library(textreuse))
set.seed(2023)
S <- as.data.table(read_parquet(file.path(DERIVED, "sentences.parquet")))
W <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))
setkey(S, sid)

P <- fread("data/similar_pairs.csv")
P[, `:=`(sa = as.integer(sub("sent_", "", a)), sb = as.integer(sub("sent_", "", b)))][, c("a", "b") := NULL]
rec("pairs_scored", nrow(P), "candidate sentence pairs returned by MinHash/LSH and scored with exact Jaccard")

# ---- 1. validate: recompute Jaccard on a random sample of pairs --------------------
idx <- sample(nrow(P), 3000)
shingle <- function(x) unique(textreuse::tokenize_ngrams(x, n = 5))
recomp <- vapply(idx, function(i) { A <- shingle(S[J(P$sa[i]), text]); B <- shingle(S[J(P$sb[i]), text]); length(intersect(A, B)) / length(union(A, B)) }, 0)
rec("validation_pairs", length(idx), "randomly sampled pairs re-scored from the rebuilt corpus")
rec("validation_agreement", mean(abs(recomp - P$score[idx]) < 1e-9), "share of re-scored pairs whose Jaccard equals the stored score")
stopifnot(mean(abs(recomp - P$score[idx]) < 1e-9) == 1)

# ---- 2. cross-worker pairs ----------------------------------------------------------
P[, `:=`(wa = S$wid[match(sa, S$sid)], wb = S$wid[match(sb, S$sid)])]
rec("pairs_same_worker", P[wa == wb, .N], "pairs of two sentences from the same profile (dropped)")
P <- P[wa != wb]
P[, `:=`(w1 = pmin(wa, wb), w2 = pmax(wa, wb))]
for (t in c(1, .9, .7, .5, .3)) rec(sprintf("pairs_cross_J%.1f", t), P[score >= t, .N], sprintf("cross-worker sentence pairs with J >= %.1f", t))
write_parquet(P, file.path(DERIVED, "xpairs.parquet"))

# LSH detection probability with 240 permutations in 60 bands (4 rows per band)
lsh <- data.table(J = c(.2, .3, .4, .5, .6, .7))[, p_detect := 1 - (1 - J^4)^60]
save_table(lsh, "lsh_detection_probability")
for (i in seq_len(nrow(lsh))) rec(sprintf("lsh_pdetect_J%.1f", lsh$J[i]), lsh$p_detect[i], "P(pair becomes an LSH candidate | Jaccard)")

# ---- 3. sentence-level reuse ---------------------------------------------------------
long <- rbind(P[, .(sid = sa, other = wb, score)], P[, .(sid = sb, other = wa, score)])
SR <- long[, .(maxJ = max(score), partners_exact = uniqueN(other[score >= 1]), partners_07 = uniqueN(other[score >= .7])), by = sid]
SR <- merge(S[, .(sid, wid, pos_c, n_c, nwords)], SR, by = "sid", all.x = TRUE)
SR[is.na(maxJ), `:=`(maxJ = 0, partners_exact = 0L, partners_07 = 0L)]

# ---- 4. exact-match clusters ("scripts") ---------------------------------------------
E1 <- P[score >= 1]
lab <- components_dt(E1$sa, E1$sb)
SR <- merge(SR, lab[, .(sid = node, cid = l)], by = "sid", all.x = TRUE)
SC <- SR[!is.na(cid), .(adopters = uniqueN(wid), instances = .N, rep_sid = min(sid)), by = cid]
SC <- merge(SC, S[, .(rep_sid = sid, text)], by = "rep_sid")
SC[, words := str_count(text, "\\S+")]
SR <- merge(SR, SC[, .(cid, script_adopters = adopters)], by = "cid", all.x = TRUE)
SR[is.na(script_adopters), script_adopters := 1L]
setorder(SR, sid)
write_parquet(SR, file.path(DERIVED, "sent_reuse.parquet"))
setorder(SC, -adopters)
write_parquet(SC, file.path(DERIVED, "scripts.parquet"))
rec("scripts", nrow(SC), "exact-match clusters (sets of identical sentences held by >=2 workers)")
rec("script_instances", sum(SC$instances), "sentences that belong to an exact-match cluster")
size <- SC[, .(scripts = .N, adoptions = sum(adopters)), by = .(size = cut(adopters, c(1, 2, 5, 20, 100, Inf), labels = c("2", "3-5", "6-20", "21-100", ">100")))]
size[, `:=`(script_share = scripts / sum(scripts), adoption_share = adoptions / sum(adoptions))]
save_table(size[order(size)], "script_size_distribution")
print(size[order(size)])
rec("scripts_dyadic_share", size[size == "2", script_share], "share of scripts held by exactly 2 workers")
rec("adoptions_dyadic_share", size[size == "2", adoption_share], "share of script adoptions in dyadic scripts")
rec("scripts_over100", SC[adopters > 100, .N], "scripts with more than 100 adopters")
rec("script_max_adopters", max(SC$adopters), "adopters of the most widely shared script")
# pair-weighted vs adoption-weighted concentration of the head
E1[, cid := SR$cid[match(sa, SR$sid)]]
pp <- E1[, .N, by = cid]
top100 <- SC$cid[1:100]
rec("top100_share_pairs", pp[cid %in% top100, sum(N)] / sum(pp$N), "share of exact-match sentence pairs within the 100 most-adopted scripts (pair-weighted)")
rec("top100_share_adoptions", SC[1:100, sum(adopters)] / SC[, sum(adopters)], "share of script adoptions in the 100 most-adopted scripts (adoption-weighted)")
save_table(SC[1:300, .(cid, adopters, words, text)], "scripts_top300")

# ---- 5. worker-level reuse measures ---------------------------------------------------
WR <- SR[, .(n_c = .N, n1 = sum(maxJ >= 1), n09 = sum(maxJ >= .9), n07 = sum(maxJ >= .7), n05 = sum(maxJ >= .5), n03 = sum(maxJ >= .3),
             maxJ = max(maxJ), n_scripts = uniqueN(cid[!is.na(cid)]),
             copied_words = sum(nwords[maxJ >= 1]), words_c = sum(nwords)), by = wid]
WR[, `:=`(share1 = n1 / n_c, share07 = n07 / n_c)]
write_parquet(WR, file.path(DERIVED, "worker_reuse.parquet"))
N <- nrow(WR)
for (t in c("1", "09", "07", "05", "03")) {
  v <- WR[[paste0("n", t)]] > 0
  rec(paste0("workers_involved_J", t), sum(v), paste0("workers with >=1 sentence in a cross-worker pair at J >= ", ifelse(t == "1", "1.0", paste0("0.", substr(t, 2, 2)))))
  rec(paste0("workers_involved_share_J", t), mean(v), "share of reuse-frame workers")
}
rec("reuse_frame_workers", N, "workers with >=1 codable sentence")
cat("\nPrevalence:\n"); print(WR[, .(exact = mean(n1 > 0), near07 = mean(n07 > 0), J05 = mean(n05 > 0), J03 = mean(n03 > 0))])
save_numbers("02_pairs")
