# 20_occupation_sensitivity.R -- Does the occupational homophily of sharing depend on using only the first-listed occupation?
#
# The main analysis (07_circulation.R) compares workers' first-listed occupation (occ1). Workers can list
# several categories. Here each worker is a SET of categories and a worker pair is scored by overlap:
#   first   = same first-listed category (the main-analysis measure)
#   any     = at least one category in common
#   jaccard = |A & B| / |A | B|  (graded overlap)
#   nshared = |A & B|
# Each is compared with random mixing of the same endpoints (pooled endpoint sets, as in `homophily()`):
# lift = observed mean / expected mean under random mixing; diff = observed - expected.
# Descriptive sensitivity only: no model in the manuscript is refitted, and existing outputs are not changed.
#
# Output: tables/occsens_*.csv, numbers/20_occupation_sensitivity.csv

source("analysis/00_setup.R")
set.seed(2020)
N_NULL <- 4e6

P <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[score >= 0.3]
W <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[, .(wid, occ1, occ_all, n_occ, agency)]
C <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, specific)]
setkey(W, wid)

# ---- category sets as bit masks -----------------------------------------------------------
sets <- strsplit(W$occ_all, "|", fixed = TRUE)
cats <- sort(unique(unlist(sets)))
stopifnot(length(cats) <= 16)
bit  <- setNames(2L^(seq_along(cats) - 1L), cats)
W[, mask := vapply(sets, function(s) as.integer(sum(bit[s])), 0L)]
W[mask == 0L, mask := NA_integer_]                  # no listed category: excluded, as with occ1 == NA
pop  <- vapply(0:(2^length(cats) - 1L), function(x) sum(bitwAnd(x, 2L^(0:(length(cats) - 1L))) > 0L), 0L)
popc <- function(x) pop[x + 1L]
rec("categories", length(cats), "distinct listed categories")
rec("workers_no_category", W[is.na(mask), .N], "workers with no listed category (excluded from set measures)")
rec("share_multi_category", W[!is.na(mask), mean(n_occ > 1)], "share of workers with listed categories who list more than one")

# overlap measures for a vector of mask pairs
score <- function(a, b) {
  i <- bitwAnd(a, b); u <- bitwOr(a, b)
  list(any = i > 0L, jaccard = popc(i) / popc(u), nshared = popc(i))
}

# ---- worker pairs, as in 07_circulation.R -------------------------------------------------
P[, spec := C$specific[match(sa, C$sid)]]
WP <- P[, .(maxJ = max(score), exact_generic = any(score >= 1 & !spec), exact_specific = any(score >= 1 & spec)), by = .(w1, w2)]
i1 <- match(WP$w1, W$wid); i2 <- match(WP$w2, W$wid)
WP[, `:=`(m1 = W$mask[i1], m2 = W$mask[i2], f1 = W$occ1[i1], f2 = W$occ1[i2], ag1 = W$agency[i1], ag2 = W$agency[i2],
          n1 = W$n_occ[i1], n2 = W$n_occ[i2])]
WP[, band := cut(maxJ, c(0.3, 0.5, 0.7, 0.9, 1, 1.01), right = FALSE, labels = c("[0.3,0.5)", "[0.5,0.7)", "[0.7,0.9)", "[0.9,1)", "exact"))]
WP <- WP[!is.na(m1) & !is.na(m2)]

# expected values under random mixing of the same endpoints: draw pairs from the pooled endpoint sets
# `sided = TRUE` keeps the two sides separate (needed when the sides are not exchangeable, see (c)).
null_for <- function(d, sided = FALSE) {
  if (sided) {
    j <- sample.int(nrow(d), N_NULL, TRUE); k <- sample.int(nrow(d), N_NULL, TRUE)
    m1 <- d$m1[j]; m2 <- d$m2[k]; same <- d$f1[j] == d$f2[k]
  } else {
    pool_m <- c(d$m1, d$m2); pool_f <- c(d$f1, d$f2)
    j <- sample.int(length(pool_m), N_NULL, TRUE); k <- sample.int(length(pool_m), N_NULL, TRUE)
    m1 <- pool_m[j]; m2 <- pool_m[k]; same <- pool_f[j] == pool_f[k]
  }
  s <- score(m1, m2)
  list(first = mean(same, na.rm = TRUE), any = mean(s$any), jaccard = mean(s$jaccard), nshared = mean(s$nshared))
}
summ <- function(d, label, by_value = NA_character_, sided = FALSE) {
  s <- score(d$m1, d$m2); e <- null_for(d, sided)
  obs <- list(first = mean(d$f1 == d$f2, na.rm = TRUE), any = mean(s$any), jaccard = mean(s$jaccard), nshared = mean(s$nshared))
  rbindlist(lapply(names(obs), function(m) data.table(subset = label, group = by_value, measure = m, n = nrow(d), observed = obs[[m]], expected = e[[m]],
                                                    lift = obs[[m]] / e[[m]], diff = obs[[m]] - e[[m]])))
}

# (a) exact-sharing pairs, all and by robustness subset (mirrors circ_homophily_robustness)
E <- WP[band == "exact"]
A <- rbindlist(list(
  summ(E, "all exact pairs"),
  summ(E[ag1 == FALSE & ag2 == FALSE], "no agency members"),
  summ(E[exact_generic & !exact_specific], "generic sentences only"),
  summ(E[exact_specific == TRUE], "includes a specific sentence")))
print(A, digits = 3); save_table(A, "occsens_exact_pairs")

# (b) by similarity band
B <- rbindlist(lapply(levels(WP$band), function(b) summ(WP[band == b], "similarity band", b)))
print(B, digits = 3); save_table(B, "occsens_by_band")

# (c) does the answer depend on multi-category workers? split exact pairs by how many categories each partner lists
# In the mixed stratum the single-category partner is placed on side 1 and the null draws each side from its own pool.
E[, kind := fifelse(n1 == 1L & n2 == 1L, "both single-category", fifelse(n1 > 1L & n2 > 1L, "both multi-category", "one multi-category"))]
sw <- E[, kind == "one multi-category" & n1 > 1L]
E[sw, `:=`(m1 = m2, m2 = m1, f1 = f2, f2 = f1, n1 = n2, n2 = n1)]
Cc <- rbindlist(lapply(unique(E$kind), function(k) summ(E[kind == k], "exact pairs by category count", k, sided = (k == "one multi-category"))))
print(Cc, digits = 3); save_table(Cc, "occsens_by_category_count")

for (m in c("first", "any", "jaccard")) {
  r <- A[subset == "all exact pairs" & measure == m]
  rec(paste0("occsens_exact_obs_", m), r$observed, paste("exact-sharing pairs, observed:", m))
  rec(paste0("occsens_exact_exp_", m), r$expected, paste("exact-sharing pairs, expected under random mixing:", m))
  rec(paste0("occsens_exact_lift_", m), r$lift, paste("exact-sharing pairs, lift over random mixing:", m))
  rg <- B[group == "[0.3,0.5)" & measure == m]
  rec(paste0("occsens_J03_05_lift_", m), rg$lift, paste("lift in the [0.3,0.5) band:", m))
}
save_numbers("20_occupation_sensitivity")
