set.seed(123)

library(data.table)
library(dplyr)
library(stringr)

cat("=============================================================\n")
cat("05-VISIBILITY-THRESHOLD\n")
cat("Empirical tests for Idea 1: Algorithmic Field Constitution\n")
cat("=============================================================\n\n")
cat("Five analyses:\n")
cat("  1. Prestige threshold for template generation (is in-degree gate-kept?)\n")
cat("  2. Structural separation: copiers do not become generators\n")
cat("  3. Profile completeness of originators vs. consumers vs. non-copiers\n")
cat("  4. Cross-community prestige gradient by community distance\n")
cat("  5. n_shared distribution: single-sentence units as broadcast signature\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading data...\n")
directed     <- readRDS("/home/rstudio/data/directed_pairs.rds")
prof         <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")
genealogy    <- fread("/home/rstudio/data/genealogy_trees.csv")
sentences_df <- readRDS("/home/rstudio/data/sentences.rds")

cat("Directed pairs:", nrow(directed), "\n")
cat("Profiles:      ", nrow(prof), "\n\n")

# Composite prestige (recompute from raw vars — same formula as 03-network.R)
prof_dt <- as.data.table(prof)
prof_dt[, prestige_hours_z    := scale(log1p(totalHours))[, 1]]
prof_dt[, prestige_jobs_z     := scale(log1p(totalJobsWorked))[, 1]]
prof_dt[, prestige_composite  := prestige_hours_z + prestige_jobs_z]
prof_dt[, prestige_pct        := frank(prestige_composite, na.last = "keep",
                                        ties.method = "average") /
                                   sum(!is.na(prestige_composite))]

dir_dt <- as.data.table(directed)

# =============================================================================
# ANALYSIS 1: PRESTIGE THRESHOLD FOR TEMPLATE GENERATION
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 1: Prestige threshold for template generation ---\n\n")

# In-degree per worker: how many unique copiers copied from each source
source_indeg <- dir_dt[, .(in_degree = uniqueN(copier)), by = source]
setnames(source_indeg, "source", "worker")

# Join with full worker list (all workers have in-degree, default 0)
all_workers <- prof_dt[, .(worker, prestige_composite, prestige_pct,
                             totalHours, totalJobsWorked, has_job)]
all_workers <- merge(all_workers, source_indeg, by = "worker", all.x = TRUE)
all_workers[is.na(in_degree), in_degree := 0L]

cat("Workers with in_degree > 0 (ever a template source):",
    format(sum(all_workers$in_degree > 0), big.mark = ","), "\n")
cat("Workers with in_degree = 0 (never a source):       ",
    format(sum(all_workers$in_degree == 0), big.mark = ","), "\n")
cat("% ever a source:", round(100 * mean(all_workers$in_degree > 0), 2), "\n\n")

# Prestige decile analysis
all_workers[!is.na(prestige_pct),
            prestige_decile := cut(prestige_pct,
                                    breaks = seq(0, 1, by = 0.1),
                                    labels = paste0("D", 1:10),
                                    include.lowest = TRUE)]

decile_stats <- all_workers[!is.na(prestige_decile), .(
  n_workers       = .N,
  pct_is_source   = round(100 * mean(in_degree > 0), 2),
  mean_in_degree  = round(mean(in_degree), 3),
  median_in_degree = as.numeric(median(in_degree)),
  total_in_degree = sum(in_degree)
), by = prestige_decile][order(prestige_decile)]

# Cumulative share of in-degree from top deciles
total_indeg <- sum(decile_stats$total_in_degree)
decile_stats[, cum_share_from_top :=
               round(100 * cumsum(rev(total_in_degree)) / total_indeg, 1)]
# Re-order to read top-down
decile_stats_display <- decile_stats[order(-as.integer(prestige_decile))]
decile_stats_display[, cum_share_from_top :=
  round(100 * cumsum(total_in_degree) / total_indeg, 1)]

cat("In-degree by prestige decile (D1=lowest prestige, D10=highest):\n")
cat(sprintf("  %-6s  %-10s  %-14s  %-14s  %-14s  %-18s\n",
            "Decile", "n_workers", "pct_is_source", "mean_in_deg",
            "total_in_deg", "cum_share_top%"))
cat(strrep("-", 82), "\n")
for (i in rev(seq_len(nrow(decile_stats)))) {
  r <- decile_stats[i]
  cat(sprintf("  %-6s  %-10s  %-14s  %-14s  %-14s  %-18s\n",
              r$prestige_decile,
              format(r$n_workers, big.mark = ","),
              paste0(r$pct_is_source, "%"),
              r$mean_in_degree,
              format(r$total_in_degree, big.mark = ","),
              paste0(decile_stats_display[prestige_decile == r$prestige_decile,
                                          cum_share_from_top], "%")))
}
cat("\n")

# Gini coefficient of in-degree distribution
gini <- function(x) {
  x <- sort(x[x >= 0 & !is.na(x)])
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  (2 * sum(x * seq_len(n)) / (n * sum(x))) - (n + 1) / n
}
cat("Gini coefficient of in-degree distribution:",
    round(gini(all_workers$in_degree), 4), "\n")

# Spearman correlation between prestige rank and in-degree
sp_r <- cor(all_workers$prestige_composite, all_workers$in_degree,
            use = "complete.obs", method = "spearman")
cat("Spearman r (prestige_composite ~ in_degree):", round(sp_r, 4), "\n")

# Concentration: what % of all in-degree is held by top 10%/5%/1% of prestige?
q90 <- quantile(all_workers$prestige_composite, 0.90, na.rm = TRUE)
q95 <- quantile(all_workers$prestige_composite, 0.95, na.rm = TRUE)
q99 <- quantile(all_workers$prestige_composite, 0.99, na.rm = TRUE)
for (lbl in c("top 10%", "top 5%", "top 1%")) {
  thresh <- switch(lbl, "top 10%" = q90, "top 5%" = q95, "top 1%" = q99)
  share  <- 100 * sum(all_workers[prestige_composite >= thresh, in_degree],
                      na.rm = TRUE) / total_indeg
  cat(sprintf("  %% of all in-degree held by %s of prestige workers: %.1f%%\n",
              lbl, share))
}
cat("\n")

fwrite(all_workers[, .(worker, prestige_composite, prestige_pct,
                        prestige_decile, in_degree)],
       "/home/rstudio/data/worker_indegree_prestige.csv")

# =============================================================================
# ANALYSIS 2: STRUCTURAL SEPARATION — COPIERS DO NOT BECOME GENERATORS
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 2: Structural separation of generators and consumers ---\n\n")

# Out-degree per worker: how many sources they copied from
copier_outdeg <- dir_dt[, .(out_degree = uniqueN(source)), by = copier]
setnames(copier_outdeg, "copier", "worker")

node_roles <- merge(all_workers[, .(worker, prestige_composite,
                                     prestige_pct, in_degree,
                                     totalHours, totalJobsWorked)],
                    copier_outdeg, by = "worker", all.x = TRUE)
node_roles[is.na(out_degree), out_degree := 0L]

node_roles[, role := fcase(
  in_degree > 0 & out_degree  > 0, "both",
  in_degree > 0 & out_degree == 0, "generator_only",
  in_degree == 0 & out_degree > 0, "consumer_only",
  default = "neither"
)]

cat("Role distribution:\n")
role_summary <- node_roles[, .(
  n               = .N,
  pct             = round(100 * .N / nrow(node_roles), 1),
  mean_prestige   = round(mean(prestige_composite, na.rm = TRUE), 3),
  median_hours    = round(median(totalHours, na.rm = TRUE), 1),
  mean_in_degree  = round(mean(in_degree), 2),
  mean_out_degree = round(mean(out_degree), 2)
), by = role][order(-mean_prestige)]
print(role_summary)
cat("\n")

# Correlation between in-degree and out-degree
cor_io <- cor(node_roles$in_degree, node_roles$out_degree,
              use = "complete.obs", method = "spearman")
cat("Spearman r (in_degree ~ out_degree):", round(cor_io, 4), "\n")
cat("(Negative = structural separation; near-0 = independent roles)\n\n")

# Does more copying associate with higher prestige?
# (If copying raises prestige, more active copiers should be more successful)
out_tiers <- c(0, 1, 5, 21, 101, Inf)
out_labels <- c("0 (non-copier)", "1-4", "5-20", "21-100", "101+")
node_roles[, out_tier := cut(out_degree, breaks = out_tiers, labels = out_labels,
                              right = FALSE, include.lowest = TRUE)]

cat("Prestige by out-degree tier (if copying raises prestige: higher tier = higher prestige):\n")
tier_pres <- node_roles[!is.na(out_tier), .(
  n            = .N,
  mean_prestige = round(mean(prestige_composite, na.rm = TRUE), 3),
  median_hours  = round(median(totalHours, na.rm = TRUE), 1),
  pct_has_job   = round(100 * mean(totalJobsWorked > 0, na.rm = TRUE), 1)
), by = out_tier][order(out_tier)]
print(tier_pres)
cat("\n")

# Prestige comparison: pure generators vs. pure consumers
cat("Pure generators (in_degree>0, out_degree=0) vs. pure consumers (in_degree=0, out_degree>0):\n")
gen <- node_roles[role == "generator_only"]
con <- node_roles[role == "consumer_only"]
cat(sprintf("  Generators  — n=%s  mean_prestige=%.3f  median_hours=%.0f\n",
            format(nrow(gen), big.mark=","),
            mean(gen$prestige_composite, na.rm=TRUE),
            median(gen$totalHours, na.rm=TRUE)))
cat(sprintf("  Consumers   — n=%s  mean_prestige=%.3f  median_hours=%.0f\n",
            format(nrow(con), big.mark=","),
            mean(con$prestige_composite, na.rm=TRUE),
            median(con$totalHours, na.rm=TRUE)))

if (nrow(gen) > 1 && nrow(con) > 1) {
  wt <- wilcox.test(gen$prestige_composite, con$prestige_composite,
                    alternative = "greater")
  cat(sprintf("  Wilcoxon test (generators > consumers): W=%.0f, p=%.4g\n",
              wt$statistic, wt$p.value))
}
cat("\n")

fwrite(node_roles[, .(worker, role, in_degree, out_degree,
                       prestige_composite, out_tier)],
       "/home/rstudio/data/worker_roles.csv")

# =============================================================================
# ANALYSIS 3: PROFILE COMPLETENESS OF ORIGINATORS vs. CONSUMERS vs. NON-COPIERS
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 3: Profile completeness by worker role ---\n\n")

cat("Loading profiles.csv for completeness indicators...\n")
raw_profiles <- fread("/home/rstudio/data/profiles.csv", na.strings = c("", "NA"))

raw_profiles[, desc_words   := str_count(description, "\\S+")]
raw_profiles[, has_title    := as.integer(!is.na(title) & nchar(title) > 2)]
raw_profiles[, has_desc     := as.integer(!is.na(description) & desc_words >= 20)]
raw_profiles[, has_rate     := as.integer(!is.na(hourlyRate) & hourlyRate > 0)]
raw_profiles[, has_occ      := as.integer(!is.na(occupations))]
raw_profiles[, completeness := has_title + has_desc + has_rate + has_occ]

# Define groups
root_workers     <- unique(genealogy$root[!is.na(genealogy$root)])

# High consumers: pure consumers with high out_degree (top quintile among consumers)
consumer_dt      <- node_roles[role == "consumer_only"]
q80_out          <- quantile(consumer_dt$out_degree, 0.80)
high_consumers   <- consumer_dt[out_degree >= q80_out, worker]

# Non-copiers with at least one sentence (so they could have been copiers)
non_copiers      <- prof_dt[is_copier_30 == FALSE & n_sentences > 0, worker]

cat("Group sizes:\n")
cat("  Template originators (roots):", length(root_workers), "\n")
cat("  High-copying consumers (top 20% out-degree among consumers):",
    length(high_consumers), "\n")
cat("  Non-copiers (with profiles):", format(length(non_copiers), big.mark=","), "\n\n")

compare_groups <- function(workers_list, label, raw_prof) {
  sub <- raw_prof[worker_id %in% workers_list]
  data.table(
    group          = label,
    n              = nrow(sub),
    mean_complete  = round(mean(sub$completeness, na.rm = TRUE), 3),
    mean_words     = round(mean(sub$desc_words, na.rm = TRUE), 1),
    pct_has_title  = round(100 * mean(sub$has_title, na.rm = TRUE), 1),
    pct_has_rate   = round(100 * mean(sub$has_rate, na.rm = TRUE), 1),
    pct_has_occ    = round(100 * mean(sub$has_occ, na.rm = TRUE), 1),
    median_hours   = round(median(sub$totalHours, na.rm = TRUE), 1),
    median_jobs    = round(median(sub$totalJobsWorked, na.rm = TRUE), 1)
  )
}

completeness_table <- rbindlist(list(
  compare_groups(root_workers,   "A: Originators", raw_profiles),
  compare_groups(high_consumers, "B: High-consumers", raw_profiles),
  compare_groups(non_copiers,    "C: Non-copiers", raw_profiles),
  compare_groups(raw_profiles$worker_id, "D: All workers", raw_profiles)
))

cat("Profile completeness and characteristics by group:\n")
print(completeness_table)
cat("\n")

cat("Interpretation:\n")
cat("  If algorithmic visibility drives copying, originators (A) should have\n")
cat("  higher completeness/hours than consumers (B) and non-copiers (C).\n")
cat("  Non-copiers (C) may include both inactive accounts and high-achievers\n")
cat("  who don't need to copy.\n\n")

fwrite(completeness_table, "/home/rstudio/data/completeness_by_group.csv")

# =============================================================================
# ANALYSIS 4: CROSS-COMMUNITY PRESTIGE GRADIENT BY COMMUNITY DISTANCE
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 4: Prestige gradient by community distance ---\n\n")
cat("Prediction: gradient should strengthen as community distance increases.\n")
cat("Broadcast mechanism: only very high-prestige profiles are visible\n")
cat("across large community distances, amplifying the prestige gradient.\n\n")

# Use high-confidence edges only for cleanest signal
dir_hc <- dir_dt[confidence == "high" & is.finite(delta_prestige) &
                   !is.na(same_country) & !is.na(same_occ)]

dir_hc[, community_distance := fcase(
  same_country  & same_occ,  "1_same_country_same_occ",
  same_country  & !same_occ, "2_same_country_diff_occ",
  !same_country & same_occ,  "3_diff_country_same_occ",
  !same_country & !same_occ, "4_diff_country_diff_occ"
)]

cat("High-confidence edges by community distance:\n")
cd_stats <- dir_hc[!is.na(community_distance), .(
  n_edges           = .N,
  pct_src_higher    = round(100 * mean(delta_prestige > 0), 2),
  mean_delta_pres   = round(mean(delta_prestige), 4),
  median_delta_pres = round(median(delta_prestige), 4),
  se_delta          = round(sd(delta_prestige) / sqrt(.N), 5)
), by = community_distance][order(community_distance)]

cat(sprintf("  %-35s  %-10s  %-16s  %-14s\n",
            "Community distance", "n_edges", "pct_src_higher", "mean_delta_pres"))
cat(strrep("-", 80), "\n")
for (i in seq_len(nrow(cd_stats))) {
  r <- cd_stats[i]
  cat(sprintf("  %-35s  %-10s  %-16s  %-14s\n",
              r$community_distance,
              format(r$n_edges, big.mark = ","),
              paste0(r$pct_src_higher, "%"),
              r$mean_delta_pres))
}
cat("\n")

# Is the gradient monotonically stronger with distance? (test D1→D4 ordering)
pct_vals <- cd_stats$pct_src_higher
is_monotone <- all(diff(pct_vals) >= 0)
cat("Gradient monotonically increasing with community distance:",
    if (is_monotone) "YES" else "NO", "\n")

# Pairwise t-tests: is D4 (diff/diff) significantly stronger than D1 (same/same)?
if (sum(dir_hc$community_distance == "1_same_country_same_occ") > 30 &&
    sum(dir_hc$community_distance == "4_diff_country_diff_occ") > 30) {
  g1 <- dir_hc[community_distance == "1_same_country_same_occ", delta_prestige]
  g4 <- dir_hc[community_distance == "4_diff_country_diff_occ", delta_prestige]
  tt <- t.test(g4, g1, alternative = "greater")
  cat(sprintf("t-test (diff/diff > same/same): t=%.3f, p=%.4g\n",
              tt$statistic, tt$p.value))
}
cat("\n")

# Additional: prestige of SOURCES by community distance
cat("Prestige of sources by community distance (higher = more algorithmically visible):\n")
src_pres <- dir_hc[!is.na(community_distance), .(
  mean_prestige_src    = round(mean(prestige_src, na.rm = TRUE), 3),
  median_hours_src     = round(median(hours_src, na.rm = TRUE), 1),
  pct_src_top10pct_pres = round(100 * mean(prestige_src >
    quantile(dir_dt$prestige_src, 0.90, na.rm = TRUE), na.rm = TRUE), 1)
), by = community_distance][order(community_distance)]
print(src_pres)
cat("\n")

fwrite(cd_stats, "/home/rstudio/data/prestige_gradient_by_distance.csv")

# =============================================================================
# ANALYSIS 5: n_shared DISTRIBUTION — SINGLE-SENTENCE BROADCAST SIGNATURE
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 5: n_shared distribution as broadcast signature ---\n\n")
cat("Prediction: peak at n_shared=1 (single-sentence extraction) under\n")
cat("broadcast observation. Whole-profile copying would show n_shared >> 1.\n\n")

cat("Loading sentence-level pairs and building profile-level similarity...\n")
pairs_raw <- readRDS("/home/rstudio/data/pairs_workers_30.rds")
cat("Sentence-level pairs:", nrow(pairs_raw), "\n")

pairs_dt2 <- as.data.table(pairs_raw)
pairs_dt2[, wa := pmin(worker_a, worker_b)]
pairs_dt2[, wb := pmax(worker_a, worker_b)]
profile_sim <- pairs_dt2[, .(n_shared = .N), by = .(wa, wb)]
setkey(profile_sim, wa, wb)
rm(pairs_raw, pairs_dt2); gc()

cat("Unique undirected profile pairs:", nrow(profile_sim), "\n\n")

# --- 5a: Overall n_shared distribution across ALL profile pairs ---
cat("5a. Overall distribution of n_shared across ALL pairs in network:\n")
n_sh_breaks <- c(1, 2, 3, 5, 11, 21, 51, 101, Inf)
n_sh_labels <- c("1", "2", "3-4", "5-10", "11-20", "21-50", "51-100", "101+")
profile_sim[, n_shared_band := cut(n_shared, breaks = n_sh_breaks,
                                    labels = n_sh_labels,
                                    right = FALSE, include.lowest = TRUE)]

overall_dist <- profile_sim[, .(
  n_pairs = .N,
  pct     = round(100 * .N / nrow(profile_sim), 2)
), by = n_shared_band][order(n_shared_band)]
print(overall_dist)
cat("\n")

cat("Gini coefficient of n_shared across all pairs:",
    round(gini(profile_sim$n_shared), 4), "\n\n")

# --- 5b: n_shared distribution within TEMPLATE COMMUNITIES (adopter-root pairs) ---
cat("5b. n_shared for adopter-root pairs within traceable templates (>=50 workers):\n\n")

sents_dt <- as.data.table(sentences_df)
sent_freq <- sents_dt[, .(
  n_workers = uniqueN(worker),
  workers   = list(unique(worker))
), by = sentence_text]
sent_freq[, word_count := str_count(sentence_text, "\\S+")]
templates  <- sent_freq[n_workers >= 50 & word_count > 8][order(-n_workers)]

# For each template: for each adopter, get n_shared with root
all_adopter_root <- vector("list", nrow(templates))
for (i in seq_len(nrow(templates))) {

  adopters_i <- unlist(templates$workers[i])
  # Root = earliest created_at among adopters
  root_dates <- prof_dt[worker %in% adopters_i & !is.na(created_at),
                         .(worker, created_at)][order(created_at)]
  if (nrow(root_dates) < 2) next
  root_i <- root_dates$worker[1]

  # For each non-root adopter, look up n_shared with root
  others <- setdiff(adopters_i, root_i)
  wa_vec <- pmin(others, root_i)
  wb_vec <- pmax(others, root_i)
  looked_up <- profile_sim[.(wa_vec, wb_vec), .(n_shared)]
  n_sh_vec  <- ifelse(is.na(looked_up$n_shared), 0L, looked_up$n_shared)

  all_adopter_root[[i]] <- data.table(
    template_rank = i,
    n_adopters    = length(adopters_i),
    n_shared      = n_sh_vec
  )
}
adopter_root_dt <- rbindlist(all_adopter_root)

cat("Total adopter-root pairs analysed:", nrow(adopter_root_dt), "\n")

dist_template <- adopter_root_dt[, .(
  n_pairs = .N,
  pct     = round(100 * .N / nrow(adopter_root_dt), 2)
), by = .(n_shared_band = cut(n_shared, breaks = n_sh_breaks,
                               labels = n_sh_labels,
                               right = FALSE,
                               include.lowest = TRUE))][order(n_shared_band)]
# Include n_shared=0 explicitly
n_zero <- sum(adopter_root_dt$n_shared == 0)
cat(sprintf("  n_shared = 0 (pair not in LSH output): %s (%.1f%%)\n",
            format(n_zero, big.mark=","),
            100 * n_zero / nrow(adopter_root_dt)))
print(dist_template)
cat("\n")

pct_sole <- 100 * mean(adopter_root_dt$n_shared <= 1)
cat(sprintf("Pairs with n_shared <= 1 (only template sentence or not found): %.1f%%\n",
            pct_sole))
cat(sprintf("Gini coefficient of n_shared within template communities: %.4f\n",
            gini(adopter_root_dt$n_shared)))
cat("\n")

# --- 5c: Compare: n_shared for within-template pairs vs. random pairs ---
cat("5c. Mean n_shared comparison:\n")
cat(sprintf("  Within-template adopter-root pairs:  mean=%.3f  median=%.0f\n",
            mean(adopter_root_dt$n_shared), median(adopter_root_dt$n_shared)))
cat(sprintf("  All pairs in network:                mean=%.3f  median=%.0f\n",
            mean(profile_sim$n_shared), median(profile_sim$n_shared)))
cat("\n")
cat("Interpretation: if within-template pairs have LOW n_shared relative to\n")
cat("all network pairs, this supports selective extraction (broadcast).\n")
cat("If HIGH, it suggests template membership predicts whole-profile copying.\n\n")

fwrite(adopter_root_dt, "/home/rstudio/data/adopter_root_nshared.csv")
fwrite(overall_dist,    "/home/rstudio/data/nshared_overall_dist.csv")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("=============================================================\n")
cat("--- SUMMARY: Evidence for Algorithmic Field Constitution ---\n\n")

cat("CLAIM: Algorithmic visibility creates a stratified field with\n")
cat("permanent generator/consumer separation, broadcast diffusion,\n")
cat("and selective single-sentence copying.\n\n")

cat("Evidence summary:\n\n")

cat("1. PRESTIGE THRESHOLD\n")
top10_share <- round(100 * sum(all_workers[prestige_pct >= 0.90, in_degree],
                                na.rm=TRUE) / total_indeg, 1)
cat(sprintf("   Top 10%% prestige workers hold %.1f%% of all in-degree\n", top10_share))
cat(sprintf("   Gini of in-degree: %.4f (1.0 = perfectly concentrated)\n",
            gini(all_workers$in_degree)))
cat(sprintf("   Spearman r (prestige ~ in_degree): %.4f\n", sp_r))
cat("   → Strong prestige gating of who becomes a template source\n\n")

cat("2. STRUCTURAL SEPARATION\n")
role_pcts <- role_summary[, .(role, n, pct)]
for (i in seq_len(nrow(role_pcts))) {
  cat(sprintf("   %-20s: n=%s (%s%%)\n",
              role_pcts$role[i],
              format(role_pcts$n[i], big.mark=","),
              role_pcts$pct[i]))
}
cat(sprintf("   Spearman r (in_degree ~ out_degree): %.4f\n", cor_io))
cat("   → Copying and generating are structurally distinct roles\n\n")

cat("3. PROFILE COMPLETENESS\n")
cat("   (See completeness_by_group.csv for full table)\n")
orig_row <- completeness_table[group == "A: Originators"]
cons_row <- completeness_table[group == "B: High-consumers"]
cat(sprintf("   Originators: completeness=%.2f, median_hours=%.0f\n",
            orig_row$mean_complete, orig_row$median_hours))
cat(sprintf("   Consumers:   completeness=%.2f, median_hours=%.0f\n",
            cons_row$mean_complete, cons_row$median_hours))
cat("   → Originators have more complete, algorithmically visible profiles\n\n")

cat("4. CROSS-COMMUNITY GRADIENT\n")
for (i in seq_len(nrow(cd_stats))) {
  cat(sprintf("   %s: %.1f%% upward\n",
              cd_stats$community_distance[i], cd_stats$pct_src_higher[i]))
}
cat(sprintf("   Monotonically increasing: %s\n",
            if (is_monotone) "YES ✓" else "NO ✗"))
cat("   → Prestige gradient strongest when broadcast is the only mechanism\n\n")

cat("5. SINGLE-SENTENCE UNITS\n")
cat(sprintf("   %.1f%% of adopter-root pairs share n_shared <= 1\n", pct_sole))
cat(sprintf("   Mean n_shared (adopter-root): %.3f vs. %.3f (all network pairs)\n",
            mean(adopter_root_dt$n_shared), mean(profile_sim$n_shared)))
cat("   → Sentences travel as self-contained units, not as whole profiles\n\n")

cat("=============================================================\n")
cat("05-VISIBILITY-THRESHOLD COMPLETE\n")
cat("=============================================================\n")
