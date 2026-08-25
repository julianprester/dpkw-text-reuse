set.seed(123)

library(data.table)
library(dplyr)
library(stringr)

cat("=============================================================\n")
cat("04b-TRANSMISSION-TEST\n")
cat("Broadcast vs. chain diffusion: two empirical tests\n")
cat("=============================================================\n\n")

cat("METHODOLOGICAL NOTE:\n")
cat("For exact-match sentences (J=1.0), all adopter pairs form a complete\n")
cat("clique in the similarity graph, making broadcast and chain topologies\n")
cat("indistinguishable from genealogical structure alone.\n")
cat("These tests instead use PROFILE-LEVEL similarity (how many sentences\n")
cat("two workers share overall) and TEMPORAL ORDERING under three timestamp\n")
cat("variants to look for evidence of chain vs. broadcast transmission.\n\n")
cat("Two tests:\n")
cat("  TEST A — Temporal decay: does profile similarity to the root\n")
cat("           decay as the adopter joins later? (chain predicts decay;\n")
cat("           broadcast predicts flat)\n")
cat("  TEST B — Root vs. predecessor: is each adopter more similar to the\n")
cat("           root or to their immediate temporal predecessor? (chain\n")
cat("           predicts predecessor > root; broadcast predicts root >= pred)\n\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading data...\n")
pairs_sent    <- readRDS("/home/rstudio/data/pairs_workers_30.rds")   # sentence-level
sentences_df  <- readRDS("/home/rstudio/data/sentences.rds")
profile_stats <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")

cat("Sentence-level pairs:", nrow(pairs_sent), "\n")
cat("Sentences:           ", nrow(sentences_df), "\n")
cat("Profiles:            ", nrow(profile_stats), "\n\n")

# =============================================================================
# 2. TIMESTAMP VARIANTS
#    ts_created  : created_at only
#    ts_updated  : updated_at only (NA where unavailable)
#    ts_composite: updated_at if available and > created_at + 30 days,
#                  else created_at (best single-value estimate)
# =============================================================================

cat("--- 2. BUILDING TIMESTAMP VARIANTS ---\n\n")

prof_ts <- as.data.table(profile_stats)[, .(
  worker, created_at, updated_at,
  n_sentences, country, primary_occupation
)]

prof_ts[, ts_created   := created_at]
prof_ts[, ts_updated   := updated_at]
prof_ts[, ts_composite := fifelse(
  !is.na(updated_at) & updated_at > created_at + 30 * 86400,
  updated_at,
  created_at
)]

cat("Timestamp availability:\n")
cat("  created_at non-NA: ", sum(!is.na(prof_ts$ts_created)), "\n")
cat("  updated_at non-NA: ", sum(!is.na(prof_ts$ts_updated)), "\n")
cat("  composite  non-NA: ", sum(!is.na(prof_ts$ts_composite)), "\n")

cat("\nWorkers where updated_at > created_at + 30 days:",
    sum(!is.na(prof_ts$updated_at) &
        prof_ts$updated_at > prof_ts$created_at + 30 * 86400,
        na.rm = TRUE), "\n\n")

# =============================================================================
# 3. IDENTIFY TEMPLATE COMMUNITIES
#    Exact-match sentences in >= 50 workers (same set as 04-genealogy.R)
# =============================================================================

cat("--- 3. IDENTIFYING TEMPLATE COMMUNITIES ---\n\n")

sents_dt <- as.data.table(sentences_df)
sent_freq <- sents_dt[, .(
  n_workers = uniqueN(worker),
  workers   = list(unique(worker))
), by = sentence_text]

sent_freq[, word_count := str_count(sentence_text, "\\S+")]
templates <- sent_freq[n_workers >= 50 & word_count > 8][order(-n_workers)]
cat("Traceable templates:", nrow(templates), "\n\n")

# =============================================================================
# 4. AGGREGATE SENTENCE-LEVEL PAIRS TO PROFILE-LEVEL (UNDIRECTED)
#    n_shared_sentences between any two workers
# =============================================================================

cat("--- 4. AGGREGATING TO PROFILE-LEVEL PAIRS ---\n\n")
cat("Aggregating sentence-level pairs to profile-level...\n")

pairs_dt <- as.data.table(pairs_sent)

# Create canonical undirected key so (A,B) and (B,A) are the same pair
pairs_dt[, wa := pmin(worker_a, worker_b)]
pairs_dt[, wb := pmax(worker_a, worker_b)]

profile_sim <- pairs_dt[, .(
  n_shared = .N,
  max_jac  = max(score),
  n_exact  = sum(score == 1.0)
), by = .(wa, wb)]

setkey(profile_sim, wa, wb)
cat("Unique undirected profile pairs:", nrow(profile_sim), "\n\n")

rm(pairs_dt); gc()

# Helper: look up n_shared between two workers (undirected)
lookup_sim <- function(w1, w2) {
  key_a <- pmin(w1, w2)
  key_b <- pmax(w1, w2)
  res <- profile_sim[.(key_a, key_b), .(n_shared, max_jac)]
  list(n_shared = res$n_shared, max_jac = res$max_jac)
}

# =============================================================================
# 5. RUN TESTS FOR EACH TEMPLATE
# =============================================================================

cat("=============================================================\n")
cat("--- 5. RUNNING TESTS ON TOP 30 TEMPLATES ---\n\n")

ts_vars <- c("ts_created", "ts_updated", "ts_composite")

results_a <- list()  # Test A: temporal decay
results_b <- list()  # Test B: root vs predecessor

# Process top 30 templates
for (tmpl_idx in seq_len(min(30, nrow(templates)))) {

  tmpl_text  <- templates$sentence_text[tmpl_idx]
  n_adopters <- templates$n_workers[tmpl_idx]

  adopters <- unlist(templates$workers[tmpl_idx])

  # Join timestamps
  adopter_ts <- prof_ts[worker %in% adopters,
                         .(worker, ts_created, ts_updated, ts_composite,
                           n_sentences)]

  # Run under each timestamp ordering
  for (ts_var in ts_vars) {

    ts_col <- adopter_ts[[ts_var]]
    valid  <- adopter_ts[!is.na(ts_col)]
    if (nrow(valid) < 10) next

    valid <- valid[order(valid[[ts_var]])]
    root_worker <- valid$worker[1]

    # ---- TEST A: temporal decay of profile similarity to root ----
    # For each non-root adopter, look up n_shared with root
    # and regress on days_from_root

    decay_rows <- vector("list", nrow(valid) - 1)
    for (i in seq(2, nrow(valid))) {
      wi      <- valid$worker[i]
      ts_i    <- valid[[ts_var]][i]
      ts_root <- valid[[ts_var]][1]
      days    <- as.numeric(difftime(ts_i, ts_root, units = "days"))

      sim_vals <- lookup_sim(wi, root_worker)
      n_sh     <- if (length(sim_vals$n_shared) == 0 || is.na(sim_vals$n_shared))
                    0L else sim_vals$n_shared

      decay_rows[[i - 1]] <- data.table(
        template_idx   = tmpl_idx,
        n_adopters     = n_adopters,
        ts_variant     = ts_var,
        adopter_rank   = i,
        days_from_root = days,
        n_shared       = n_sh
      )
    }
    decay_dt <- rbindlist(decay_rows)

    # Correlation between days_from_root and n_shared
    if (nrow(decay_dt[is.finite(days_from_root)]) > 5) {
      sub <- decay_dt[is.finite(days_from_root) & days_from_root > 0]
      if (nrow(sub) > 5 && sd(sub$n_shared) > 0) {
        cor_val <- cor(sub$days_from_root, sub$n_shared,
                       use = "complete.obs", method = "spearman")
      } else {
        cor_val <- NA_real_
      }
      results_a[[length(results_a) + 1]] <- data.table(
        template_idx = tmpl_idx,
        n_adopters   = n_adopters,
        ts_variant   = ts_var,
        n_valid      = nrow(sub),
        spearman_r   = cor_val,
        mean_n_shared = mean(sub$n_shared),
        pct_zero_shared = 100 * mean(sub$n_shared == 0)
      )
    }

    # ---- TEST B: root vs. predecessor similarity ----
    # For each adopter (rank >= 2), compare:
    #   n_shared(adopter, root)        [sim_to_root]
    #   n_shared(adopter, rank-1 pred) [sim_to_pred]

    pred_rows <- vector("list", nrow(valid) - 1)
    for (i in seq(2, nrow(valid))) {
      wi    <- valid$worker[i]
      wpred <- valid$worker[i - 1]

      sim_root <- lookup_sim(wi, root_worker)
      sim_pred <- lookup_sim(wi, wpred)

      n_root <- if (length(sim_root$n_shared) == 0 || is.na(sim_root$n_shared))
                  0L else sim_root$n_shared
      n_pred <- if (length(sim_pred$n_shared) == 0 || is.na(sim_pred$n_shared))
                  0L else sim_pred$n_shared

      pred_rows[[i - 1]] <- data.table(
        template_idx   = tmpl_idx,
        n_adopters     = n_adopters,
        ts_variant     = ts_var,
        adopter_rank   = i,
        n_shared_root  = n_root,
        n_shared_pred  = n_pred,
        pred_gt_root   = (n_pred > n_root)
      )
    }
    pred_dt <- rbindlist(pred_rows)
    if (nrow(pred_dt) > 0) {
      results_b[[length(results_b) + 1]] <- data.table(
        template_idx    = tmpl_idx,
        n_adopters      = n_adopters,
        ts_variant      = ts_var,
        n_valid         = nrow(pred_dt),
        mean_sim_root   = mean(pred_dt$n_shared_root),
        mean_sim_pred   = mean(pred_dt$n_shared_pred),
        pct_pred_gt_root = 100 * mean(pred_dt$pred_gt_root),
        ratio_pred_root = mean(pred_dt$n_shared_pred) /
                          pmax(mean(pred_dt$n_shared_root), 0.001)
      )
    }
  }

  if (tmpl_idx %% 10 == 0) cat("  Processed", tmpl_idx, "templates...\n")
}

results_a_dt <- rbindlist(results_a)
results_b_dt <- rbindlist(results_b)

# =============================================================================
# 6. REPORT TEST A: TEMPORAL DECAY
# =============================================================================

cat("\n=============================================================\n")
cat("--- TEST A RESULTS: Temporal decay of profile similarity to root ---\n\n")

cat("Interpretation guide:\n")
cat("  Spearman r < 0 (negative): similarity DECAYS with time -> chain-consistent\n")
cat("  Spearman r ~ 0: no relationship -> broadcast-consistent or uninformative\n")
cat("  Spearman r > 0: similarity INCREASES with time -> unexpected under both\n\n")

cat("Aggregate Spearman r by timestamp variant:\n")
agg_a <- results_a_dt[!is.na(spearman_r), .(
  n_templates  = .N,
  mean_r       = round(mean(spearman_r), 4),
  median_r     = round(median(spearman_r), 4),
  pct_negative = round(100 * mean(spearman_r < 0), 1),
  pct_near_zero = round(100 * mean(abs(spearman_r) < 0.1), 1)
), by = ts_variant]
print(agg_a)
cat("\n")

cat("Per-template Spearman r (ts_composite ordering):\n")
per_tmpl_a <- results_a_dt[ts_variant == "ts_composite"][order(template_idx)]
cat(sprintf("  %-4s  %-6s  %-10s  %-12s  %-14s\n",
            "Rank", "N_adop", "Spearman_r", "Mean_n_shr", "Pct_zero_shr"))
cat(strrep("-", 55), "\n")
for (i in seq_len(nrow(per_tmpl_a))) {
  r   <- per_tmpl_a$spearman_r[i]
  cat(sprintf("  %-4d  %-6d  %-10s  %-12.2f  %-14.1f\n",
              per_tmpl_a$template_idx[i],
              per_tmpl_a$n_adopters[i],
              if (is.na(r)) "NA" else sprintf("%+.4f", r),
              per_tmpl_a$mean_n_shared[i],
              per_tmpl_a$pct_zero_shared[i]))
}
cat("\n")

cat("Mean n_shared_sentences between adopter and root (all templates, ts_composite):\n")
cat("  Mean:", round(mean(results_a_dt[ts_variant=="ts_composite"]$mean_n_shared,
                           na.rm=TRUE), 3), "\n")
cat("  Pct with 0 shared (besides template sentence):",
    round(mean(results_a_dt[ts_variant=="ts_composite"]$pct_zero_shared,
               na.rm=TRUE), 1), "%\n\n")

# =============================================================================
# 7. REPORT TEST B: ROOT VS. PREDECESSOR
# =============================================================================

cat("=============================================================\n")
cat("--- TEST B RESULTS: Root vs. temporal predecessor similarity ---\n\n")

cat("Interpretation guide:\n")
cat("  pct_pred_gt_root > 50%: more adopters are closer to predecessor -> chain\n")
cat("  pct_pred_gt_root < 50%: more adopters are closer to root -> broadcast\n")
cat("  ratio_pred_root > 1: average predecessor similarity > root similarity -> chain\n")
cat("  ratio_pred_root < 1: average root similarity > predecessor similarity -> broadcast\n\n")

cat("Aggregate by timestamp variant:\n")
agg_b <- results_b_dt[, .(
  n_templates      = .N,
  mean_sim_root    = round(mean(mean_sim_root), 3),
  mean_sim_pred    = round(mean(mean_sim_pred), 3),
  mean_ratio       = round(mean(ratio_pred_root), 3),
  pct_pred_gt_root = round(mean(pct_pred_gt_root), 1)
), by = ts_variant]
print(agg_b)
cat("\n")

cat("Per-template results (ts_composite):\n")
per_tmpl_b <- results_b_dt[ts_variant == "ts_composite"][order(template_idx)]
cat(sprintf("  %-4s  %-6s  %-10s  %-10s  %-12s  %-8s\n",
            "Rank", "N_adop", "Sim_root", "Sim_pred", "Pct_pred>root", "Ratio"))
cat(strrep("-", 60), "\n")
for (i in seq_len(nrow(per_tmpl_b))) {
  cat(sprintf("  %-4d  %-6d  %-10.3f  %-10.3f  %-12.1f  %-8.3f\n",
              per_tmpl_b$template_idx[i],
              per_tmpl_b$n_adopters[i],
              per_tmpl_b$mean_sim_root[i],
              per_tmpl_b$mean_sim_pred[i],
              per_tmpl_b$pct_pred_gt_root[i],
              per_tmpl_b$ratio_pred_root[i]))
}
cat("\n")

# =============================================================================
# 8. ROBUSTNESS: COMPARE RESULTS ACROSS TIMESTAMP ORDERINGS
# =============================================================================

cat("=============================================================\n")
cat("--- 8. ROBUSTNESS: CONSISTENCY ACROSS TIMESTAMP VARIANTS ---\n\n")

# For Test A: compare Spearman r across variants (do they agree in sign?)
spread_a <- dcast(results_a_dt[!is.na(spearman_r)],
                  template_idx ~ ts_variant,
                  value.var = "spearman_r")
if (all(c("ts_created", "ts_updated", "ts_composite") %in% names(spread_a))) {
  spread_a[, agree_sign :=
    sign(ts_created) == sign(ts_composite) &
    (is.na(ts_updated) | sign(ts_updated) == sign(ts_composite))]
  cat("Test A — Fraction of templates where all timestamp variants\n")
  cat("agree on sign of Spearman r:",
      round(100 * mean(spread_a$agree_sign, na.rm = TRUE), 1), "%\n\n")
}

# For Test B: compare pct_pred_gt_root across variants
spread_b <- dcast(results_b_dt,
                  template_idx ~ ts_variant,
                  value.var = "pct_pred_gt_root")
if (all(c("ts_created", "ts_composite") %in% names(spread_b))) {
  spread_b[, direction_created   := ts_created   > 50]
  spread_b[, direction_composite := ts_composite > 50]
  if ("ts_updated" %in% names(spread_b))
    spread_b[, direction_updated := ts_updated > 50]
  spread_b[, agree_direction :=
    direction_created == direction_composite]
  cat("Test B — Fraction of templates where created_at and composite\n")
  cat("orderings agree on broadcast vs. chain direction:",
      round(100 * mean(spread_b$agree_direction, na.rm = TRUE), 1), "%\n\n")
}

# =============================================================================
# 9. INTERPRETATION SUMMARY
# =============================================================================

cat("=============================================================\n")
cat("--- 9. INTERPRETATION SUMMARY ---\n\n")

# Test A overall verdict
a_composite <- results_a_dt[ts_variant == "ts_composite" & !is.na(spearman_r)]
mean_r_a <- mean(a_composite$spearman_r)
pct_neg  <- 100 * mean(a_composite$spearman_r < -0.05)
pct_pos  <- 100 * mean(a_composite$spearman_r >  0.05)
pct_flat <- 100 - pct_neg - pct_pos

cat("TEST A SUMMARY (composite timestamp):\n")
cat(sprintf("  Mean Spearman r across templates: %+.4f\n", mean_r_a))
cat(sprintf("  Templates with r < -0.05 (chain-consistent):     %.1f%%\n", pct_neg))
cat(sprintf("  Templates with r > +0.05 (unexpected):           %.1f%%\n", pct_pos))
cat(sprintf("  Templates with |r| <= 0.05 (broadcast/flat):     %.1f%%\n", pct_flat))
if (mean_r_a < -0.05) {
  cat("  VERDICT: Mild negative trend — some chain-consistent signal\n")
} else if (abs(mean_r_a) <= 0.05) {
  cat("  VERDICT: No decay — consistent with broadcast or uninformative\n")
} else {
  cat("  VERDICT: Positive trend — unexpected; requires further investigation\n")
}
cat("\n")

# Test B overall verdict
b_composite <- results_b_dt[ts_variant == "ts_composite"]
mean_ratio  <- mean(b_composite$ratio_pred_root)
mean_pct    <- mean(b_composite$pct_pred_gt_root)

cat("TEST B SUMMARY (composite timestamp):\n")
cat(sprintf("  Mean ratio (pred_sim / root_sim): %.3f\n", mean_ratio))
cat(sprintf("  Mean pct adopters closer to predecessor: %.1f%%\n", mean_pct))
if (mean_ratio > 1.05 && mean_pct > 50) {
  cat("  VERDICT: Predecessor similarity > root similarity — chain-consistent\n")
} else if (mean_ratio < 0.95 && mean_pct < 50) {
  cat("  VERDICT: Root similarity > predecessor similarity — broadcast-consistent\n")
} else {
  cat("  VERDICT: Ambiguous — no clear directional signal\n")
}

cat("\n")
cat("OVERALL METHODOLOGICAL CONCLUSION:\n")
cat("The broadcast vs. chain question cannot be definitively resolved from\n")
cat("sentence-level similarity data alone, because identical sentences create\n")
cat("complete cliques in the similarity graph regardless of transmission path.\n")
cat("The profile-level tests above offer partial but indirect evidence.\n")
cat("The strength of the evidence and its direction across timestamp variants\n")
cat("should inform how confidently we characterise the diffusion structure.\n\n")

# =============================================================================
# 10. SAVE OUTPUTS
# =============================================================================

fwrite(results_a_dt, "/home/rstudio/data/transmission_test_a.csv")
fwrite(results_b_dt, "/home/rstudio/data/transmission_test_b.csv")
cat("Saved: data/transmission_test_a.csv, transmission_test_b.csv\n\n")

cat("=============================================================\n")
cat("04b-TRANSMISSION-TEST COMPLETE\n")
cat("=============================================================\n")
