set.seed(123)

library(data.table)
library(stringr)
library(dplyr)

cat("=============================================================\n")
cat("03b-COMMUNITY PROFILES: Louvain community sentence inspection\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading data...\n")
sentences_df  <- readRDS("/home/rstudio/data/sentences.rds")
membership    <- fread("/home/rstudio/data/louvain_membership.csv")
top_comms     <- fread("/home/rstudio/data/louvain_communities.csv")
profile_stats <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")

cat("Sentences:", nrow(sentences_df), "\n")
cat("Membership entries:", nrow(membership), "\n")
cat("Communities:", n_distinct(membership$community), "\n\n")

# =============================================================================
# 2. JOIN SENTENCES WITH COMMUNITY MEMBERSHIP
# =============================================================================

sents_dt <- as.data.table(sentences_df)
setkey(sents_dt, worker)
setkey(membership, worker)

sents_comm <- sents_dt[membership, nomatch = 0]
# sents_comm: sentence_id, worker, sentence_text, community

cat("Sentences with community assignment:", nrow(sents_comm), "\n")
cat("Workers in graph (have community):  ", n_distinct(sents_comm$worker), "\n\n")

n_total_workers <- n_distinct(sents_comm$worker)

# =============================================================================
# 3. SENTENCE FREQUENCY WITHIN EACH COMMUNITY
#    For each (community, sentence_text): how many distinct workers in that
#    community have that sentence?
# =============================================================================

cat("Computing within-community sentence frequencies...\n")

within_freq <- sents_comm[, .(
  n_in_comm  = uniqueN(worker)
), by = .(community, sentence_text)]

# Also compute global frequency (all workers in graph)
global_freq <- sents_comm[, .(
  n_global = uniqueN(worker)
), by = sentence_text]

within_freq <- merge(within_freq, global_freq, by = "sentence_text")

# Community sizes
comm_sizes_dt <- sents_comm[, .(comm_size = uniqueN(worker)), by = community]
within_freq   <- merge(within_freq, comm_sizes_dt, by = "community")

# Within-community penetration: what % of community members have this sentence?
within_freq[, pct_in_comm := n_in_comm / comm_size]

# Lift: how much more common is the sentence inside vs. outside the community?
within_freq[, lift := (n_in_comm / comm_size) /
                       (n_global / n_total_workers)]

# =============================================================================
# 4. PROFILE TOP 15 COMMUNITIES
# =============================================================================

top15_ids <- top_comms[order(-size)][1:15, community]

# Profile covariates per community
profiles_dt <- as.data.table(profile_stats)[, .(worker, country, primary_occupation)]
comm_profiles <- merge(membership, profiles_dt, by = "worker", all.x = TRUE)

cat("=============================================================\n")
cat("COMMUNITY PROFILES (top 15 by size)\n")
cat("=============================================================\n\n")

for (cid in top15_ids) {

  comm_info  <- top_comms[community == cid]
  comm_size  <- comm_info$size
  top_ctry   <- comm_info$top_country
  top_ctry_p <- comm_info$top_country_pct
  top_occ    <- comm_info$top_occ
  top_occ_p  <- comm_info$top_occ_pct

  # Top 5 country breakdown
  ctry_dist <- comm_profiles[community == cid & !is.na(country),
    .N, by = country][order(-N)][1:5]

  # Top 3 occupation breakdown
  occ_dist  <- comm_profiles[community == cid & !is.na(primary_occupation),
    .N, by = primary_occupation][order(-N)][1:3]

  # Most shared sentences within this community: ranked by pct_in_comm
  # Only show sentences present in >= 2 workers in this community
  top_sents <- within_freq[community == cid & n_in_comm >= 2][
    order(-pct_in_comm)][1:8]

  cat(sprintf("─────────────────────────────────────────────────────────\n"))
  cat(sprintf("COMMUNITY %s  (size = %s)\n",
              cid, format(comm_size, big.mark = ",")))
  cat(sprintf("  Top country: %s (%.1f%%)   Top occ: %s (%.1f%%)\n",
              top_ctry, top_ctry_p, top_occ, top_occ_p))

  cat("  Country breakdown (top 5):\n")
  for (i in seq_len(nrow(ctry_dist))) {
    cat(sprintf("    %-30s  n=%s (%.1f%%)\n",
                ctry_dist$country[i],
                format(ctry_dist$N[i], big.mark = ","),
                100 * ctry_dist$N[i] / comm_size))
  }

  cat("  Occupation breakdown (top 3):\n")
  for (i in seq_len(nrow(occ_dist))) {
    cat(sprintf("    %-35s  n=%s (%.1f%%)\n",
                occ_dist$primary_occupation[i],
                format(occ_dist$N[i], big.mark = ","),
                100 * occ_dist$N[i] / comm_size))
  }

  cat("  Most shared sentences (ranked by % of community members):\n")
  if (nrow(top_sents) == 0) {
    cat("    (no sentences shared by >= 2 members)\n")
  } else {
    for (i in seq_len(nrow(top_sents))) {
      s      <- top_sents$sentence_text[i]
      n_in   <- top_sents$n_in_comm[i]
      n_glob <- top_sents$n_global[i]
      pct    <- round(100 * top_sents$pct_in_comm[i], 1)
      lift_v <- round(top_sents$lift[i], 1)
      trunc  <- if (!is.na(s) && nchar(s) > 110) paste0(substr(s, 1, 107), "...") else s
      cat(sprintf("    [%d] %s%% of community | n_global=%s | lift=%.1fx\n",
                  i, pct, format(n_glob, big.mark = ","), lift_v))
      cat(sprintf("        \"%s\"\n", trunc))
    }
  }
  cat("\n")
}

# =============================================================================
# 5. CROSS-COMMUNITY SENTENCE OVERLAP
#    Do the top sentences per community differ across communities?
# =============================================================================

cat("=============================================================\n")
cat("--- 5. CROSS-COMMUNITY SENTENCE OVERLAP ---\n\n")

# For each of the top 15 communities, get their #1 most-shared sentence
top1_per_comm <- within_freq[community %in% top15_ids & n_in_comm >= 5][
  , .SD[which.max(pct_in_comm)], by = community]

cat("Top sentence per community (shows whether templates are distinct):\n\n")
for (i in seq_len(nrow(top1_per_comm))) {
  cid   <- top1_per_comm$community[i]
  s     <- top1_per_comm$sentence_text[i]
  pct   <- round(100 * top1_per_comm$pct_in_comm[i], 1)
  n_g   <- top1_per_comm$n_global[i]
  trunc <- if (!is.na(s) && nchar(s) > 100) paste0(substr(s, 1, 97), "...") else s
  cat(sprintf("  Community %3s (%s%% of members, n_global=%s):\n    \"%s\"\n\n",
              cid, pct, format(n_g, big.mark = ","), trunc))
}

# How many of the top-sentence texts are unique across the top 15 communities?
n_unique_top1 <- n_distinct(top1_per_comm$sentence_text, na.rm = TRUE)
cat(sprintf("Unique top-sentences across top 15 communities: %d of 15\n\n",
            n_unique_top1))

# =============================================================================
# 6. SAVE
# =============================================================================

fwrite(within_freq[n_in_comm >= 2],
       "/home/rstudio/data/community_sentence_freq.csv")

cat("Saved: data/community_sentence_freq.csv\n\n")
cat("=============================================================\n")
cat("03b-COMMUNITY PROFILES COMPLETE\n")
cat("=============================================================\n")
