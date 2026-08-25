set.seed(123)

library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(tidytext)
library(stringr)
library(tokenizers)
library(purrr)
library(lubridate)

cat("=============================================================\n")
cat("01-BUILD-PAIRS: Sentence mapping, joins, profile reuse stats\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. LOAD AND CLEAN PROFILES
# =============================================================================

cat("Loading profiles.csv...\n")
dpkw_data <- fread("/home/rstudio/data/profiles.csv", na.strings = c("", "NA"))

# Parse occupations: extract first listed category from Python-style list string
# e.g. "['Admin Support', 'Writing']" -> "Admin Support"
dpkw_data[, primary_occupation := str_match(occupations, "'([^']+)'")[, 2]]

# Experience / status flags
dpkw_data[, has_job      := totalJobsWorked > 0 & !is.na(totalJobsWorked)]
dpkw_data[, created_at_p := ymd_hms(created_at, quiet = TRUE)]
dpkw_data[, updated_at_p := ymd_hms(updated_at, quiet = TRUE)]

cat("Profiles loaded:", nrow(dpkw_data), "\n")
cat("Primary occupation — top 15:\n")
print(dpkw_data[, .N, by = primary_occupation][order(-N)][1:15])
cat("\n")

# =============================================================================
# 2. REPLICATE TOKENISATION FROM text-reuse.R
#    (Must be identical to produce matching sentence IDs)
# =============================================================================

cat("Replicating tokenisation from text-reuse.R...\n")

dpkw_df <- tibble(
  worker      = dpkw_data$worker_id,
  description = dpkw_data$description
)

# Word-frequency filter: keep workers who have >= 1 non-stop, non-numeric word
# that appears >100 times across the corpus
dpkw_tidy <- dpkw_df %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords(), by = "word") %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)

dpkw_covariates <- dpkw_tidy %>%
  distinct(worker) %>%
  replace_na(list(worker = ""))

analysis_df <- dpkw_covariates %>%
  inner_join(dpkw_df, by = "worker")

cat("Profiles after word-frequency filter:", nrow(analysis_df), "\n")
rm(dpkw_tidy, dpkw_covariates, dpkw_df)

# Clean text (same as text-reuse.R)
analysis_df <- analysis_df %>%
  mutate(
    clean_text = str_replace_all(description, "\\r\\n|\\r|\\n", "\n"),
    clean_text = str_replace_all(clean_text, "[\\s\t]+", " "),
    clean_text = str_trim(clean_text)
  )

# Tokenise into sentences; filter 8–50 words; assign sentence IDs
cat("Tokenising into sentences (8–50 words)...\n")
sentences_df <- analysis_df %>%
  select(worker, clean_text) %>%
  mutate(sentences = map(clean_text, ~tokenize_sentences(., simplify = TRUE))) %>%
  unnest(sentences) %>%
  mutate(word_count = str_count(sentences, "\\S+")) %>%
  filter(!is.na(sentences), word_count >= 8, word_count <= 50) %>%
  rowid_to_column("unique_sentence_row_id") %>%
  mutate(sentence_id = paste0("sent_", unique_sentence_row_id)) %>%
  select(sentence_id, worker, sentence_text = sentences) %>%
  mutate(sentence_text = str_to_lower(sentence_text))

cat("Total sentences:", nrow(sentences_df), "\n")
cat("Unique workers with >= 1 sentence:", n_distinct(sentences_df$worker), "\n\n")

# =============================================================================
# 3. LOAD SIMILAR PAIRS AND JOIN TO WORKERS
# =============================================================================

cat("Loading similar_pairs.csv...\n")
pairs_raw <- fread("/home/rstudio/data/similar_pairs.csv")
cat("Raw pairs:", nrow(pairs_raw), "\n")

# Join sentence IDs -> worker IDs
sentences_dt <- as.data.table(sentences_df)
setkey(sentences_dt, sentence_id)

pairs_workers <- pairs_raw[
  sentences_dt[, .(sentence_id, worker_a = worker, sentence_text_a = sentence_text)],
  on = .(a = sentence_id), nomatch = 0
][
  sentences_dt[, .(sentence_id, worker_b = worker, sentence_text_b = sentence_text)],
  on = .(b = sentence_id), nomatch = 0
]

# Remove self-pairs
pairs_workers <- pairs_workers[worker_a != worker_b]
cat("Pairs after removing self-pairs:", nrow(pairs_workers), "\n")

# Apply J >= 0.3 threshold
pairs_30 <- pairs_workers[score >= 0.3]
cat("Pairs at J >= 0.3:", nrow(pairs_30), "\n")

# Report threshold summary
thresholds <- c(0.30, 0.50, 0.70, 1.00)
for (t in thresholds) {
  n <- nrow(pairs_workers[score >= t])
  cat(sprintf("  J >= %.2f : %s pairs between different workers\n", t, format(n, big.mark = ",")))
}
cat("\n")

rm(pairs_raw)
gc()

# =============================================================================
# 4. PROFILE-LEVEL REUSE STATISTICS
# =============================================================================

cat("Computing profile-level reuse statistics...\n")

# Count sentences per worker
sentence_counts <- sentences_dt[, .(n_sentences = .N), by = worker]

# For each threshold, find sentences that appear in at least one cross-worker pair
compute_reuse <- function(pairs_dt, threshold, col_suffix) {
  p <- pairs_dt[score >= threshold]
  # All sentence IDs (from either side) that are involved in a cross-worker pair
  sents_a <- p[, .(sentence_id = a, worker = worker_a)]
  sents_b <- p[, .(sentence_id = b, worker = worker_b)]
  involved <- unique(rbind(sents_a, sents_b))
  involved[, .(n_copied = .N), by = worker]
}

reuse_30  <- compute_reuse(pairs_workers, 0.30, "30")
reuse_50  <- compute_reuse(pairs_workers, 0.50, "50")
reuse_100 <- compute_reuse(pairs_workers, 1.00, "100")

setnames(reuse_30,  "n_copied", "n_sentences_copied_30")
setnames(reuse_50,  "n_copied", "n_sentences_copied_50")
setnames(reuse_100, "n_copied", "n_sentences_exact")

# Number of distinct other workers each worker shares sentences with (at J>=0.3)
partner_counts <- rbind(
  pairs_30[, .(ego = worker_a, alter = worker_b)],
  pairs_30[, .(ego = worker_b, alter = worker_a)]
)[, .(n_copy_partners = uniqueN(alter)), by = ego]
setnames(partner_counts, "ego", "worker")

# Merge everything
profile_reuse <- sentence_counts %>%
  left_join(as_tibble(reuse_30),  by = "worker") %>%
  left_join(as_tibble(reuse_50),  by = "worker") %>%
  left_join(as_tibble(reuse_100), by = "worker") %>%
  left_join(as_tibble(partner_counts), by = "worker") %>%
  replace_na(list(
    n_sentences_copied_30 = 0,
    n_sentences_copied_50 = 0,
    n_sentences_exact     = 0,
    n_copy_partners       = 0
  )) %>%
  mutate(
    prop_copied_30 = n_sentences_copied_30 / n_sentences,
    prop_copied_50 = n_sentences_copied_50 / n_sentences,
    prop_exact     = n_sentences_exact     / n_sentences,
    is_copier_30   = n_sentences_copied_30 > 0,
    is_copier_50   = n_sentences_copied_50 > 0,
    is_copier_exact = n_sentences_exact    > 0
  )

cat("\n--- Profile-level reuse summary ---\n")
cat("Total profiles with >= 1 sentence:", nrow(profile_reuse), "\n\n")

cat("Proportion of profiles with >= 1 copied sentence:\n")
for (col in c("is_copier_30", "is_copier_50", "is_copier_exact")) {
  pct <- round(100 * mean(profile_reuse[[col]]), 1)
  cat(sprintf("  %s : %s%%\n", col, pct))
}

cat("\nprop_copied_30 distribution (among profiles with >= 1 sentence):\n")
print(quantile(profile_reuse$prop_copied_30, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)))

cat("\nprop_exact distribution:\n")
print(quantile(profile_reuse$prop_exact, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)))

cat("\nn_copy_partners distribution:\n")
print(quantile(profile_reuse$n_copy_partners, probs = c(0, 0.5, 0.75, 0.9, 0.95, 0.99, 1)))

# =============================================================================
# 5. ATTACH PROFILE COVARIATES TO REUSE STATS
# =============================================================================

cat("\nAttaching profile covariates...\n")

profiles_clean <- dpkw_data[, .(
  worker_id,
  country,
  primary_occupation,
  totalHours,
  totalJobsWorked,
  rating,
  hourlyRate,
  has_job,
  created_at = created_at_p,
  updated_at = updated_at_p
)]

profile_reuse_full <- profile_reuse %>%
  left_join(as_tibble(profiles_clean), by = c("worker" = "worker_id"))

# =============================================================================
# 6. SAVE OUTPUTS
# =============================================================================

cat("\nSaving outputs...\n")

saveRDS(sentences_df,       "/home/rstudio/data/sentences.rds")
saveRDS(pairs_30,           "/home/rstudio/data/pairs_workers_30.rds")
saveRDS(profile_reuse_full, "/home/rstudio/data/profile_reuse_stats.rds")

fwrite(profile_reuse_full, "/home/rstudio/data/profile_reuse_stats.csv")

cat("Saved:\n")
cat("  data/sentences.rds           — sentence-to-worker mapping\n")
cat("  data/pairs_workers_30.rds    — cross-worker pairs at J >= 0.3\n")
cat("  data/profile_reuse_stats.rds — per-worker reuse statistics with covariates\n")
cat("  data/profile_reuse_stats.csv — same as CSV\n\n")

cat("=============================================================\n")
cat("01-BUILD-PAIRS COMPLETE\n")
cat("=============================================================\n")
