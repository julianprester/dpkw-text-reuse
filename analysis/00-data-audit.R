set.seed(123)

library(data.table)
library(stringr)
library(lubridate)

cat("=============================================================\n")
cat("DATA AUDIT\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. PROFILES.CSV
# =============================================================================

cat("--- 1. PROFILES.CSV ---\n\n")

profiles <- fread("/home/rstudio/data/profiles.csv", na.strings = c("", "NA"))

cat("Dimensions:", nrow(profiles), "rows x", ncol(profiles), "cols\n")
cat("Columns:", paste(names(profiles), collapse = ", "), "\n\n")

# Missing values
cat("Missing values per column:\n")
na_counts <- sapply(profiles, function(x) sum(is.na(x)))
print(na_counts[na_counts > 0])
cat("\n")

# Description length distribution
profiles[, desc_words := str_count(description, "\\S+")]
cat("Description word count distribution:\n")
print(quantile(profiles$desc_words, probs = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1), na.rm = TRUE))
cat("Profiles with no description (NA or empty):", sum(is.na(profiles$description)), "\n")
cat("Profiles with < 50 words:", sum(profiles$desc_words < 50, na.rm = TRUE), "\n")
cat("Profiles with >= 50 words:", sum(profiles$desc_words >= 50, na.rm = TRUE), "\n\n")

# Country distribution
cat("Top 25 countries:\n")
print(profiles[, .N, by = country][order(-N)][1:25])
cat("\n")

# Occupations field — inspect format
cat("Occupations field — first 10 non-NA values:\n")
occ_sample <- profiles[!is.na(occupations), occupations][1:10]
for (i in seq_along(occ_sample)) cat(" [", i, "]", occ_sample[i], "\n")
cat("\nUnique occupation values (first 30):\n")
unique_occ <- unique(profiles[!is.na(occupations), occupations])
print(head(sort(unique_occ), 30))
cat("Total unique occupation values:", length(unique_occ), "\n\n")

# Date coverage
profiles[, created_at_parsed := ymd_hms(created_at, quiet = TRUE)]
cat("Account creation date range:\n")
cat(" Min:", format(min(profiles$created_at_parsed, na.rm = TRUE)), "\n")
cat(" Max:", format(max(profiles$created_at_parsed, na.rm = TRUE)), "\n")
cat(" NAs:", sum(is.na(profiles$created_at_parsed)), "\n\n")

# Rating distribution
cat("Rating distribution:\n")
print(quantile(profiles$rating, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
cat("Profiles with no rating (NA):", sum(is.na(profiles$rating)), "\n")
cat("Profiles with rating = 0:", sum(profiles$rating == 0, na.rm = TRUE), "\n\n")

# Hourly rate distribution
cat("Hourly rate distribution (USD):\n")
print(quantile(profiles$hourlyRate, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
cat("Profiles with no hourly rate (NA):", sum(is.na(profiles$hourlyRate)), "\n\n")

# =============================================================================
# 2. SIMILAR_PAIRS.CSV — SCORE DISTRIBUTION (no join needed)
# =============================================================================

cat("=============================================================\n")
cat("--- 2. SIMILAR_PAIRS.CSV ---\n\n")

cat("Loading similar_pairs.csv (this may take a moment)...\n")
pairs <- fread("/home/rstudio/data/similar_pairs.csv")

cat("Dimensions:", nrow(pairs), "rows x", ncol(pairs), "cols\n")
cat("Columns:", paste(names(pairs), collapse = ", "), "\n\n")

cat("Score distribution (Jaccard similarity):\n")
print(quantile(pairs$score, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)))
cat("Mean:", mean(pairs$score), "\n\n")

cat("Pair counts at different Jaccard thresholds:\n")
thresholds <- c(0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
for (t in thresholds) {
  n <- sum(pairs$score >= t)
  pct <- round(100 * n / nrow(pairs), 1)
  cat(sprintf("  J >= %.2f : %10s pairs  (%s%%)\n", t, format(n, big.mark = ","), pct))
}
cat("\n")

# Count exact matches (J == 1.0)
exact <- sum(pairs$score == 1.0)
cat("Exact matches (J = 1.0):", format(exact, big.mark = ","), "\n\n")

# =============================================================================
# 3. SENTENCE-LEVEL STATS (reconstruct sentences_df from profiles)
# =============================================================================

cat("=============================================================\n")
cat("--- 3. SENTENCE-LEVEL STATS ---\n\n")

cat("Note: Sentence-level stats require tokenising profiles — skipping full tokenisation.\n")
cat("Instead, estimating from profile description lengths.\n\n")

# Average sentences per profile (rough estimate: 1 sentence per ~15 words)
profiles[, est_sentences := pmax(1, round(desc_words / 15))]
cat("Estimated sentences per profile (desc words / 15):\n")
print(quantile(profiles$est_sentences, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1), na.rm = TRUE))
cat("\n")

# Unique sentence IDs in pairs
cat("Unique sentence IDs appearing in similar_pairs:\n")
unique_sents <- length(unique(c(pairs$a, pairs$b)))
cat(" ", format(unique_sents, big.mark = ","), "unique sentences\n\n")

cat("=============================================================\n")
cat("AUDIT COMPLETE\n")
cat("=============================================================\n")
