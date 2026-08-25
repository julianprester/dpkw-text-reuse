set.seed(123)

library(dplyr)
library(tidyr)
library(tibble)
library(tidyverse)
library(tidytext)
library(stringr)
library(textreuse)
library(tokenizers)
library(readr)
library(purrr)
library(parallel)
library(data.table)
library(lubridate)

# ================================

# LOAD DATA

dpkw_data <- read.csv("profiles.csv")

dpkw_df <- tibble(
  worker = dpkw_data$worker_id,
  description = dpkw_data$description
)

dpkw_tidy <-
  dpkw_df %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)

dpkw_covariates <-
  dpkw_tidy %>%
  distinct(
    worker
  ) %>%
  replace_na(list(
    worker = ""
  ))

analysis_df <-
  dpkw_covariates %>%
  inner_join(dpkw_df)

rm(dpkw_tidy, dpkw_df, dpkw_data, dpkw_covariates)

# ==================================

# CLEAN TEXT

analysis_df <- analysis_df %>%
  mutate(
    # Standardize newlines first, then handle whitespace
    clean_text = str_replace_all(description, "\\r\\n|\\r|\\n", "\n"),
    clean_text = str_replace_all(clean_text, "[\\s\t]+", " "), # Replace tabs/multiple spaces with single space
    clean_text = str_trim(clean_text)
  )

sentences_df <- analysis_df %>%
  select(worker, clean_text) %>%
  # Use map from purrr to apply tokenizer safely to each text entry
  mutate(sentences = map(clean_text, ~tokenize_sentences(., simplify = TRUE))) %>%
  # Unnest the list of sentences
  unnest(sentences) %>%
  # Filter by word count instead of character count
  mutate(
    word_count = str_count(sentences, "\\S+")  # Count non-whitespace sequences (words)
  ) %>%
  filter(
    !is.na(sentences),
    word_count >= 8,      # Minimum 8 words (adjust as needed)
    word_count <= 50     # Maximum 100 words (optional - removes very long sentences)
  ) %>%
  # Create a globally unique ID for each sentence
  rowid_to_column("unique_sentence_row_id") %>% # Add a unique row ID first
  mutate(sentence_id = paste0("sent_", unique_sentence_row_id)) %>% # Create unique sentence ID
  select(sentence_id, worker, sentence_text = sentences) %>%
  mutate(
    sentence_text = str_to_lower(sentence_text)
  )

similar_pairs <- read.csv("similar_pairs.csv")

full_df <- similar_pairs %>%
  inner_join(sentences_df, by = join_by(a == sentence_id)) %>%
  rename(worker_a = worker, sentence_text_a = sentence_text) %>%
  inner_join(sentences_df, by = join_by(b == sentence_id)) %>%
  rename(worker_b = worker, sentence_text_b = sentence_text) %>%
  inner_join(analysis_df, by = join_by(worker_a == worker)) %>%
  rename(description_a = description) %>%
  select(-clean_text) %>%
  inner_join(analysis_df, by = join_by(worker_b == worker)) %>%
  rename(description_b = description) %>%
  select(-clean_text)

profiles <- read.csv("profiles.csv")

# =============================================================================
# 2. APPLY SIMILARITY THRESHOLD
# =============================================================================

cat("\nApplying similarity threshold...\n")

similarity_threshold <- 0.3

# full_df already has: a, b, score, worker_a, sentence_text_a, worker_b, sentence_text_b, description_a, description_b
filtered_pairs <- full_df %>%
  filter(score >= similarity_threshold)
cat("Similar pairs (Jaccard >= 0.3):", nrow(filtered_pairs), "\n")

# Remove self-pairs (same worker copying themselves)
filtered_pairs <- filtered_pairs %>%
  filter(worker_a != worker_b)
cat("Pairs between different workers:", nrow(filtered_pairs), "\n")

# =============================================================================
# 3. JOIN WITH PROFILE DATES AND COVARIATES
# =============================================================================

cat("\nJoining with profile information...\n")

# Prepare profile data with parsed dates
profiles_clean <- profiles %>%
  select(worker_id, created_at, updated_at, totalHours, totalJobsWorked,
         rating, hourlyRate, country, occupations) %>%
  mutate(
    # Parse ISO 8601 dates (e.g., "2011-04-24T08:23:53.000Z")
    created_at = ymd_hms(created_at, quiet = TRUE),
    updated_at = ymd_hms(updated_at, quiet = TRUE),
    # Convert numeric fields
    totalHours = as.numeric(totalHours),
    totalJobsWorked = as.numeric(totalJobsWorked),
    rating = as.numeric(rating),
    hourlyRate = as.numeric(hourlyRate)
  )

# Check date parsing
cat("\nDate parsing check:\n")
cat("created_at NAs:", sum(is.na(profiles_clean$created_at)), "of", nrow(profiles_clean), "\n")
cat("updated_at NAs:", sum(is.na(profiles_clean$updated_at)), "of", nrow(profiles_clean), "\n")
cat("Sample dates:\n")
print(head(profiles_clean %>% select(worker_id, created_at, updated_at)))

# Create renamed versions for joining
profiles_a <- profiles_clean %>%
  rename(
    created_at_a = created_at,
    updated_at_a = updated_at,
    totalHours_a = totalHours,
    totalJobsWorked_a = totalJobsWorked,
    rating_a = rating,
    hourlyRate_a = hourlyRate,
    country_a = country,
    occupations_a = occupations
  )

profiles_b <- profiles_clean %>%
  rename(
    created_at_b = created_at,
    updated_at_b = updated_at,
    totalHours_b = totalHours,
    totalJobsWorked_b = totalJobsWorked,
    rating_b = rating,
    hourlyRate_b = hourlyRate,
    country_b = country,
    occupations_b = occupations
  )

# Join profile info for worker_a and worker_b
pairs_full <- filtered_pairs %>%
  left_join(profiles_a, by = c("worker_a" = "worker_id")) %>%
  left_join(profiles_b, by = c("worker_b" = "worker_id"))

cat("\nPairs with full profile info:", nrow(pairs_full), "\n")

# Clean up memory
rm(full_df, filtered_pairs, profiles_a, profiles_b)
gc()
