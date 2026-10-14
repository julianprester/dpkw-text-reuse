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

# ================================

# LOAD DATA

dpkw_data <- read.csv("data/profiles.csv")

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

corpus_data_df <- sentences_df #%>% sample_n(10000)

minhash <- minhash_generator(n = 240, seed = 456) 

texts <- corpus_data_df$sentence_text
names(texts) <- corpus_data_df$sentence_id

corpus <- TextReuseCorpus(
  text = texts,
  tokenizer = textreuse::tokenize_ngrams,
  n = 5,
  lowercase = FALSE,
  minhash_func = minhash,
  keep_tokens = FALSE,
  progress = TRUE
)

buckets <- lsh(corpus, bands = 60, progress = TRUE)
candidates <- lsh_candidates(buckets)
similar_pairs <- lsh_compare(candidates, corpus, jaccard_similarity, progress = TRUE)

write.csv(similar_pairs, "similar_pairs.csv")