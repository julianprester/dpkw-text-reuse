set.seed(123)

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

cat("=============================================================\n")
cat("02-BOILERPLATE: Sentence frequency, boilerplate ID,\n")
cat("                community-level prevalence\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. LOAD INTERMEDIATE DATA
# =============================================================================

cat("Loading intermediate data...\n")
sentences_df   <- readRDS("/home/rstudio/data/sentences.rds")
profile_reuse  <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")

cat("Sentences loaded:", nrow(sentences_df), "\n")
cat("Profiles loaded: ", nrow(profile_reuse), "\n\n")

n_total_workers <- n_distinct(sentences_df$worker)

# =============================================================================
# 2. SENTENCE FREQUENCY ANALYSIS
#    For each unique sentence text, how many distinct workers have it?
# =============================================================================

cat("--- 2. SENTENCE FREQUENCY ANALYSIS ---\n\n")

sentence_freq <- as.data.table(sentences_df)[
  , .(n_workers = uniqueN(worker)), by = sentence_text
][order(-n_workers)]

cat("Total unique sentence texts:", nrow(sentence_freq), "\n")
cat("Total workers:               ", n_total_workers, "\n\n")

# Distribution of sentence sharing
breaks <- c(1, 2, 6, 21, 51, 101, 501, 1001, Inf)
labels <- c("1 (unique)", "2-5", "6-20", "21-50", "51-100", "101-500", "501-1000", "1001+")
sentence_freq[, freq_band := cut(n_workers, breaks = breaks, labels = labels,
                                  right = FALSE, include.lowest = TRUE)]

cat("Distribution of sentence sharing:\n")
freq_dist <- sentence_freq[, .(
  n_sentences = .N,
  pct_of_sentences = round(100 * .N / nrow(sentence_freq), 2)
), by = freq_band][order(freq_band)]
print(freq_dist)
cat("\n")

# How many unique sentences cover what fraction of worker-sentence instances?
cat("Cumulative reach of most-shared sentences:\n")
sentence_freq[, instances := n_workers]  # each sentence contributes n_workers instances
thresholds_top <- c(10, 50, 100, 500, 1000, 5000, 10000)
total_instances <- sum(sentence_freq$n_workers)
for (k in thresholds_top) {
  top_k    <- sentence_freq[1:min(k, .N)]
  reach    <- sum(top_k$n_workers)
  pct      <- round(100 * reach / total_instances, 2)
  cat(sprintf("  Top %6s sentences cover %s worker-sentence instances (%s%% of all)\n",
              format(k, big.mark = ","), format(reach, big.mark = ","), pct))
}
cat("\n")

# =============================================================================
# 3. BOILERPLATE CANDIDATES
#    Threshold: sentences appearing in >= 50 distinct workers (> 0.014% of corpus)
#    Research-plan threshold equivalent: >= 0.5% -> 1765+ workers (also show this tier)
# =============================================================================

cat("--- 3. BOILERPLATE CANDIDATES ---\n\n")

boilerplate_50   <- sentence_freq[n_workers >= 50]
boilerplate_500  <- sentence_freq[n_workers >= 500]
boilerplate_1pct <- sentence_freq[n_workers >= round(0.01 * n_total_workers)]

cat("Sentences in >= 50 workers:   ", nrow(boilerplate_50),   "\n")
cat("Sentences in >= 500 workers:  ", nrow(boilerplate_500),  "\n")
cat("Sentences in >= 1% of workers (>=", round(0.01 * n_total_workers), "):",
    nrow(boilerplate_1pct), "\n\n")

cat("Top 100 most widely shared sentences:\n")
cat("(n_workers = distinct workers with this exact sentence)\n\n")

top100 <- sentence_freq[1:min(100, .N)]
for (i in seq_len(nrow(top100))) {
  s   <- top100$sentence_text[i]
  n   <- top100$n_workers[i]
  pct <- round(100 * n / n_total_workers, 2)
  # Truncate long sentences for display
  display <- if (nchar(s) > 120) paste0(substr(s, 1, 117), "...") else s
  cat(sprintf("[%3d] (n=%6s, %.2f%%) %s\n", i, format(n, big.mark = ","), pct, display))
}
cat("\n")

# =============================================================================
# 4. RHETORICAL CATEGORY TAGGING (keyword-based; flags for close reading)
#    Categories from the research plan:
#    A = Professional identity assertion
#    B = Service catalogue / skills list
#    C = Client orientation / call to action
#    D = Credibility anchor (experience claim)
#    E = Contact / availability statement
#    F = Platform-specific (Upwork mentions)
#    ? = Unclassified
# =============================================================================

cat("--- 4. RHETORICAL CATEGORY TAGGING (top 200 sentences) ---\n\n")

top200 <- sentence_freq[1:min(200, .N), .(sentence_text, n_workers)]

tag_sentence <- function(s) {
  s <- tolower(s)
  tags <- character(0)

  # A: Professional identity assertion
  if (str_detect(s, "dedicated|hard.?working|hardworking|passionate|committed|motivated|professional|reliable|honest|trustworthy|diligent")) tags <- c(tags, "A:identity")

  # B: Service catalogue
  if (str_detect(s, "\\b(logo|web|seo|wordpress|graphic|content|data entry|virtual assistant|video|photo|edit|design|develop|crm|excel|python|javascript|php)")) tags <- c(tags, "B:service")

  # C: Client orientation / call to action
  if (str_detect(s, "look forward|happy to help|feel free|contact me|reach out|let.?s (work|discuss|talk)|hire me|client.{0,20}satisfaction|your (success|goal|project|business)")) tags <- c(tags, "C:client")

  # D: Credibility anchor
  if (str_detect(s, "years? of experience|years? in|\\d+ years?|proven track record|strong background|extensive experience")) tags <- c(tags, "D:credibility")

  # E: Contact / availability
  if (str_detect(s, "available|respond|message|inbox|24.?7|quickly|asap|prompt|reply|get in touch")) tags <- c(tags, "E:availability")

  # F: Platform / Upwork specific
  if (str_detect(s, "upwork|top.?rated|job success|feedback|review|bid|proposal")) tags <- c(tags, "F:platform")

  if (length(tags) == 0) tags <- "?:unclassified"
  paste(tags, collapse = "|")
}

top200[, rhetorical_tags := sapply(sentence_text, tag_sentence)]

cat("Tag distribution across top 200 sentences:\n")
all_tags <- unlist(str_split(top200$rhetorical_tags, "\\|"))
tag_counts <- sort(table(all_tags), decreasing = TRUE)
print(tag_counts)
cat("\n")

cat("Sample sentences per rhetorical category:\n\n")
for (tag_prefix in c("A:identity", "B:service", "C:client", "D:credibility", "E:availability", "F:platform", "?:unclassified")) {
  examples <- top200[str_detect(rhetorical_tags, fixed(tag_prefix)) & !is.na(sentence_text)][1:min(5, .N)]
  if (nrow(examples) > 0) {
    cat(sprintf("  [%s]\n", tag_prefix))
    for (j in seq_len(nrow(examples))) {
      s   <- examples$sentence_text[j]
      n   <- examples$n_workers[j]
      if (is.na(s)) next
      display <- if (nchar(s) > 100) paste0(substr(s, 1, 97), "...") else s
      cat(sprintf("    n=%s | %s\n", format(n, big.mark = ","), display))
    }
    cat("\n")
  }
}

# =============================================================================
# 5. COMMUNITY-LEVEL PREVALENCE
# =============================================================================

cat("=============================================================\n")
cat("--- 5. COMMUNITY-LEVEL PREVALENCE ---\n\n")

prs <- as.data.table(profile_reuse)

# ---- 5a. By country (min 300 workers) ----
cat("5a. Copying rates by country (min 300 workers with sentences):\n\n")

by_country <- prs[!is.na(country), .(
  n_workers         = .N,
  pct_copier_30     = round(100 * mean(is_copier_30),    1),
  pct_copier_exact  = round(100 * mean(is_copier_exact), 1),
  mean_prop_copied_30 = round(mean(prop_copied_30), 3),
  median_prop_copied_30 = round(median(prop_copied_30), 3)
), by = country][n_workers >= 300][order(-pct_copier_30)]

cat("Countries ranked by % profiles with >= 1 copied sentence (J>=0.3):\n")
print(by_country, nrows = 40)
cat("\n")

# ---- 5b. By primary occupation ----
cat("5b. Copying rates by primary occupation:\n\n")

by_occ <- prs[!is.na(primary_occupation), .(
  n_workers           = .N,
  pct_copier_30       = round(100 * mean(is_copier_30),    1),
  pct_copier_exact    = round(100 * mean(is_copier_exact), 1),
  mean_prop_copied_30 = round(mean(prop_copied_30), 3),
  median_prop_copied_30 = round(median(prop_copied_30), 3)
), by = primary_occupation][order(-pct_copier_30)]

print(by_occ, nrows = 20)
cat("\n")

# ---- 5c. Cross-tabulation: top 10 countries × occupation ----
cat("5c. Cross-tab: pct_copier_30 for top 10 countries × occupation (min 50 workers per cell):\n\n")

top10_countries <- by_country[1:10, country]

crosstab <- prs[country %in% top10_countries & !is.na(primary_occupation), .(
  n_workers     = .N,
  pct_copier_30 = round(100 * mean(is_copier_30), 1)
), by = .(country, primary_occupation)][n_workers >= 50]

# Pivot wide
crosstab_wide <- dcast(crosstab, country ~ primary_occupation,
                       value.var = "pct_copier_30", fill = NA)
print(crosstab_wide)
cat("\n")

# ---- 5d. Proportion of sentences that are exact copies, by occupation ----
cat("5d. Proportion of sentences that are exact copies, by occupation:\n")
exact_by_occ <- prs[!is.na(primary_occupation), .(
  mean_prop_exact   = round(mean(prop_exact), 3),
  median_prop_exact = round(median(prop_exact), 3),
  pct_any_exact     = round(100 * mean(is_copier_exact), 1)
), by = primary_occupation][order(-mean_prop_exact)]
print(exact_by_occ)
cat("\n")

# =============================================================================
# 6. SAVE OUTPUTS
# =============================================================================

cat("=============================================================\n")
cat("Saving outputs...\n")

fwrite(sentence_freq,       "/home/rstudio/data/sentence_frequencies.csv")
fwrite(boilerplate_50,      "/home/rstudio/data/boilerplate_candidates.csv")
fwrite(top200,              "/home/rstudio/data/top200_sentences_tagged.csv")
fwrite(by_country,          "/home/rstudio/data/prevalence_by_country.csv")
fwrite(by_occ,              "/home/rstudio/data/prevalence_by_occupation.csv")
fwrite(crosstab_wide,       "/home/rstudio/data/prevalence_crosstab.csv")

cat("Saved:\n")
cat("  data/sentence_frequencies.csv      — all unique sentences with worker count\n")
cat("  data/boilerplate_candidates.csv    — sentences in >= 50 workers\n")
cat("  data/top200_sentences_tagged.csv   — top 200 with rhetorical tags\n")
cat("  data/prevalence_by_country.csv     — copying rates by country\n")
cat("  data/prevalence_by_occupation.csv  — copying rates by occupation\n")
cat("  data/prevalence_crosstab.csv       — country × occupation cross-tab\n\n")

cat("=============================================================\n")
cat("02-BOILERPLATE COMPLETE\n")
cat("=============================================================\n")
