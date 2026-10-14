# 01_corpus.R -- Rebuild the sentence corpus exactly as in analysis/text-reuse.R
# (so that sentence ids match data/similar_pairs.csv), and add what the original
# pipeline discarded: each sentence's position in its profile, plus profile-,
# title- and metadata-level covariates.
#
# Output: data/derived/workers.parquet, data/derived/sentences.parquet

source("analysis/00_setup.R")
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(tibble); library(tidytext); library(tokenizers) })
set.seed(123)
t0 <- Sys.time()

# ---- 1. Worker filter, verbatim from text-reuse.R -------------------------------
dpkw_data <- read.csv("data/profiles.csv")
rec("profiles_raw", nrow(dpkw_data), "rows in data/profiles.csv (unique worker ids)")
dpkw_df <- tibble(worker = dpkw_data$worker_id, description = dpkw_data$description)
keep <- dpkw_df %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords(), by = "word") %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  distinct(worker) %>%
  replace_na(list(worker = ""))
analysis_df <- keep %>% inner_join(dpkw_df, by = "worker") %>% as.data.table()
rec("profiles_filtered", nrow(analysis_df), "profiles with >=1 non-stopword, non-numeric word that occurs >100 times in corpus")
analysis_df[, wid := .I]
analysis_df[, clean_text := str_trim(str_replace_all(str_replace_all(description, "\\r\\n|\\r|\\n", "\n"), "[\\s\t]+", " "))]

# ---- 2. Sentences with positions (same order and filter as text-reuse.R) ----------
toks <- tokenize_sentences(analysis_df$clean_text)
S <- data.table(wid = rep(analysis_df$wid, lengths(toks)), sentence = unlist(toks, use.names = FALSE))
S[, pos_all := seq_len(.N), by = wid]
S[, n_all := .N, by = wid]
S[, nwords := str_count(sentence, "\\S+")]
S[, nchar_s := nchar(sentence)]
S[, offset := cumsum(nchar_s + 1L) - (nchar_s + 1L), by = wid]   # character offset of the sentence start in clean_text (approx.)
chars_all <- S[, .(chars_all_sent = sum(nchar_s)), by = wid]
S <- S[!is.na(sentence) & nwords >= 8 & nwords <= 50]
S[, sid := .I]                              # == unique_sentence_row_id in text-reuse.R
S[, pos_c := seq_len(.N), by = wid]
S[, n_c := .N, by = wid]
S[, text := str_to_lower(sentence)]
rec("sentences", nrow(S), "codable sentences (8-50 words)")
rec("workers_with_sentences", uniqueN(S$wid), "workers with >=1 codable sentence (reuse frame)")

# ---- 3. Profile metadata --------------------------------------------------------
W <- merge(analysis_df[, .(wid, worker, description, clean_text)],
           as.data.table(dpkw_data)[, -"description"], by.x = "worker", by.y = "worker_id", all.x = TRUE, sort = FALSE)
setorder(W, wid)
occ <- str_match_all(W$occupations, "'([^']+)'")
occ <- lapply(occ, function(m) if (nrow(m)) m[, 2] else character(0))
W[, `:=`(occ1 = vapply(occ, function(v) if (length(v)) v[1] else NA_character_, ""),
         n_occ = lengths(occ),
         occ_all = vapply(occ, paste, "", collapse = "|"))]
W[country == "", country := NA][city == "", city := NA]
W[, city_key := ifelse(is.na(city) | is.na(country), NA, paste(country, str_to_lower(str_squish(city)), sep = "::"))]
W[, agency := agency == "True"]
W[, `:=`(cr = ts_parse(created_at), up = ts_parse(updated_at))]
REF <- max(W$up, na.rm = TRUE)                # latest profile edit observed: lower bound of scrape date
rec("reference_date", as.numeric(format(REF, "%Y%m%d")), "max(updated_at) = latest observed profile edit; tenure is measured to this date")
W[, tenure_yrs := as.numeric(difftime(REF, cr, units = "days")) / 365.25]
W[, edit_gap_days := as.numeric(difftime(up, cr, units = "days"))]
W[, since_edit_days := as.numeric(difftime(REF, up, units = "days"))]
W[, text_dated := !is.na(edit_gap_days) & edit_gap_days <= 1]   # never edited after day of creation
W[, cohort_year := as.integer(format(cr, "%Y"))]
W[, lrate := fifelse(hourlyRate >= 3 & hourlyRate <= 500, log(hourlyRate), NA_real_)]
W[, `:=`(ljobs = log1p(totalJobsWorked), lhours = log1p(totalHours))]
W[, rated := rating > 0]
W[, hours_per_job := fifelse(totalJobsWorked > 0, totalHours / totalJobsWorked, NA_real_)]

# ---- 4. Description and title features ------------------------------------------
lw <- str_to_lower(W$description)
lines <- str_split(W$description, "\n")
W[, `:=`(desc_chars = nchar(description),
         desc_words = str_count(description, "\\S+"),
         n_lines = lengths(lines),
         n_short_lines = vapply(lines, function(x) sum(str_count(x, "\\S+") %in% 1:8), 0L),
         caps_words = str_count(description, "\\b[A-Z]{3,}\\b"),
         we_voice = str_count(lw, "\\b(we|our|us)\\b"),
         i_voice = str_count(lw, "\\b(i|my|me|i'm|i've)\\b"),
         years_claim = str_detect(lw, "\\b\\d{1,2}\\s*\\+?\\s*(years?|yrs)\\b"),
         mentions_upwork = str_detect(lw, "upwork"),
         exclam = str_count(description, "!"),
         has_url = str_detect(lw, "https?://|www\\."))]
W <- merge(W, chars_all, by = "wid", all.x = TRUE)
W <- merge(W, S[, .(n_c = .N, chars_c = sum(nchar_s)), by = wid], by = "wid", all.x = TRUE)
W[is.na(n_c), `:=`(n_c = 0L, chars_c = 0L)]
W[, n_all := lengths(toks)]
W[, codable_char_share := fifelse(desc_chars > 0, chars_c / nchar(clean_text), NA_real_)]
W[, title_l := str_squish(str_to_lower(title))]
W[, `:=`(title_words = str_count(title, "\\S+"), title_segments = str_count(title, "\\||,|/| - ") + 1L)]
W[, title_dups := .N, by = title_l]
W[is.na(title_l) | title_l == "", title_dups := NA_integer_]

# ---- 5. Save ----------------------------------------------------------------------
W[, `:=`(description = NULL, clean_text = NULL)]
write_parquet(W, file.path(DERIVED, "workers.parquet"))
write_parquet(S[, .(sid, wid, pos_all, n_all, pos_c, n_c, offset, nwords, text)], file.path(DERIVED, "sentences.parquet"))
write_parquet(data.table(wid = analysis_df$wid, description = analysis_df$description), file.path(DERIVED, "descriptions.parquet"))

rec("text_dated_profiles", W[n_c > 0 & text_dated == TRUE, .N], "reuse-frame profiles never edited after the day of creation")
rec("agency_share", W[n_c > 0, mean(agency, na.rm = TRUE)], "share of reuse-frame profiles flagged as agency members")
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
save_numbers("01_corpus")
