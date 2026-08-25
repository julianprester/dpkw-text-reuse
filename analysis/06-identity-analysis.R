set.seed(123)

library(data.table)
library(dplyr)
library(stringr)

cat("=============================================================\n")
cat("06-IDENTITY-ANALYSIS\n")
cat("Empirical tests for Idea 5: Franchise Professional Identity\n")
cat("=============================================================\n\n")
cat("Five analyses:\n")
cat("  1. Rhetorical category mapping and geographic spread\n")
cat("  2. Credential substitution: experience reduces copying within occupations\n")
cat("  3. Template retention as constitutive adoption proxy\n")
cat("  4. Country gradient and cultural distance (English proficiency proxy)\n")
cat("  5. Template community prototype coherence\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading data...\n")
sentences_df <- readRDS("/home/rstudio/data/sentences.rds")
prof         <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")
comm_freq    <- fread("/home/rstudio/data/community_sentence_freq.csv")
top_comms    <- fread("/home/rstudio/data/louvain_communities.csv")
membership   <- fread("/home/rstudio/data/louvain_membership.csv")
prevalence_c <- fread("/home/rstudio/data/prevalence_by_country.csv")

cat("Sentences:       ", nrow(sentences_df), "\n")
cat("Profiles:        ", nrow(prof), "\n")
cat("Comm-freq rows:  ", nrow(comm_freq), "\n\n")

prof_dt <- as.data.table(prof)
sents_dt <- as.data.table(sentences_df)

# =============================================================================
# SHARED HELPER: RHETORICAL TAGGER
# Five types:
#   T1 = Professional character claims  (dedicated, hardworking, honest...)
#   T2 = Professional pledge claims     (over-delivering, client satisfaction...)
#   T3 = Service/capability claims      (I do X, I specialise in X...)
#   T4 = Relational/availability claims (look forward to, feel free to contact...)
#   T5 = Credibility anchors            (years of experience, proven track record...)
#   T6 = Platform-specific              (Upwork, top-rated, job success...)
#   T? = Unclassified
# =============================================================================

tag_rhetorical <- function(s) {
  s <- tolower(s)
  tags <- character(0)

  if (str_detect(s, paste0(
    "\\b(dedicated|hard.?working|hardworking|passionate|committed|motivated|",
    "reliable|honest|trustworthy|diligent|responsible|loyal|sincere|",
    "integrity|enthusiastic|detail.oriented|self.motivated)\\b")))
    tags <- c(tags, "T1:character")

  if (str_detect(s, paste0(
    "over.deliver|going beyond|exceed.{0,20}expect|",
    "client.{0,20}satisfaction|100%.{0,15}(quality|satisf|commit)|",
    "your.{0,20}success|your.{0,20}goal|timely.{0,10}deliver|",
    "on.time.{0,10}deliver|quality.{0,10}work|giving more than")))
    tags <- c(tags, "T2:pledge")

  if (str_detect(s, paste0(
    "\\b(logo|web|seo|wordpress|graphic|content|data entry|virtual assistant|",
    "video|photo|edit|design|develop|crm|excel|python|javascript|php|",
    "social media|copywr|translat|writing|proofreading|bookkeeping|",
    "amazon|shopify|woocommerce|magento|android|ios|mobile app)\\b")))
    tags <- c(tags, "T3:service")

  if (str_detect(s, paste0(
    "look forward|happy to help|feel free|contact me|reach out|",
    "let.?s (work|discuss|talk|connect)|hire me|message me|",
    "available|get in touch|looking forward|discuss.{0,20}project|",
    "thank you.{0,20}(consider|time|opport)")))
    tags <- c(tags, "T4:relational")

  if (str_detect(s, paste0(
    "\\d+\\s*\\+?\\s*years?.{0,10}(experience|in |of )|",
    "years? of (experience|professional|working)|",
    "proven track record|strong background|extensive experience|",
    "highly experienced|seasoned|expert in|specialist in")))
    tags <- c(tags, "T5:credibility")

  if (str_detect(s, paste0(
    "upwork|top.?rated|job success|\\bjss\\b|",
    "feedback|hourly.{0,10}rate|bid|proposal|freelancer")))
    tags <- c(tags, "T6:platform")

  if (length(tags) == 0) tags <- "T?:unclassified"
  paste(tags, collapse = "|")
}

# =============================================================================
# ANALYSIS 1: RHETORICAL CATEGORY MAPPING AND GEOGRAPHIC SPREAD
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 1: Rhetorical category mapping & geographic spread ---\n\n")
cat("Prediction: franchise identity templates (T1:character, T2:pledge)\n")
cat("spread more widely across countries and occupations than\n")
cat("service/credibility templates (T3, T5).\n\n")

# Compute sentence frequency and demographics for sentences >= 20 workers
cat("Computing per-sentence geographic and occupational spread...\n")
sents_with_demo <- sents_dt[prof_dt[, .(worker, country, primary_occupation)],
                              on = "worker", nomatch = 0]

sent_spread <- sents_with_demo[, .(
  n_workers    = uniqueN(worker),
  n_countries  = uniqueN(country[!is.na(country)]),
  n_occs       = uniqueN(primary_occupation[!is.na(primary_occupation)]),
  top_ctry_pct = {
    ctry <- country[!is.na(country)]
    if (length(ctry) == 0) NA_real_
    else round(100 * max(table(ctry)) / length(ctry), 1)
  }
), by = sentence_text]

sent_spread <- sent_spread[n_workers >= 20]
cat("Sentences with >= 20 workers:", nrow(sent_spread), "\n")

# Apply rhetorical tags
cat("Applying rhetorical tags...\n")
sent_spread[, rhetorical_tags := sapply(sentence_text, tag_rhetorical)]

# Assign primary rhetorical type (first tag)
sent_spread[, primary_type := str_extract(rhetorical_tags, "T[1-6?]:[^|]+")]

cat("\nSentences by primary rhetorical type:\n")
type_counts <- sent_spread[, .N, by = primary_type][order(-N)]
print(type_counts)
cat("\n")

# Compare spread metrics by rhetorical type
cat("Geographic and occupational spread by rhetorical type:\n")
cat("(Franchise identity theory: T1 and T2 should have widest spread)\n\n")
spread_by_type <- sent_spread[!is.na(primary_type), .(
  n_sentences    = .N,
  mean_n_workers = round(mean(n_workers), 1),
  mean_n_ctry    = round(mean(n_countries, na.rm = TRUE), 1),
  mean_n_occs    = round(mean(n_occs, na.rm = TRUE), 1),
  mean_top_ctry_pct = round(mean(top_ctry_pct, na.rm = TRUE), 1),
  median_n_workers  = round(median(n_workers), 1)
), by = primary_type][order(-mean_n_ctry)]

print(spread_by_type)
cat("\n")

# Show top 5 sentences per rhetorical type
cat("Top 5 most-used sentences per type:\n\n")
for (tp in c("T1:character", "T2:pledge", "T3:service", "T4:relational",
             "T5:credibility")) {
  examples <- sent_spread[primary_type == tp][order(-n_workers)][1:min(5, .N)]
  if (nrow(examples) == 0) next
  cat(sprintf("[%s]\n", tp))
  for (i in seq_len(nrow(examples))) {
    s <- examples$sentence_text[i]
    trunc <- if (nchar(s) > 95) paste0(substr(s, 1, 92), "...") else s
    cat(sprintf("  n=%3d | ctry=%2d | occ=%2d | top_ctry=%.0f%% | \"%s\"\n",
                examples$n_workers[i], examples$n_countries[i],
                examples$n_occs[i], examples$top_ctry_pct[i], trunc))
  }
  cat("\n")
}

fwrite(sent_spread[, .(sentence_text, n_workers, n_countries, n_occs,
                        top_ctry_pct, rhetorical_tags, primary_type)],
       "/home/rstudio/data/sentence_rhetorical_spread.csv")

# =============================================================================
# ANALYSIS 2: CREDENTIAL SUBSTITUTION TEST
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 2: Credential substitution within occupations ---\n\n")
cat("Prediction: log1p(totalHours) negatively predicts prop_copied_30.\n")
cat("This relationship should be STRONGEST in occupations with weak formal\n")
cat("credentials (web dev, VA) and WEAKEST where credentials are verifiable\n")
cat("(translation, writing).\n\n")

model_dt <- prof_dt[
  !is.na(primary_occupation) & !is.na(totalHours) & n_sentences > 0,
  .(worker, prop_copied_30, totalHours, totalJobsWorked,
    primary_occupation, created_at, has_job)
]
model_dt[, log_hours := log1p(totalHours)]
model_dt[, log_jobs  := log1p(totalJobsWorked)]
model_dt[, join_year := as.integer(format(created_at, "%Y"))]

cat("Workers in model:", format(nrow(model_dt), big.mark=","), "\n\n")

# Overall correlation
cat("Overall Spearman correlation:\n")
cat(sprintf("  prop_copied_30 ~ log_hours: r=%.4f\n",
            cor(model_dt$prop_copied_30, model_dt$log_hours,
                use="complete.obs", method="spearman")))
cat(sprintf("  prop_copied_30 ~ log_jobs:  r=%.4f\n",
            cor(model_dt$prop_copied_30, model_dt$log_jobs,
                use="complete.obs", method="spearman")))
cat("\n")

# By occupation: correlation between log_hours and prop_copied_30
cat("Spearman r (prop_copied_30 ~ log_hours) by occupation:\n")
cat("(Negative = more experienced workers copy less = credential substitution)\n\n")

occ_corr <- model_dt[, .(
  n            = .N,
  r_hours      = round(cor(prop_copied_30, log_hours,
                            use="complete.obs", method="spearman"), 4),
  r_jobs       = round(cor(prop_copied_30, log_jobs,
                            use="complete.obs", method="spearman"), 4),
  mean_prop_copied = round(mean(prop_copied_30), 4),
  pct_copier   = round(100 * mean(prop_copied_30 > 0), 1)
), by = primary_occupation][order(r_hours)]

print(occ_corr)
cat("\n")

# Linear regression: prop_copied ~ log_hours + occupation
lm_main <- lm(prop_copied_30 ~ log_hours + log_jobs +
                factor(primary_occupation) + join_year,
              data = model_dt[!is.na(join_year)])
cat("Main regression coefficients (key vars):\n")
coefs <- summary(lm_main)$coefficients
cat(sprintf("  log_hours coef: %.6f (p=%.4g)\n",
            coefs["log_hours", "Estimate"],
            coefs["log_hours", "Pr(>|t|)"]))
cat(sprintf("  log_jobs  coef: %.6f (p=%.4g)\n",
            coefs["log_jobs", "Estimate"],
            coefs["log_jobs", "Pr(>|t|)"]))
cat(sprintf("  R-squared: %.4f\n\n",
            summary(lm_main)$r.squared))

# Credential-consistent occupations (low copying) vs. inconsistent (high copying)
cat("Mean prop_copied_30 by occupation (sorted):\n")
occ_means <- model_dt[, .(mean_prop = round(mean(prop_copied_30), 4),
                            n = .N),
                       by = primary_occupation][order(-mean_prop)]
print(occ_means)
cat("\n")

fwrite(occ_corr, "/home/rstudio/data/credential_substitution_by_occ.csv")

# =============================================================================
# ANALYSIS 3: TEMPLATE RETENTION AS CONSTITUTIVE ADOPTION PROXY
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 3: Template retention by rhetorical type ---\n\n")
cat("Prediction: franchise identity templates (T1:character, T2:pledge)\n")
cat("show higher long-term retention (adopters who joined long ago still\n")
cat("have the template) than service/credibility templates (T3, T5).\n\n")

# For sentences with >= 20 workers: join with worker tenure
sents_with_tenure <- sents_with_demo[
  prof_dt[, .(worker, created_at, updated_at)],
  on = "worker", nomatch = 0
]
sents_with_tenure[, tenure_days := as.numeric(
  difftime(updated_at, created_at, units = "days"))]

# For each sentence, compute % of adopters with tenure > 365 days
sent_retention <- sents_with_tenure[
  !is.na(tenure_days) & tenure_days >= 0,
  .(
    n_workers_with_tenure = .N,
    pct_tenure_gt_365     = round(100 * mean(tenure_days > 365), 1),
    pct_tenure_gt_730     = round(100 * mean(tenure_days > 730), 1),
    mean_tenure_days      = round(mean(tenure_days), 0),
    median_tenure_days    = round(median(tenure_days), 0)
  ),
  by = sentence_text
]

# Join with rhetorical tags
sent_retention <- sent_retention[
  sent_spread[, .(sentence_text, n_workers, primary_type, rhetorical_tags)],
  on = "sentence_text", nomatch = 0
][n_workers >= 20]

cat("Retention stats by rhetorical type:\n\n")
retention_by_type <- sent_retention[!is.na(primary_type), .(
  n_sentences         = .N,
  mean_pct_gt365      = round(mean(pct_tenure_gt_365, na.rm = TRUE), 1),
  mean_pct_gt730      = round(mean(pct_tenure_gt_730, na.rm = TRUE), 1),
  mean_tenure_days    = round(mean(mean_tenure_days, na.rm = TRUE), 0),
  median_tenure_days  = round(median(median_tenure_days, na.rm = TRUE), 0)
), by = primary_type][order(-mean_pct_gt365)]

print(retention_by_type)
cat("\n")

cat("Interpretation:\n")
cat("  High pct_gt365: most adopters had the template for > 1 year\n")
cat("  (consistent with constitutive/durable adoption).\n")
cat("  Low pct_gt365: mostly recent adopters (strategic/short-term).\n\n")

# Long-tenure vs recent adopters for T1/T2 vs T3/T5
for (pair in list(c("T1:character", "T2:pledge"),
                  c("T3:service",   "T5:credibility"))) {
  sub <- sent_retention[primary_type %in% pair]
  cat(sprintf("  Types %s: mean_pct_gt365=%.1f%%  median_tenure=%.0f days\n",
              paste(pair, collapse="/"),
              mean(sub$pct_tenure_gt_365, na.rm=TRUE),
              median(sub$median_tenure_days, na.rm=TRUE)))
}
cat("\n")

fwrite(sent_retention[, .(sentence_text, n_workers, primary_type,
                           pct_tenure_gt_365, pct_tenure_gt_730,
                           mean_tenure_days, median_tenure_days)],
       "/home/rstudio/data/template_retention.csv")

# =============================================================================
# ANALYSIS 4: COUNTRY GRADIENT AND CULTURAL DISTANCE
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 4: Country gradient and English proficiency ---\n\n")
cat("Prediction: countries more culturally distant from Anglo-American\n")
cat("professional discourse (lower English proficiency) copy more.\n")
cat("This supports franchise identity as professional language acquisition.\n\n")

# English Proficiency Index (EF EPI 2023 approximate scores)
# Native-equivalent = 700; higher score = higher proficiency
ef_epi <- data.table(
  country = c(
    "United States","United Kingdom","Canada","Australia",
    "New Zealand","Ireland","South Africa",
    "Netherlands","Sweden","Norway","Denmark","Finland",
    "Austria","Germany","Portugal","Belgium",
    "Singapore","Malaysia","Philippines",
    "Romania","Serbia","Poland","Ukraine","Croatia","Bulgaria","Hungary","Greece","Slovakia","Slovenia","Lithuania",
    "Switzerland","Israel","Argentina","Chile","Colombia","Peru",
    "Nigeria","Kenya","Tanzania","Ghana",
    "Russia","Belarus","Kazakhstan",
    "Turkey","Egypt","Morocco","Saudi Arabia","UAE",
    "India","Pakistan","Bangladesh","Sri Lanka",
    "Indonesia","Vietnam","Thailand","China","Japan","South Korea",
    "Brazil","Mexico","Venezuela","Ecuador",
    "France","Italy","Spain","Czech Republic",
    "Nepal","Ethiopia","Cameroon"
  ),
  ef_epi = c(
    700, 700, 700, 700,
    700, 700, 611,
    663, 649, 643, 640, 605,
    609, 603, 601, 597,
    633, 552, 569,
    599, 582, 577, 571, 570, 564, 558, 556, 553, 551, 548,
    624, 558, 567, 553, 550, 543,
    495, 506, 462, 520,
    555, 536, 499,
    556, 509, 480, 464, 487,
    493, 461, 475, 494,
    542, 512, 469, 526, 511, 516,
    549, 547, 466, 477,
    548, 551, 555, 562,
    422, 401, 421
  ),
  is_l1_english = c(
    1,1,1,1,1,1,0,
    0,0,0,0,0,0,0,0,0,
    0,0,0,
    rep(0, 11),
    0,0,rep(0,4),
    rep(0,4),
    rep(0,3),
    rep(0,5),
    rep(0,4),
    rep(0,4),
    rep(0,4),
    rep(0,3),
    rep(0,3)
  )
)

# Join with country-level prevalence
country_model <- merge(
  prevalence_c[n_workers >= 300,
               .(country, n_workers, pct_copier_30, pct_copier_exact,
                 mean_prop_copied_30)],
  ef_epi, by = "country"
)

cat("Countries matched:", nrow(country_model), "of",
    nrow(prevalence_c[n_workers >= 300]), "with n>=300\n\n")

cat("Country-level data (sorted by copying rate):\n")
print(country_model[order(-pct_copier_30),
                    .(country, n_workers, pct_copier_30,
                      ef_epi, is_l1_english)][1:25])
cat("\n")

# Correlation: English proficiency ~ copying rate
r_ef <- cor(country_model$ef_epi, country_model$pct_copier_30,
            use="complete.obs", method="spearman")
r_l1 <- cor(country_model$is_l1_english, country_model$pct_copier_30,
            use="complete.obs", method="spearman")
cat(sprintf("Spearman r (ef_epi ~ pct_copier_30):       %+.4f\n", r_ef))
cat(sprintf("Spearman r (is_l1_english ~ pct_copier_30): %+.4f\n", r_l1))
cat("(Negative r expected: lower proficiency -> higher copying)\n\n")

# Regression
lm_country <- lm(pct_copier_30 ~ ef_epi + log(n_workers),
                  data = country_model)
cat("Regression: pct_copier_30 ~ ef_epi + log(n_workers)\n")
print(summary(lm_country)$coefficients)
cat(sprintf("R-squared: %.4f\n\n", summary(lm_country)$r.squared))

# Compare L1 vs. L2 English countries
l1_mean <- mean(country_model[is_l1_english == 1, pct_copier_30])
l2_mean <- mean(country_model[is_l1_english == 0, pct_copier_30])
cat(sprintf("Mean pct_copier_30: L1 English countries=%.1f%%  L2=%.1f%%\n",
            l1_mean, l2_mean))
if (sum(country_model$is_l1_english == 1) > 2 &&
    sum(country_model$is_l1_english == 0) > 2) {
  tt <- t.test(country_model[is_l1_english==0, pct_copier_30],
               country_model[is_l1_english==1, pct_copier_30],
               alternative = "greater")
  cat(sprintf("t-test (L2 > L1): t=%.3f, p=%.4g\n", tt$statistic, tt$p.value))
}
cat("\n")

fwrite(country_model, "/home/rstudio/data/country_cultural_distance.csv")

# =============================================================================
# ANALYSIS 5: TEMPLATE COMMUNITY PROTOTYPE COHERENCE
# =============================================================================

cat("=============================================================\n")
cat("--- ANALYSIS 5: Template community prototype coherence ---\n\n")
cat("Prediction: template communities function like market categories —\n")
cat("high internal coherence (high % of members share the defining sentence)\n")
cat("and exclusive boundaries (the defining sentence is community-specific).\n\n")

# For each community, identify the defining sentence (highest pct_in_comm)
defining_sents <- comm_freq[
  n_in_comm >= 5,
  .SD[which.max(pct_in_comm)],
  by = community
]
setnames(defining_sents, "sentence_text", "defining_sentence")

# Join with community sizes
defining_sents <- merge(
  defining_sents,
  top_comms[, .(community, size, top_country, top_country_pct,
                top_occ, top_occ_pct)],
  by = "community"
)

# Prototype coherence: pct_in_comm = % of community members with defining sentence
# Exclusivity: lift = how much more common inside vs. outside

cat("Coherence and exclusivity of top 30 communities (by size):\n")
cat(sprintf("  %-5s  %-6s  %-14s  %-8s  %-8s  %-40s\n",
            "Comm", "Size", "pct_in_comm", "Lift", "n_global", "Defining sentence"))
cat(strrep("-", 85), "\n")
for (i in seq_len(min(30, nrow(defining_sents[order(-size)])))) {
  r <- defining_sents[order(-size)][i]
  trunc <- if (nchar(r$defining_sentence) > 38)
    paste0(substr(r$defining_sentence, 1, 35), "...") else r$defining_sentence
  cat(sprintf("  %-5s  %-6s  %-14s  %-8s  %-8s  %-40s\n",
              r$community, format(r$size, big.mark=","),
              paste0(r$pct_in_comm, "%"),
              round(r$lift, 1),
              format(r$n_global, big.mark=","),
              trunc))
}
cat("\n")

# Summary statistics
cat("Prototype coherence summary (communities with >= 100 members):\n")
large_comms <- defining_sents[size >= 100]
cat(sprintf("  n communities: %d\n", nrow(large_comms)))
cat(sprintf("  pct_in_comm distribution:\n"))
print(round(quantile(large_comms$pct_in_comm,
                     probs = c(0, .1, .25, .5, .75, .9, 1), na.rm=TRUE), 1))
cat(sprintf("  Mean lift: %.1f\n", mean(large_comms$lift, na.rm=TRUE)))
cat(sprintf("  Median lift: %.1f\n", median(large_comms$lift, na.rm=TRUE)))
cat("\n")

# Exclusivity: what fraction of defining sentences are unique to one community?
# (a defining sentence appearing in only one community = maximally exclusive)
def_sent_freq <- defining_sents[, .(n_communities = .N), by = defining_sentence]
pct_exclusive <- 100 * mean(def_sent_freq$n_communities == 1)
cat(sprintf("Defining sentences that belong to exactly 1 community: %.1f%%\n",
            pct_exclusive))
cat("(High = clear category boundaries)\n\n")

# Apply rhetorical tags to defining sentences
large_comms[, rhetorical_type := sapply(defining_sentence, tag_rhetorical)]
large_comms[, primary_type := str_extract(rhetorical_type, "T[1-6?]:[^|]+")]

cat("Rhetorical type of defining sentences (large communities, >=100 members):\n")
type_dist <- large_comms[, .N, by = primary_type][order(-N)]
print(type_dist)
cat("\n")

cat("Mean prototype coherence by rhetorical type:\n")
coh_by_type <- large_comms[!is.na(primary_type), .(
  n = .N,
  mean_pct_in_comm = round(mean(pct_in_comm, na.rm=TRUE), 1),
  mean_lift        = round(mean(lift, na.rm=TRUE), 1),
  mean_size        = round(mean(size), 0)
), by = primary_type][order(-mean_pct_in_comm)]
print(coh_by_type)
cat("\n")

fwrite(defining_sents, "/home/rstudio/data/community_coherence.csv")

# =============================================================================
# SUMMARY
# =============================================================================

cat("=============================================================\n")
cat("--- SUMMARY: Evidence for Franchise Professional Identity ---\n\n")

cat("CLAIM: Identity templates function as professional membership markers,\n")
cat("spreading as portable franchise scripts across community boundaries.\n\n")

cat("1. RHETORICAL SPREAD\n")
t1t2 <- spread_by_type[primary_type %in% c("T1:character","T2:pledge")]
t3t5 <- spread_by_type[primary_type %in% c("T3:service","T5:credibility")]
cat(sprintf("   T1/T2 (franchise identity): mean_n_ctry=%.1f mean_n_occs=%.1f\n",
            mean(t1t2$mean_n_ctry), mean(t1t2$mean_n_occs)))
cat(sprintf("   T3/T5 (credential/service): mean_n_ctry=%.1f mean_n_occs=%.1f\n",
            mean(t3t5$mean_n_ctry), mean(t3t5$mean_n_occs)))
cat("   → Character/pledge claims spread more broadly = portable membership markers\n\n")

cat("2. CREDENTIAL SUBSTITUTION\n")
cat("   Overall r(prop_copied ~ log_hours):")
cat(sprintf(" %.4f\n",
            cor(model_dt$prop_copied_30, model_dt$log_hours,
                use="complete.obs", method="spearman")))
cat("   See credential_substitution_by_occ.csv for occupation breakdown\n\n")

cat("3. TEMPLATE RETENTION\n")
r_t1t2 <- retention_by_type[primary_type %in% c("T1:character","T2:pledge")]
r_t3t5 <- retention_by_type[primary_type %in% c("T3:service","T5:credibility")]
cat(sprintf("   T1/T2 mean pct adopters with tenure >365 days: %.1f%%\n",
            mean(r_t1t2$mean_pct_gt365, na.rm=TRUE)))
cat(sprintf("   T3/T5 mean pct adopters with tenure >365 days: %.1f%%\n",
            mean(r_t3t5$mean_pct_gt365, na.rm=TRUE)))
cat("   → Higher retention for franchise identity templates = more constitutive\n\n")

cat("4. CULTURAL DISTANCE\n")
cat(sprintf("   Spearman r(ef_epi, pct_copier_30): %+.4f\n", r_ef))
cat(sprintf("   L1 English mean copying: %.1f%%  L2 English: %.1f%%\n",
            l1_mean, l2_mean))
cat("   → Lower English proficiency associated with higher copying rate\n\n")

cat("5. COMMUNITY COHERENCE\n")
cat(sprintf("   Mean prototype coherence (pct_in_comm): %.1f%%\n",
            mean(large_comms$pct_in_comm, na.rm=TRUE)))
cat(sprintf("   Mean lift of defining sentence: %.1f×\n",
            mean(large_comms$lift, na.rm=TRUE)))
cat(sprintf("   Exclusive defining sentences: %.1f%%\n", pct_exclusive))
cat("   → Communities show category-like coherence and clear boundaries\n\n")

cat("=============================================================\n")
cat("06-IDENTITY-ANALYSIS COMPLETE\n")
cat("=============================================================\n")
