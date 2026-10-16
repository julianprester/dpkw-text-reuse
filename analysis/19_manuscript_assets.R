# Publication assets from saved estimates; no model fitting or new qualitative coding.
# Run from the project root in text-reuse-r via analysis/run.sh.
suppressPackageStartupMessages({ library(data.table); library(arrow); library(ggplot2) })
options(scipen = 6)
OUT <- "analysis/output/paper"
dir.create(file.path(OUT, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)
TBL <- "analysis/output/tables"
NUM <- "analysis/output/numbers"
read_tab <- function(name) fread(file.path(TBL, paste0(name, ".csv")))
reg <- function(file, key) {
  x <- fread(file.path(NUM, paste0(file, ".csv")))[name == key]
  stopifnot(nrow(x) == 1L)
  as.numeric(x$value)
}
ni <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fx <- function(x, d = 3) sprintf(paste0("%.", d, "f"), x)
pc <- function(x) 100 * expm1(x)
bs <- function(b, se) ifelse(is.na(b), "—", paste0(fx(b), " (", fx(se), ")"))
md <- function(x, file, caption, id, note = "") {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  lines <- c(paste0("| ", paste(names(x), collapse = " | "), " |"),
              paste0("| ", paste(c(":---", rep("---:", ncol(x) - 1)), collapse = " | "), " |"),
             vapply(seq_len(nrow(x)), function(i) paste0("| ", paste(x[i, ], collapse = " | "), " |"), ""), "",
             paste0(": ", caption, " {#", id, "}"), "", if (nzchar(note)) paste0("*Note.* ", note) else character())
  writeLines(lines, file.path(OUT, "tables", paste0(file, ".qmd")), useBytes = TRUE)
}
vars <- data.table(variable = character(), value = numeric(), rendered = character(), source = character())
put <- function(variable, value, source, rendered = ni(value)) {
  vars <<- rbind(vars, data.table(variable, value, rendered, source))
}
fromreg <- function(variable, file, key, style = "int", digits = 1) {
  x <- reg(file, key)
  text <- switch(style, int = ni(x), pct = paste0(fx(100 * x, digits), "%"),
                 decimal = fx(x, digits), signedpct = paste0(sprintf(paste0("%+.", digits, "f"), x), "%"))
  put(variable, x, paste0(NUM, "/", file, ".csv#", key), text)
}
fromreg("raw-profiles", "01_corpus", "profiles_raw")
fromreg("filtered-profiles", "01_corpus", "profiles_filtered")
fromreg("ids-unique", "21_collection_counts", "ids_unique")
fromreg("ids-duplicate", "21_collection_counts", "ids_duplicate_rows")
fromreg("ids-inaccessible", "21_collection_counts", "profiles_inaccessible")
fromreg("ids-inaccessible-share", "21_collection_counts", "profiles_inaccessible_share", "pct", 1)
fromreg("ids-error-removed", "21_collection_counts", "profiles_error_removed")
fromreg("ids-error-notfound", "21_collection_counts", "profiles_error_notfound")
stopifnot(reg("21_collection_counts", "profiles_usable") == reg("01_corpus", "profiles_raw"))
fromreg("workers", "01_corpus", "workers_with_sentences")
fromreg("sentences", "01_corpus", "sentences")
fromreg("scored-pairs", "02_pairs", "pairs_scored")
fromreg("same-worker-pairs", "02_pairs", "pairs_same_worker")
fromreg("exact-workers", "02_pairs", "workers_involved_J1")
fromreg("exact-prevalence", "02_pairs", "workers_involved_share_J1", "pct")
fromreg("scripts", "02_pairs", "scripts")
fromreg("dyadic-scripts", "02_pairs", "scripts_dyadic_share", "pct")
fromreg("top100-pairs", "02_pairs", "top100_share_pairs", "pct")
fromreg("top100-adoptions", "02_pairs", "top100_share_adoptions", "pct")
fromreg("max-adopters", "02_pairs", "script_max_adopters")
fromreg("shared-sentences-median", "04_anatomy", "copier_median_copied_sentences")
fromreg("shared-share-median", "04_anatomy", "copier_median_share", "pct")
fromreg("shared-words-median", "04_anatomy", "copier_median_word_share", "pct")
fromreg("later-shared", "04_anatomy", "within_profile_relpos_diff_share_later", "pct")
for (s in c("first", "middle", "last")) fromreg(paste0("position-", s), "04_anatomy", paste0("pos_exact_", s), "pct", 2)
fromreg("lsh-low", "02_pairs", "lsh_pdetect_J0.3", "pct")
fromreg("lsh-half", "02_pairs", "lsh_pdetect_J0.5", "pct")
fromreg("lexicon", "03_content", "lexicon_informative")
fromreg("m1-all", "12_model1", "m1_sentences_all")
fromreg("m1-sentences", "12_model1", "m1_sentences_used")
fromreg("m1-profiles", "12_model1", "m1_profiles_used")
for (k in c("lspec", "number", "assurance", "encounter", "rel"))
  fromreg(paste0("m1-or-", k), "12_model1", paste0("m1_or_", k), "decimal", if (k %in% c("lspec", "number")) 3 else 2)
fromreg("m2-workers", "13_model2", "m2_workers")
fromreg("m2-sharers", "13_model2", "m2_sharers")
fromreg("m2-families", "13_model2", "m2_families")
fromreg("m2-cities", "13_model2", "m2_cities")
for (k in c("authored_spec", "shared_gen", "shared_spec"))
  fromreg(paste0("m2-", gsub("_", "-", k)), "13_model2", paste0("m2_pct_", k), "signedpct")
fromreg("family-share", "13_model2", "m2_icc_provenance", "pct")
fromreg("family-lr", "13_model2", "m2_lr_chisq_family", "decimal", 1)
for (k in c("family", "city", "resid")) fromreg(paste0("m2-var-", k), "13_model2", paste0("m2_var_", k), "decimal", 5)
for (k in c("authored_spec", "shared_gen", "shared_spec"))
  fromreg(paste0("m2ml-", gsub("_", "-", k)), "13_model2", paste0("m2ml_pct_", k), "signedpct")
for (k in c("0_jobs", "1_9_jobs", "10_49_jobs", "50__jobs"))
  fromreg(paste0("record-gen-", gsub("_", "-", k)), "13_model2", paste0("m2_shared_gen_pct_", k), "signedpct")
for (a in c("city_key", "country", "occ1"))
  fromreg(paste0("lift-", gsub("_", "-", a)), "07_circulation", paste0("lift_", a, "_exact"), "decimal", 2)
fromreg("cohort-lift-month", "07_circulation", "cohort_lift30_same_city", "decimal", 2)
fromreg("cohort-lift-week", "07_circulation", "cohort_lift7_same_city", "decimal", 2)
fromreg("generic-country-lift", "07_circulation", "lift_country_exact_generic_only", "decimal", 2)
fromreg("generic-occupation-lift", "07_circulation", "lift_occ_exact_generic_only", "decimal", 2)
fromreg("price-correlation", "07_circulation", "status_cor_obs", "decimal", 3)
fromreg("price-correlation-null", "07_circulation", "status_cor_null", "decimal", 3)
fromreg("aligned-pairs", "06_edits", "edit_pairs_sampled")
for (k in c("number", "occupation_informative", "other_content_word", "pronoun_voice", "function_word"))
  fromreg(paste0("edit-", gsub("_", "-", k)), "06_edits", paste0("edit_rate_", k), "pct")
fromreg("dated-profiles", "01_corpus", "text_dated_profiles")
fromreg("burst-family", "05_scripts", "burst_peak6_median_family", "pct")
fromreg("burst-baseline", "05_scripts", "burst_peak6_baseline", "pct")

# Sample accounting and descriptive statistics from unchanged derived data.
W <- as.data.table(read_parquet("data/derived/workers.parquet"))[n_c > 0]
WR <- as.data.table(read_parquet("data/derived/worker_reuse.parquet"))
stopifnot(nrow(W) == reg("01_corpus", "workers_with_sentences"), uniqueN(W$wid) == nrow(W))
R <- W[!is.na(lrate) & !is.na(tenure_yrs)]
stopifnot(nrow(R) == reg("13_model2", "m2_workers"))
flow <- data.frame(Stage = c("Profile input rows", "After common-vocabulary filter", "At least one qualifying sentence", "Model 1: eligible sentences (profiles with at least two units)", "Model 1: mixed profiles (identifying sentences)", "Model 2: eligible asking rate and description age"),
                   Count = c(ni(reg("01_corpus", "profiles_raw")), ni(reg("01_corpus", "profiles_filtered")), ni(nrow(W)), ni(reg("12_model1", "m1_sentences_all")),
                             paste0(ni(reg("12_model1", "m1_profiles_used")), " (", ni(reg("12_model1", "m1_sentences_used")), " sentences)"), ni(nrow(R))))
md(flow, "sample_flow", "Sample accounting", "tbl-sample-flow", "The Model 1 logit drops profiles without variation in exact sharing. Model 2 restricts posted rates to USD 3–500 and requires observed description age. These frames are not interchangeable.")
desc <- rbindlist(lapply(c("hourlyRate", "totalJobsWorked", "totalHours", "n_c", "desc_chars", "tenure_yrs"), function(v) {
  x <- R[[v]]
  data.table(variable = v, n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), median = median(x, na.rm = TRUE), p25 = unname(quantile(x, .25, na.rm = TRUE)), p75 = unname(quantile(x, .75, na.rm = TRUE)))
}))
fwrite(desc, file.path(OUT, "tables", "sample_descriptives.csv"))
labels <- c("Posted hourly rate (USD)", "Completed jobs", "Recorded hours", "Qualifying sentences", "Description characters", "Description age (years)")
dshow <- data.frame(Measure = labels, Mean = fx(desc$mean, 2), SD = fx(desc$sd, 2), Median = fx(desc$median, 2), P25 = fx(desc$p25, 2), P75 = fx(desc$p75, 2))
md(dshow, "sample_descriptives", "Descriptive statistics in the Model 2 frame", "tbl-sample-descriptives", paste0(ni(nrow(R)), " workers. Description/profile age is measured from description creation to September 8, 2023, the latest observed edit; it is not account tenure. Rates are asking prices, not realized wages."))
record <- R[, .N, by = .(Record = fcase(totalJobsWorked == 0, "0 jobs", totalJobsWorked < 10, "1–9 jobs", totalJobsWorked < 50, "10–49 jobs", default = "50+ jobs"))]
record <- record[match(c("0 jobs", "1–9 jobs", "10–49 jobs", "50+ jobs"), Record)]
md(data.frame(Record = record$Record, Workers = ni(record$N), Share = paste0(fx(100 * record$N / sum(record$N), 1), "%")), "record_groups", "Completed-job groups in the Model 2 frame", "tbl-record-groups")

# Model 1 main and appendix results.
M1 <- read_tab("model1_coefficients")
terms1 <- c("lspec", "number", "funcassurance", "funcencounter formula", "funcother", "rel", "lwords")
labels1 <- c("Occupational specificity: log(1 + distinct informative types)", "Contains a numeric token", "Assurance", "Encounter formula", "Other function", "Relative position (0 = first, 1 = last qualifying sentence)", "Log sentence words")
A <- M1[model == "M1 main (FE logit)"][match(terms1, term)]
L <- M1[model == "M1 LPM (FE OLS)"][match(terms1, term)]
md(data.frame(Predictor = labels1, `Logit coefficient (SE)` = bs(A$est, A$se), `Odds ratio` = fx(exp(A$est)), `95% interval for OR` = paste0("[", fx(exp(A$est - 1.96 * A$se)), ", ", fx(exp(A$est + 1.96 * A$se)), "]"), `Linear probability (SE)` = bs(L$est, L$se), check.names = FALSE),
   "model1", "Model 1: within-profile authorship allocation", "tbl-model1",
   "773,424 sentences in 86,956 mixed profiles for the logit; worker fixed effects and worker-clustered standard errors. Function reference: dictionary-coded identity, credential, or capability (the estimation label is ‘individuating claim’); relative position runs from 0 (first qualifying sentence) to 1 (last). Intervals are normal-approximation Wald intervals. The specificity OR is per one-unit increase in log(1 + distinct informative types), not per additional word. The linear-probability column is a link-function check on direction; it uses all 2,309,694 eligible sentences, including profiles whose sharing outcome does not vary, and no odds-ratio transformation applies.")

# Model 2 selected coefficients for the two fixed-effects forms and multilevel form.
M2 <- read_tab("model2_fixed_effects_clustered")
ML <- read_tab("model2_fixed_effects_multilevel")
terms2 <- c("authored_spec", "shared_gen", "shared_spec", "authored_spec:d1", "authored_spec:d2", "authored_spec:d3", "d1:shared_any", "d2:shared_any", "d3:shared_any")
labels2 <- c("Authored-specific share", "Shared-generic share", "Shared-specific share", "Authored-specific × 1–9 jobs", "Authored-specific × 10–49 jobs", "Authored-specific × 50+ jobs", "Shared share × 1–9 jobs", "Shared share × 10–49 jobs", "Shared share × 50+ jobs")
A <- M2[model == "M2 composition (FE, country-clustered)"][match(terms2, term)]
B <- M2[model == "M2 x verified record (FE, country-clustered)"][match(terms2, term)]
C <- ML[match(terms2, term)]
show2 <- data.frame(Predictor = labels2, `2a: Composition (SE)` = bs(A$est, A$se), `2b: Record interactions (SE)` = bs(B$est, B$se), `2c: Multilevel (SE)` = bs(C$est, C$se), check.names = FALSE)
vc <- read_tab("model2_variance_components")
show2 <- rbind(show2, data.frame(Predictor = c("City variance", "Circulation-family variance", "Residual variance"), `2a: Composition (SE)` = "—", `2b: Record interactions (SE)` = "—", `2c: Multilevel (SE)` = fx(vc$var[match(c("city_f", "family", "Residual"), vc$grp)], 5), check.names = FALSE))
md(show2, "model2", "Model 2: profile composition, verified records, and circulation families", "tbl-model2",
   "Selected coefficients; dependent variable: log posted USD hourly rate; N = 352,986 throughout. Omitted composition: authored-generic share. All forms include country and first-listed-occupation effects, completed-job groups, log(1 + jobs), log(1 + hours), rating bands, a five-degree-of-freedom description-age spline, agency membership, occupation count, log description characters, and log qualifying-sentence count. Forms 2a/2b have country-clustered SEs. Form 2c has model-based SEs and ML city and sharer-only family random effects. In 2b/2c, composition main effects refer to zero jobs; shared-share interactions shift both shared categories equally. ML SEs are not country-clustered.")
SL <- read_tab("model2_slopes_by_record")
contrast <- function(b, se) paste0(sprintf("%+.1f", pc(b)), "% [", fx(pc(b - 1.96 * se), 1), ", ", fx(pc(b + 1.96 * se), 1), "]")
md(data.frame(Record = gsub("-", "–", SL$record), `Authored-specific` = contrast(SL$authored_spec_est, SL$authored_spec_se), `Shared-generic` = contrast(SL$shared_gen_est, SL$shared_gen_se), `Shared-specific` = contrast(SL$shared_spec_est, SL$shared_spec_se), check.names = FALSE),
   "model2_record_slopes", "Full-unit composition contrasts by completed-job group (Model 2b)", "tbl-model2-record-slopes",
   "Percent asking-rate differences relative to authored-generic composition, with approximate 95% intervals using the saved delta-method SEs. These are hypothetical whole-composition contrasts, not effects of one reused sentence or changes experienced by a typical worker.")

# Eleven-entry battery; report alternative outcomes in their own units, not as rate effects.
ROB <- read_tab("robustness_estimates")
SUMMARY <- read_tab("robustness_summary")
stopifnot(identical(ROB$check, SUMMARY$check))
put("rob-strict-shared-spec", pc(ROB$m2_shared_spec[5]), paste0(TBL, "/robustness_estimates.csv#row5:m2_shared_spec"), paste0(sprintf("%+.1f", pc(ROB$m2_shared_spec[5])), "%"))
for (i in c(2, 4, 6)) put(paste0("rob-family-", i), ROB$m2_icc_provenance[i], paste0(TBL, "/robustness_estimates.csv#row", i, ":m2_icc_provenance"), paste0(fx(100 * ROB$m2_icc_provenance[i], 1), "%"))
put("rob-timing-workers", ROB$m2_n[6], paste0(TBL, "/robustness_estimates.csv#row6:m2_n"))
put("rob-timing-lr", ROB$m2_lr_chisq[6], paste0(TBL, "/robustness_estimates.csv#row6:m2_lr_chisq"), fx(ROB$m2_lr_chisq[6], 1))
put("rob-timing-gen-50", pc(ROB$m2_shared_gen_50plus[6]), paste0(TBL, "/robustness_estimates.csv#row6:m2_shared_gen_50plus"), paste0(sprintf("%+.1f", pc(ROB$m2_shared_gen_50plus[6])), "%"))
put("rob-rating-gen-or", exp(ROB$m2_shared_gen[10]), paste0(TBL, "/robustness_estimates.csv#row10:m2_shared_gen"), fx(exp(ROB$m2_shared_gen[10]), 2))
put("rob-hours-gen", pc(ROB$m2_shared_gen[11]), paste0(TBL, "/robustness_estimates.csv#row11:m2_shared_gen"), paste0(fx(pc(ROB$m2_shared_gen[11]), 0), "%"))
short <- c("Main specification", "Sharing at J ≥ .7", "Sentences ≥ 15 words", "Exclude agency members", "Stricter specificity", "Within-one-day edit subset", "Ten largest countries", "Exclude ≥ 90% shared profiles", "Model 1 linear probability", "Rating < 4.8 (≥ 5 jobs)", "Hours conditional on rate")
rs <- data.frame(Check = short, `M1 allocation` = "—", `Authored-specific` = "—", `Shared-generic` = "—", `Shared-specific` = "—", `Family share` = "—", `Shared-generic: 0 → 50+ jobs` = "—", check.names = FALSE)
for (i in seq_len(nrow(ROB))) {
  z <- ROB[i]
  if (z$dv == "lrate") {
    rs[i, 2] <- if (isTRUE(SUMMARY$P1_authorship_allocation[i])) "Retained" else "Not retained"
    if (i != 9) {
      rs[i, 3:5] <- paste0(sprintf("%+.1f", pc(c(z$m2_authored_spec, z$m2_shared_gen, z$m2_shared_spec))), "%")
      rs[i, 7] <- paste0(sprintf("%+.1f", pc(z$m2_shared_gen_0jobs)), "% → ", sprintf("%+.1f", pc(z$m2_shared_gen_50plus)), "%")
    }
    fam <- if (i == 1) reg("13_model2", "m2_icc_provenance") else z$m2_icc_provenance
    if (!is.na(fam)) rs[i, 6] <- paste0(fx(100 * fam, 1), "%")
  } else if (z$dv == "lowrating") {
    rs[i, 3:5] <- c("No association", paste0("OR ", fx(exp(z$m2_shared_gen), 2)), paste0("OR ", fx(exp(z$m2_shared_spec), 2)))
  } else {
    rs[i, 3:5] <- c("No association", paste0("+", fx(pc(z$m2_shared_gen), 0), "% (1+hours)"), paste0("+", fx(pc(z$m2_shared_spec), 0), "% (1+hours)"))
  }
}
md(rs, "robustness", "Robustness and outcome triangulation", "tbl-robustness",
   "Rate rows show full-unit composition contrasts against authored-generic text; family share is family/(family + residual) variance, excluding city variance. ‘Retained’ denotes the saved joint allocation signature, not validation of every dictionary label. The stricter rule uses maximum word KL ≥ 1 bit and changes the Model 1 specificity scale. The LPM row changes only Model 1; Model 2 is not an independent new rate fit. The two outcome rows are not price-discount tests. Blank family entries were not refit.")
ra <- data.frame(Check = short, `M1 sentences` = ni(ROB$m1_n), `Worker outcome N` = ni(ROB$m2_n), `Specificity estimate (SE)` = bs(ROB$m1_lspec, ROB$m1_lspec_se), `Number estimate (SE)` = bs(ROB$m1_number, ROB$m1_number_se), check.names = FALSE)
md(ra, "robustness_samples", "Robustness samples and selected Model 1 coefficients", "tbl-robustness-samples",
   "Model 1 entries are logit coefficients except the LPM row. Alternative-outcome entries repeat main Model 1 rather than estimating new allocation relationships. The stricter rule uses maximum word KL, not log(1 + informative types). The ≥ 15-word check retains original qualifying-sentence position indicators.")
rb <- data.frame(Check = short, `Authored-specific (SE)` = bs(ROB$m2_authored_spec, ROB$m2_authored_spec_se), `Shared-generic (SE)` = bs(ROB$m2_shared_gen, ROB$m2_shared_gen_se), `Shared-specific (SE)` = bs(ROB$m2_shared_spec, ROB$m2_shared_spec_se), check.names = FALSE)
md(rb, "robustness_coefficients", "Robustness worker-level coefficients", "tbl-robustness-coefficients",
   "Country-clustered SEs. Rate rows are log-rate coefficients without record interactions; the rating row is log odds and the hours row uses log(1 + recorded hours). Outcome-specific controls are described in Appendix D. The LPM row repeats the main worker-level estimates.")

# Conservative publication versions of existing descriptive figures.
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title = element_blank(), legend.position = "bottom"))
savep <- function(p, name, w = 7, h = 4.4) ggsave(file.path(OUT, "figures", paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
H <- read_tab("circ_homophily_by_band")[attribute %in% c("city_key", "country", "occ1")]
H[, label := factor(attribute, levels = c("city_key", "country", "occ1"), labels = c("Same city", "Same country", "Same first-listed occupation"))]
H[, band := factor(band, levels = c("[0.3,0.5)", "[0.5,0.7)", "[0.7,0.9)", "[0.9,1)", "exact"), labels = c(".3–.5", ".5–.7", ".7–.9", ".9–<1", "1 (exact)"))]
savep(ggplot(H, aes(band, lift, color = label, group = label)) + geom_hline(yintercept = 1, linetype = 2, color = "grey50") + geom_line(linewidth = .8) + geom_point(size = 2.5) + scale_color_manual(values = c("#b73730", "#205a78", "#a66c00")) + labs(x = "Maximum sentence similarity within a worker pair (Jaccard)", y = "Lift relative to endpoint random mixing", color = NULL), "circulation")
P <- rbindlist(lapply(c("authored_spec", "shared_gen", "shared_spec"), function(k) data.table(record = SL$record, type = k, est = SL[[paste0(k, "_est")]], se = SL[[paste0(k, "_se")]])))
P[, record := factor(record, levels = SL$record, labels = gsub("-", "–", SL$record))]
P[, type := factor(type, levels = c("authored_spec", "shared_spec", "shared_gen"), labels = c("Authored-specific", "Shared-specific", "Shared-generic"))]
savep(ggplot(P, aes(record, pc(est), color = type, group = type)) + geom_hline(yintercept = 0, color = "grey50") + geom_errorbar(aes(ymin = pc(est - 1.96 * se), ymax = pc(est + 1.96 * se)), width = .1, position = position_dodge(.25)) + geom_line(linewidth = .7, position = position_dodge(.25)) + geom_point(size = 2.5, position = position_dodge(.25)) + scale_color_manual(values = c("#00796b", "#a66c00", "#b73730")) + labs(x = "Completed-job group", y = "Full-unit asking-rate contrast (%)", color = NULL), "model2_records", h = 4.7)
E <- read_tab("edits_rate_by_token_class")
E[, cls := factor(cls, levels = rev(c("number", "occupation-informative", "other content word", "pronoun/voice", "function word")))]
savep(ggplot(E, aes(100 * edit_rate, cls)) + geom_col(fill = "#205a78") + labs(x = "Substituted or deleted tokens / all side-A tokens (%)", y = NULL), "variant_slots", h = 3.5)

# Distribution of partial sharing among exact sharers (repertoire section).
SH <- WR[n1 > 0]
med_share <- median(SH$share1)
stopifnot(nrow(SH) == reg("02_pairs", "workers_involved_J1"), abs(med_share - reg("04_anatomy", "copier_median_share")) < 1e-6)
savep(ggplot(SH, aes(share1)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = .05, boundary = 0, fill = "#205a78", color = "white", linewidth = .3) +
  geom_vline(xintercept = med_share, linetype = 2, color = "#b73730") +
  annotate("text", x = med_share + .03, y = Inf, vjust = 1.6, hjust = 0, color = "#b73730", size = 3.6, label = paste0("Median sharer:\n", fx(100 * med_share, 1), "% of qualifying sentences")) +
  scale_x_continuous(labels = function(x) paste0(100 * x, "%"), breaks = seq(0, 1, .25)) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"), expand = expansion(mult = c(0, .05))) +
  labs(x = "Share of the profile's qualifying sentences that are exact-shared", y = "Share of exact sharers") +
  theme(panel.grid.major.x = element_blank()), "sharing_share", h = 4)

# Method diagram, using only static graphics so Quarto needs no host R or browser execution.
boxes <- function(b, edges, note = NULL, ylim = c(0, 7)) {
  p <- ggplot() + geom_rect(data = b, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2), fill = "#eef3f5", color = "#205a78", linewidth = .5) +
    geom_text(data = b, aes(x, y, label = text), size = 3.5, lineheight = 1.05) +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend), arrow = arrow(length = grid::unit(.12, "inches")), linewidth = .5, color = "#205a78") +
    coord_cartesian(xlim = c(0, 10), ylim = ylim, clip = "off") + theme_void()
  if (!is.null(note)) p <- p + annotate("text", x = 5, y = .25, label = note, size = 3.2)
  p
}
b <- data.table(x = c(2, 5, 8, 3, 7, 3, 7, 5, 5), y = c(7.4, 7.4, 7.4, 5.8, 5.8, 4.3, 4.3, 2.8, 1.2), w = c(2.7, 2.7, 2.7, 3.5, 3.5, 3.5, 3.5, 8, 6), h = c(.85, .85, .85, 1.1, 1.1, 1.1, 1.1, 1.0, .75),
                text = c("Public profile records", "Sentence corpus", "Cross-worker reuse", "Model 1\nWithin-profile allocation", "Model 2\nComposition and asking rates", "Content, function, position\nand variant alignments", "Verified records, city effects,\nand circulation families", "Qualitative analysis\nClose readings of theoretically sampled profiles and near-copy pairs", "Abductive interpretation and bounded propositions"))
e <- data.table(x = c(3.35, 6.35, 7.9, 8.1, 3, 7, 3, 7, 5), y = c(7.4, 7.4, 6.975, 6.975, 5.25, 5.25, 3.75, 3.75, 2.3), xend = c(3.65, 6.65, 3, 7, 3, 7, 3, 7, 5), yend = c(7.4, 7.4, 6.35, 6.35, 4.85, 4.85, 3.3, 3.3, 1.575))
savep(boxes(b, e, ylim = c(.5, 8)), "method", w = 8, h = 5.6)
b <- data.table(x = c(5, 5, 2.6, 7.4, 5, 5), y = c(6.35, 4.95, 3.45, 3.45, 1.85, .65), w = c(8, 8, 4, 4, 8, 8), h = c(.8, .85, 1, 1, .85, .75),
                text = c("COPYABILITY: visible, reproducible profile text (scope condition)", "P1: SELECTIVE ALLOCATION\nSpecific / quantified claims versus reusable assurances and formulae", "CONTENT AND AUTHORSHIP\nP2a: authored-specific premium\nP2b: shared-generic discount", "CIRCULATION / PROVENANCE\nP2c: family-linked asking-rate\ndifferences, net of city", "ASKING-RATE POSITION\nText composition and family associations", "P3: NON-SUBSTITUTION\nShared-generic association persists across verified-record groups"))
e <- data.table(x = c(5, 3, 7, 2.6, 7.4), y = c(5.95, 4.525, 4.525, 2.95, 2.95), xend = c(5, 2.6, 7.4, 3.5, 6.5), yend = c(5.375, 3.95, 3.95, 2.275, 2.275))
savep(boxes(b, e), "theory", w = 8, h = 5.3)

# Publication excerpts. The paper carries no case labels or platform identifiers; internal case codes stay in this
# script and in the audit file. Each excerpt must sit inside a saved unit: exact-shared (J = 1) or, for variants, J in [.5, 1).
# Pair rows are checked against the separately sampled near-copy pairs. Counts quoted in the text come from the same units.
rd <- function(f) as.data.table(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"))
S <- rd("qual/profile_sample_sentences.csv")
O <- rd("qual/profile_sample_overview.csv")
NC <- rd("qual/near_copy_pairs.csv")
qrow <- function(key, source, case, pos, kind, quotation) data.table(key, source, internal_case = case, pos = as.numeric(pos), kind, quotation)
Q <- rbindlist(list(
  qrow("risk1", "sentence", "P002", 11, "exact", "We could always start off with a small project with shared risk and then keep going if you're happy with my work."),
  qrow("risk2", "sentence", "P002", 12, "exact", "Chances are that you'll be more than happy."),
  qrow("guarantee", "sentence", "P065", 5, "exact", "I guarantee you will be satisfied with my work!"),
  qrow("pride", "sentence", "P065", 9, "exact", "I take pride in leaving all of my clients satisfied."),
  qrow("contact", "sentence", "P112", 13, "exact", "Please contact me so we can discuss how we can work together to fully meet your business needs!"),
  qrow("testimonial", "sentence", "P043", 6, "exact", "But don't just take my word for it!"),
  qrow("close1", "sentence", "P043", 20, "exact", "I am excited to collaborate with you and bring your vision to life."),
  qrow("close2", "sentence", "P043", 21, "exact", "Let's work together to create something truly extraordinary!"),
  qrow("shopify1", "sentence", "P149", 1, "exact", "Over the last 5 years I have been working as a Shopify developer."),
  qrow("shopify2", "sentence", "P149", 2, "exact", "I am specialized in Shopify front-end, back-end and Shopify speed optimization."),
  qrow("slot-base", "pair", "N039", NA, "pair", "We always prefer to keep our work transparent"),
  qrow("slot-web", "pair", "N039", NA, "pair", "We always prefer to keep our web development & application development work transparent"),
  qrow("slot-crm", "pair", "N086", NA, "pair", "We always prefer to keep our CRM development & consultation work transparent"),
  qrow("number-label", "pair", "N075", NA, "pair", "100% satisfaction: I focus on providing value to all of my clients and earning their trust"),
  qrow("review-label", "pair", "N075", NA, "pair", "Client reviews: I focus on providing value to all of my clients and earning their trust"),
  qrow("pledge", "sentence", "P016", 2, "exact", "I believe in creating original, engaging design solutions that resonate with your audience and are functional based on your unique needs."),
  qrow("open15", "sentence", "P016", 1, "variant", "I am a full time freelance graphic designer with over 15 years of professional experience"),
  qrow("open3", "sentence", "P101", 1, "variant", "Hi, I am full time freelance graphic designer and illustrator with over 3 years of professional experience"),
  qrow("developer", "sentence", "P060", 1, "exact", "I'm a developer with experience in building websites for small and medium sized businesses."),
  qrow("virtue1", "sentence", "P137", 8, "exact", "Over-delivering: this is core to my work as a freelancer."),
  qrow("virtue2", "sentence", "P137", 9, "exact", "My focus is on giving more than what I expect to receive."),
  qrow("virtue3", "sentence", "P137", 13, "exact", "Kindness: one of the biggest aspects of my life that I implement in every facet of my life."),
  qrow("reply", "sentence", "P057", 4, "exact", "I always reply within 24 hours if we don't have an on-going project."),
  qrow("warned", "sentence", "P057", 5, "exact", "You will be warned about any delays, vacations and emergency situations beforehand."),
  qrow("winwork", "sentence", "P060", 2, "exact", "Whether you're trying to win work, list your services or even create a whole online store \u2014 I can help!"),
  qrow("seo1", "sentence", "P081", 2, "exact", "I am a high-end SEO expert and specialized in increasing the amount of traffic to websites."),
  qrow("seo2", "sentence", "P081", 5, "exact", "My expertise lies in ecommerce, WordPress, Joomla, Shopify, Wix and local SEO."),
  qrow("migration", "sentence", "P112", 9, "exact", "I have also a lot of experience with migration from SharePoint on-premise to SharePoint online (Office 365)."),
  qrow("reviews", "sentence", "P137", 7, "exact", "The client reviews and feedback on my profile are immensely important to me and the value that I provide.")
))
norm <- function(x) trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
Q[, adopters := NA_real_]
for (i in seq_len(nrow(Q))) {
  z <- Q[i]
  if (z$source == "sentence") {
    u <- S[profile_code == z$internal_case & pos_c == z$pos]
    stopifnot(nrow(u) == 1, grepl(norm(z$quotation), norm(u$text), fixed = TRUE))
    stopifnot(if (z$kind == "exact") u$maxJ == 1 else (u$maxJ >= .5 && u$maxJ < 1))
    if (z$kind == "exact") Q$adopters[i] <- u$script_adopters
  } else {
    u <- NC[pair_code == z$internal_case]
    stopifnot(nrow(u) == 1, grepl(norm(z$quotation), norm(u$sentence_a), fixed = TRUE) || grepl(norm(z$quotation), norm(u$sentence_b), fixed = TRUE))
  }
}
# The text calls the "win work" sentence the second most widely shared in the corpus.
stopifnot(Q[key == "winwork"]$adopters == sort(read_tab("scripts_top300")$adopters, decreasing = TRUE)[2])
fwrite(Q, file.path(OUT, "quotation_audit.csv"))
for (i in which(!is.na(Q$adopters)))
  put(paste0("adopt-", Q$key[i]), Q$adopters[i], "qual/profile_sample_sentences.csv#script_adopters")
# Posted rates and completed jobs for profiles discussed in the text (public profile metadata in the sampled overview).
meta <- function(case, col) { v <- O[profile_code == case][[col]]; stopifnot(length(v) == 1); as.numeric(v) }
put("rate-virtue-low", meta("P065", "hourlyRate"), "qual/profile_sample_overview.csv#P065:hourlyRate")
put("rate-virtue-high", meta("P137", "hourlyRate"), "qual/profile_sample_overview.csv#P137:hourlyRate")
put("jobs-seo", meta("P081", "totalJobsWorked"), "qual/profile_sample_overview.csv#P081:totalJobsWorked")
put("jobs-sharepoint", meta("P112", "totalJobsWorked"), "qual/profile_sample_overview.csv#P112:totalJobsWorked")
put("jobs-ux", meta("P137", "totalJobsWorked"), "qual/profile_sample_overview.csv#P137:totalJobsWorked")

stopifnot(!anyDuplicated(vars$variable))
writeLines(paste0(vars$variable, ': "', vars$rendered, '"'), "_variables.yml", useBytes = TRUE)
fwrite(vars, file.path(OUT, "number_audit.csv"))
inputs <- c("analysis/19_manuscript_assets.R", "analysis/12_model1_authorship.R", "analysis/13_model2_valuation.R", "analysis/14_robustness.R",
            list.files(NUM, pattern = "\\.csv$", full.names = TRUE),
            list.files(TBL, pattern = "^(model[12]_|robustness_|circ_homophily_by_band|edits_rate_by_token_class).*\\.csv$", full.names = TRUE),
            "qual/profile_sample_sentences.csv", "qual/profile_sample_overview.csv", "qual/near_copy_pairs.csv")
fwrite(data.table(path = inputs, md5 = unname(tools::md5sum(inputs))), file.path(OUT, "input_snapshot.csv"))
writeLines(c("Publication assets assembled from saved estimates, without re-estimation.",
             "Tables: model coefficients, explicitly labeled SEs, and normal-approximation intervals.",
             "Figures: publication labels replace earlier causal or actor-direction wording.",
             "Original age spline retained; tenure_yrs is DESCRIPTION/PROFILE AGE, distinct from job/hour experience controls.",
             "Within-one-day edit status does not establish that text preceded any completed work.",
             paste0("Quotation checks: all ", nrow(Q), " excerpts found in saved exact-shared, variant, or near-copy-pair units."),
             paste0("Model 2 frame check: ", nrow(R), " workers.")), file.path(OUT, "asset_checks.txt"))
cat("Manuscript assets assembled; saved estimates and quotations checked.\n")
