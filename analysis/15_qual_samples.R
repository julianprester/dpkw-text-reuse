# 15_qual_samples.R -- Theoretically sampled materials for the qualitative strand.
#
#  A. profile_sample:     ~150 profiles in theoretically defined cells (typical, deviant and
#                         contrast cases for Models 1 and 2), with full text and sentence markup
#  B. near_copy_pairs:    100 near-copy pairs (0.6 <= J < 1) with word-level diff markup
#  C. provenance_worksheet: ~70 template families to trace to their sources (largest, most
#                         extreme in price, most local, burstiest), with search strings
#
# Output: qual/*.csv (contain full public profile texts -- do not share; anonymise quotes)

source("analysis/00_setup.R")
suppressPackageStartupMessages({ library(fixest); library(tokenizers) })
set.seed(1515)
QUAL <- "qual"; dir.create(QUAL, showWarnings = FALSE)
S  <- as.data.table(read_parquet(file.path(DERIVED, "sentences.parquet")))[, .(sid, wid, pos_c, n_c, text)]
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, maxJ, cid, script_adopters)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, move, macro, spec_n, specific)]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0]
SA <- as.data.table(read_parquet(file.path(DERIVED, "script_attrs.parquet")))
AS <- as.data.table(read_parquet(file.path(DERIVED, "worker_assembly.parquet")))
D  <- as.data.table(read_parquet(file.path(DERIVED, "descriptions.parquet")))
X  <- merge(merge(S, SR, by = "sid"), C, by = "sid")
X  <- merge(X, SA[, .(cid, family)], by = "cid", all.x = TRUE)
setorder(X, wid, pos_c)

# ---- worker-level quantities used for sampling -------------------------------------------
comp <- X[, .(share1 = mean(maxJ >= 1), first_shared = any(pos_c == 1 & maxJ >= 1), first_shared_spec = any(pos_c == 1 & maxJ >= 1 & specific),
              last_shared = any(pos_c == n_c & maxJ >= 1), first_authored = any(pos_c == 1 & maxJ < 1), n1 = sum(maxJ >= 1)), by = wid]
fam <- X[maxJ >= 1 & !is.na(family), .(n = .N), by = .(wid, family)][order(wid, -n)][, .SD[1], by = wid][, .(wid, family)]
Wk <- merge(merge(merge(W, comp, by = "wid"), fam, by = "wid", all.x = TRUE), AS[, .(wid, best_partner_cover)], by = "wid", all.x = TRUE)
big_c <- Wk[, .N, by = country][N >= 200 & !is.na(country), country]
Wk[, `:=`(country_f = factor(fifelse(country %in% big_c, country, "Other")), occ_f = factor(fifelse(is.na(occ1), "None", occ1)), lchars = log(desc_chars))]
# adjusted price: residual of log rate on experience, tenure, country and occupation (for sampling only)
fr <- feols(lrate ~ ljobs + lhours + splines::ns(tenure_yrs, 5) | country_f + occ_f, data = Wk[!is.na(lrate) & !is.na(tenure_yrs)])
Wk[!is.na(lrate) & !is.na(tenure_yrs), adj_price := resid(fr)]
Wk[, rate_pct_country := frank(hourlyRate, ties.method = "average", na.last = "keep") / sum(!is.na(hourlyRate)), by = country]
fi <- Wk[!is.na(family) & !is.na(adj_price), .(family_price_index = mean(adj_price), adopters_priced = .N), by = family]

# ---- A. profile sample ----------------------------------------------------------------------
pick <- function(d, n, cell) { d <- d[!is.na(hourlyRate)]; if (nrow(d) == 0) return(NULL); d[sample(.N, min(n, .N)), .(wid, cell = cell)] }
lowF  <- fi[adopters_priced >= 30][order(family_price_index)][1:5, family]
highF <- fi[adopters_priced >= 30][order(-family_price_index)][1:5, family]
bigF  <- fi[adopters_priced >= 100][order(-adopters_priced)][1:5, family]
cells <- rbindlist(list(
  pick(Wk[first_authored & last_shared & share1 %between% c(0.15, 0.5) & n_c >= 5], 20, "A1 typical: authored opening, shared closing"),
  pick(Wk[first_shared_spec == TRUE & n_c >= 4], 15, "A2 deviant: shared, occupation-specific opening"),
  pick(Wk[share1 >= 0.9 & n_c >= 4], 15, "A3 fully templated (>= 90% shared)"),
  pick(Wk[totalJobsWorked >= 50 & share1 >= 1/3 & n_c >= 5], 15, "A4 experienced (50+ jobs) with >= 1/3 shared"),
  pick(Wk[best_partner_cover == 1 & n1 >= 3], 15, "A5 block cloner (all shared text from one source, >= 3 sentences)"),
  pick(Wk[best_partner_cover <= 0.5 & n1 >= 3], 15, "A6 bricoleur (no source covers > half, >= 3 sentences)"),
  rbindlist(lapply(lowF, function(f) pick(Wk[family == f], 3, paste0("A7 low-priced family (", f, ")")))),
  rbindlist(lapply(highF, function(f) pick(Wk[family == f], 3, paste0("A8 high-priced family (", f, ")")))),
  rbindlist(lapply(bigF, function(f) { d <- Wk[family == f & !is.na(adj_price)][order(adj_price)]; d[c(1, .N), .(wid, cell = paste0("A9 same family, price contrast (", f, ")"))] })),
  pick(Wk[share1 == 0 & totalJobsWorked >= 50 & rate_pct_country >= 0.9 & n_c >= 5], 15, "A10 contrast: fully authored, top-decile price, 50+ jobs")
))
cells <- unique(cells, by = "wid")
ov <- merge(cells, Wk[, .(wid, worker, country, occ1, title, hourlyRate, totalJobsWorked, totalHours, rating, tenure_yrs, agency, n_c, share1, family, adj_price)], by = "wid")
ov <- merge(ov, fi[, .(family, family_price_index)], by = "family", all.x = TRUE)
ov <- merge(ov, D, by = "wid")
ov[, profile_code := sprintf("P%03d", .I)]
setcolorder(ov, c("profile_code", "cell", "worker"))
fwrite(ov[order(cell)], file.path(QUAL, "profile_sample_overview.csv"))
mark <- merge(X[wid %in% ov$wid, .(wid, pos_c, n_c, shared = maxJ >= 1, maxJ = round(maxJ, 2), script_adopters, family, move, macro, spec_n, text)], ov[, .(wid, profile_code)], by = "wid")
fwrite(mark[order(profile_code, pos_c)][, -"wid"], file.path(QUAL, "profile_sample_sentences.csv"))
rec("qual_profiles", nrow(ov), "profiles in the qualitative sample"); print(ov[, .N, by = cell][order(cell)])

# ---- B. near-copy pairs with word-level diffs ----------------------------------------------
P <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[score >= 0.6 & score < 1]
P[, `:=`(oa = W$occ1[match(wa, W$wid)], ob = W$occ1[match(wb, W$wid)])]
P <- rbind(P[oa == ob][sample(.N, 50)][, stratum := "same occupation"], P[oa != ob][sample(.N, 50)][, stratum := "different occupation"])
P[, `:=`(ta = S$text[match(sa, S$sid)], tb = S$text[match(sb, S$sid)])]
diffmark <- function(a, b) {
  A <- tokenize_words(a, strip_numeric = FALSE)[[1]]; B <- tokenize_words(b, strip_numeric = FALSE)[[1]]
  voc <- unique(c(A, B)); code <- intToUtf8(0x10000L + seq_along(voc), multiple = TRUE)
  tr <- strsplit(attr(adist(paste(code[match(A, voc)], collapse = ""), paste(code[match(B, voc)], collapse = ""), counts = TRUE), "trafos")[1, 1], "")[[1]]
  i <- 1; j <- 1; out <- character(0)
  for (o in tr) {
    if (o == "M") { out <- c(out, A[i]); i <- i + 1; j <- j + 1 }
    else if (o == "S") { out <- c(out, sprintf("[%s -> %s]", A[i], B[j])); i <- i + 1; j <- j + 1 }
    else if (o == "D") { out <- c(out, sprintf("[-%s]", A[i])); i <- i + 1 }
    else { out <- c(out, sprintf("[+%s]", B[j])); j <- j + 1 }
  }
  paste(out, collapse = " ")
}
P[, diff := mapply(diffmark, ta, tb)]
P[, pair_code := sprintf("N%03d", .I)]
fwrite(P[, .(pair_code, stratum, jaccard = round(score, 2), occupation_a = oa, occupation_b = ob, sentence_a = ta, sentence_b = tb, diff)], file.path(QUAL, "near_copy_pairs.csv"))

# ---- C. provenance worksheet -----------------------------------------------------------------
FA <- SA[, .(scripts = .N, script_adoptions = sum(adopters), lead = text[which.max(adopters)], lead_cid = cid[which.max(adopters)]), by = family]
fam_ad <- unique(merge(X[!is.na(family) & maxJ >= 1, .(family, wid)], W[, .(wid, country, occ1, cr, text_dated)], by = "wid"), by = c("family", "wid"))
FA <- merge(FA, fam_ad[, .(adopters = .N,
  top_countries = paste(head(sprintf("%s %.0f%%", names(sort(table(country), TRUE)), 100 * sort(prop.table(table(country)), TRUE)), 3), collapse = "; "),
  top_occupations = paste(head(sprintf("%s %.0f%%", names(sort(table(occ1), TRUE)), 100 * sort(prop.table(table(occ1)), TRUE)), 2), collapse = "; "),
  cty_conc = { t <- prop.table(table(country)); sum(t^2) },
  first_dated_adoption = suppressWarnings(min(as.Date(cr[text_dated %in% TRUE])))), by = family], by = "family")
FA <- merge(FA, fi, by = "family", all.x = TRUE)
# distinctive search string: the member sentence with the rarest words (fewest adopters among long members)
mem <- merge(SA[words >= 10, .(family, cid, text, adopters)], FA[, .(family)], by = "family")
FA <- merge(FA, mem[order(adopters)][, .(search_string = text[1]), by = family], by = "family", all.x = TRUE)
sel <- unique(rbind(FA[order(-adopters)][1:40, .(family, reason = "largest families")],
                    FA[adopters_priced >= 30][order(family_price_index)][1:10, .(family, reason = "lowest-priced families")],
                    FA[adopters_priced >= 30][order(-family_price_index)][1:10, .(family, reason = "highest-priced families")],
                    FA[adopters >= 30][order(-cty_conc)][1:10, .(family, reason = "most local families")]), by = "family")
burst <- fread(file.path(OUT, "tables", "families_burstiness.csv"))[, .(family, peak6, span_months)]
WS <- merge(merge(sel, FA, by = "family"), burst, by = "family", all.x = TRUE)[order(reason, -adopters)]
WS[, family_code := sprintf("F%03d", .I)]
WS <- WS[, .(family_code, reason, family, adopters, scripts, lead_sentence = lead, search_string, top_countries, top_occupations,
             first_dated_adoption, peak6_share = round(peak6, 2), family_price_index = round(family_price_index, 3),
             source_found = "", source_url = "", source_type = "", source_date = "", source_language_country = "", framing = "", personalisation_instructions = "", notes = "")]
fwrite(WS, file.path(QUAL, "provenance_worksheet.csv"))
rec("qual_families", nrow(WS), "template families in the provenance worksheet"); print(WS[, .N, by = reason])
save_numbers("15_qual_samples")
