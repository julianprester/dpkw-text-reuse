# 06_edits.R -- Template slots: what do workers change when they reuse a sentence?
# Word-level alignment of near-copy pairs (0.5 <= J < 1). Each word type is mapped to a
# single Unicode code point so that utils::adist() returns a word-level edit transcript.
#
# Output: tables/edits_*.csv, numbers/06_edits.csv

source("analysis/00_setup.R")
suppressPackageStartupMessages(library(tokenizers))
set.seed(66)
P  <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[score >= 0.5 & score < 1]
S  <- as.data.table(read_parquet(file.path(DERIVED, "sentences.parquet")))[, .(sid, text)]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[, .(wid, occ1, country)]
lex <- fread(file.path(OUT, "tables", "lexicon_occupational_specificity.csv"))[, .(token, informative)]
rec("near_pairs_total", nrow(P), "cross-worker sentence pairs with 0.5 <= J < 1")
P <- P[sample(.N, min(.N, 400000))]
P[, `:=`(ta = S$text[match(sa, S$sid)], tb = S$text[match(sb, S$sid)])]
P[, same_occ := W$occ1[match(wa, W$wid)] == W$occ1[match(wb, W$wid)]]
P[, same_cty := W$country[match(wa, W$wid)] == W$country[match(wb, W$wid)]]

ta <- tokenize_words(P$ta, strip_numeric = FALSE); tb <- tokenize_words(P$tb, strip_numeric = FALSE)
vocab <- unique(c(unlist(ta), unlist(tb)))
code <- intToUtf8(0x10000L + seq_along(vocab), multiple = TRUE)
enc <- function(tt) vapply(tt, function(v) paste(code[match(v, vocab)], collapse = ""), "")
ea <- enc(ta); eb <- enc(tb)

ops <- vector("list", nrow(P))
for (i in seq_len(nrow(P))) {
  tr <- attr(adist(ea[i], eb[i], counts = TRUE), "trafos")[1, 1]
  ch <- strsplit(tr, "")[[1]]
  a <- ta[[i]]; b <- tb[[i]]; ia <- 1L; ib <- 1L
  out_type <- character(0); out_a <- character(0); out_b <- character(0); out_pos <- numeric(0)
  for (o in ch) {
    if (o == "M") { ia <- ia + 1L; ib <- ib + 1L }
    else if (o == "S") { out_type <- c(out_type, "sub"); out_a <- c(out_a, a[ia]); out_b <- c(out_b, b[ib]); out_pos <- c(out_pos, (ia - 1) / max(1, length(a) - 1)); ia <- ia + 1L; ib <- ib + 1L }
    else if (o == "D") { out_type <- c(out_type, "del"); out_a <- c(out_a, a[ia]); out_b <- c(out_b, NA); out_pos <- c(out_pos, (ia - 1) / max(1, length(a) - 1)); ia <- ia + 1L }
    else if (o == "I") { out_type <- c(out_type, "ins"); out_a <- c(out_a, NA); out_b <- c(out_b, b[ib]); out_pos <- c(out_pos, (ia - 1) / max(1, length(a) - 1)); ib <- ib + 1L }
  }
  ops[[i]] <- if (length(out_type)) data.table(pair = i, op = out_type, a = out_a, b = out_b, rel = out_pos) else NULL
}
O <- rbindlist(ops)
rec("edit_pairs_sampled", nrow(P), "near-copy pairs aligned")
rec("edit_ops", nrow(O), "word-level edit operations")
rec("edits_per_pair_median", med(O[, .N, by = pair]$N), "median number of word edits per near-copy pair")

# ---- token classes -------------------------------------------------------------------
stop <- stopwords::stopwords("en", source = "snowball")
pron <- c("i", "we", "my", "our", "me", "us", "i'm", "i've", "we're", "we've", "you", "your", "you're", "myself", "ourselves")
numw <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "fifteen", "twenty")
info <- lex[informative == TRUE, token]
cls <- function(x) fcase(is.na(x), NA_character_, x %in% pron, "pronoun/voice", str_detect(x, "^[0-9]+$") | x %in% numw, "number",
                         x %in% info, "occupation-informative", x %in% stop, "function word", default = "other content word")
# edit rate per class: edited tokens / all tokens of that class (side a)
allA <- data.table(pair = rep(seq_len(nrow(P)), lengths(ta)), tok = unlist(ta))
allA[, cls := cls(tok)]
edA <- O[op %in% c("sub", "del"), .(pair, tok = a)]
edA[, cls := cls(tok)]
rate <- merge(allA[, .(tokens = .N), by = cls], edA[, .(edited = .N), by = cls], by = "cls")[, edit_rate := edited / tokens][order(-edit_rate)]
print(rate); save_table(rate, "edits_rate_by_token_class")
for (i in seq_len(nrow(rate))) rec(paste0("edit_rate_", gsub("[^a-z]", "_", rate$cls[i])), rate$edit_rate[i], "share of tokens of this class that are substituted or deleted in near-copy pairs")
# same vs cross occupation: are occupation-informative tokens swapped more across occupations?
rate_occ <- merge(allA[, .(tokens = .N), by = .(cls, same_occ = P$same_occ[pair])], edA[, .(edited = .N), by = .(cls, same_occ = P$same_occ[pair])], by = c("cls", "same_occ"))[, edit_rate := edited / tokens]
print(dcast(rate_occ[!is.na(same_occ)], cls ~ same_occ, value.var = "edit_rate")); save_table(rate_occ, "edits_rate_by_class_and_occupation_match")
# most frequent substitutions (unordered)
sub <- O[op == "sub"][, `:=`(x = pmin(a, b), y = pmax(a, b))][, .N, by = .(x, y)][order(-N)]
sub[, `:=`(cx = cls(x), cy = cls(y))]
save_table(sub[1:300], "edits_top_substitutions"); print(sub[1:40])
subc <- O[op == "sub", .N, by = .(ca = cls(a), cb = cls(b))][, share := N / sum(N)][order(-N)]
save_table(subc, "edits_substitution_class_pairs")
# where in the sentence do edits happen?
posd <- O[, .N, by = .(rel_bin = pmin(floor(rel * 5), 4))][order(rel_bin)][, share := N / sum(N)]
save_table(posd, "edits_position"); print(posd)
# pair-level: what kind of edit dominates?
pl <- O[, .(n = .N, only_num = all(cls(fcoalesce(a, b)) == "number"), any_num = any(cls(fcoalesce(a, b)) == "number"),
            any_info = any(cls(fcoalesce(a, b)) == "occupation-informative"), any_pron = any(cls(fcoalesce(a, b)) == "pronoun/voice")), by = pair]
rec("pairs_any_number_edit", mean(pl$any_num), "share of near-copy pairs with >=1 edited number")
rec("pairs_any_occinfo_edit", mean(pl$any_info), "share of near-copy pairs with >=1 edited occupation-informative word")
rec("pairs_any_pronoun_edit", mean(pl$any_pron), "share of near-copy pairs with >=1 edited pronoun (voice change, e.g. I/we)")
save_numbers("06_edits")
