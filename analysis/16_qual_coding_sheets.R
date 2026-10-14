# 16_qual_coding_sheets.R -- Canonical units and readers for qualitative coding (codebook v1).
#
# Pass 1: title + full description; codable sentences numbered; computed labels and metadata hidden.
# Pass 2: the same profiles with reuse flags, adopter counts, family, nearest variant and word diff.
# Pass 3: the 100 near-copy pairs of qual/near_copy_pairs.csv.
# Initial readers hid labels, not theory context; final judgments followed full-context review.
#
# Output: qual/coding/units/* (stimulus tables and full readers; no blank coding fields),
#         qual/coding/julian/views/* (compact readers used for the first-author close reading)

source("analysis/00_setup.R")
suppressPackageStartupMessages(library(tokenizers))
SH <- "qual/coding/units"; VW <- "qual/coding/julian/views"
for (d in c(SH, VW)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
OV <- fread("qual/profile_sample_overview.csv")
SE <- fread("qual/profile_sample_sentences.csv")
setorder(OV, profile_code)

# ---- 1. re-segment the descriptions exactly as in 01_corpus.R ----------------------------
OV[, clean_text := str_trim(str_replace_all(str_replace_all(description, "\\r\\n|\\r|\\n", "\n"), "[\\s\t]+", " "))]
seg <- tokenize_sentences(OV$clean_text)
G <- data.table(profile_code = rep(OV$profile_code, lengths(seg)), segment = unlist(seg, use.names = FALSE))
G[, seg_id := seq_len(.N), by = profile_code]
G[, nwords := str_count(segment, "\\S+")]
G[, codable := !is.na(segment) & nwords >= 8 & nwords <= 50]
G[, pos := NA_integer_][codable == TRUE, pos := seq_len(.N), by = profile_code]
chk <- merge(G[codable == TRUE, .(profile_code, pos, low = str_to_lower(segment))], SE[, .(profile_code, pos = pos_c, text)], by = c("profile_code", "pos"))
stopifnot(nrow(chk) == nrow(SE), all(chk$low == chk$text))
rec("sheet_profiles", nrow(OV), "profiles in the coding sheets")
rec("sheet_sentences", nrow(SE), "numbered (codable) sentences")
rec("sheet_segments_other", G[codable == FALSE, .N], "other text segments shown for context (headings, fragments, run-on lists)")

# ---- 2. sharing information and nearest variants (pass 2) ----------------------------------
S <- as.data.table(read_parquet(file.path(DERIVED, "sentences.parquet")))[, .(sid, wid, pos_c, text)]
key <- merge(OV[, .(profile_code, wid)], S[wid %in% OV$wid, .(wid, pos = pos_c, sid)], by = "wid")
SE2 <- merge(SE[, .(profile_code, pos = pos_c, shared, maxJ, script_adopters, family)], key[, .(profile_code, pos, sid)], by = c("profile_code", "pos"))
near <- SE2[maxJ >= 0.5 & maxJ < 1, sid]
P <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[sa %in% near | sb %in% near, .(sa, sb, score)]
P <- rbind(P[sa %in% near, .(sid = sa, other = sb, score)], P[sb %in% near, .(sid = sb, other = sa, score)])
setorder(P, sid, -score, other)
P <- P[, .SD[1], by = sid]
P[, `:=`(own = S$text[match(sid, S$sid)], variant = S$text[match(other, S$sid)])]
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
P[, diff := mapply(diffmark, own, variant)]
SE2 <- merge(SE2, P[, .(sid, variant, diff)], by = "sid", all.x = TRUE)
G <- merge(G, SE2[, .(profile_code, pos, shared, maxJ, script_adopters, family, variant, diff)], by = c("profile_code", "pos"), all.x = TRUE)
setorder(G, profile_code, seg_id)
rec("sheet_near_copies", length(near), "numbered sentences that are near-copies (0.5 <= J < 1) -> slot coding")

# ---- 3. separately sampled near-copy pairs ------------------------------------------------
PR <- fread("qual/near_copy_pairs.csv")

# ---- 4. readers ---------------------------------------------------------------------------
trunc <- function(x, n) ifelse(!is.na(x) & nchar(x) > n, paste0(substr(x, 1, n), " …"), x)
flag <- function(g) fifelse(g$maxJ >= 1, sprintf("`SHARED x%d · fam %s`", g$script_adopters, g$family),
                     fifelse(g$maxJ >= 0.5, sprintf("`NEAR-COPY J=%.2f`", g$maxJ), "`authored`"))
reader <- function(pass, full) {
  intro <- if (pass == 1) "Initial pass: computed labels and metadata hidden. Numbered sentences [n] are the sentence coding units; segments marked · are context (headings, fragments, run-on lists). Codes are stored separately in `../julian/`."
           else "Pass 2. Each numbered sentence is flagged: SHARED xN (exact shingle-set match in N profiles; fam = co-adoption family), NEAR-COPY (with its nearest variant and a word diff: [old -> new], [-dropped], [+added]), or authored (undetected at J >= .5, not proven original). Slot and profile codes are stored separately in `../julian/`."
  out <- c(sprintf("# Pass %d reader", pass), "", intro, "")
  for (pc in OV$profile_code) {
    o <- OV[profile_code == pc]
    out <- c(out, sprintf("## %s", pc), "", sprintf("**Title:** %s", o$title))
    if (pass == 2) out <- c(out, "", sprintf("**First-listed category:** %s", fifelse(is.na(o$occ1) | o$occ1 == "", "none", o$occ1)))
    out <- c(out, "")
    g <- G[profile_code == pc]
    for (i in seq_len(nrow(g))) {
      if (isTRUE(g$codable[i])) {
        txt <- if (pass == 2 && !full && g$maxJ[i] < 0.5) trunc(g$segment[i], 70) else g$segment[i]
        line <- if (pass == 1) sprintf("- **[%d]** %s", g$pos[i], txt) else sprintf("- **[%d]** %s %s", g$pos[i], flag(g[i]), txt)
        out <- c(out, line)
        if (pass == 2 && !is.na(g$diff[i])) out <- c(out, sprintf("    - nearest variant: %s", g$variant[i]), sprintf("    - diff: %s", g$diff[i]))
      } else {
        out <- c(out, sprintf("- · %s", if (full) g$segment[i] else trunc(g$segment[i], if (pass == 1) 160 else 70)))
      }
    }
    out <- c(out, "")
  }
  out
}
writeLines(reader(1, TRUE), file.path(SH, "1_reader.md")); writeLines(reader(2, TRUE), file.path(SH, "2_reader.md"))
# compact readers for the first-author close reading (same units; context segments and pass-2 texts truncated)
compact <- function(pass) {
  out <- character(0)
  for (pc in OV$profile_code) {
    o <- OV[profile_code == pc]
    out <- c(out, sprintf("## %s | %s%s", pc, o$title, if (pass == 2) sprintf(" | cat: %s", o$occ1) else ""))
    g <- G[profile_code == pc]
    for (i in seq_len(nrow(g))) {
      if (isTRUE(g$codable[i])) {
        if (pass == 1) out <- c(out, sprintf("[%d] %s", g$pos[i], g$segment[i]))
        else {
          f <- fifelse(g$maxJ[i] >= 1, sprintf("S%d/f%s", g$script_adopters[i], g$family[i]), fifelse(g$maxJ[i] >= 0.5, sprintf("N%.2f", g$maxJ[i]), "A"))
          out <- c(out, if (!is.na(g$diff[i])) sprintf("[%d] %s diff: %s", g$pos[i], f, g$diff[i]) else sprintf("[%d] %s %s", g$pos[i], f, trunc(g$segment[i], 60)))
        }
      } else out <- c(out, sprintf("· %s", trunc(g$segment[i], if (pass == 1) 120 else 50)))
    }
  }
  out
}
writeLines(compact(1), file.path(VW, "1_reader_compact.md")); writeLines(compact(2), file.path(VW, "2_reader_compact.md"))

# ---- 5. canonical stimulus tables (the assembler adds persisted codes) -----------------------
fwrite(G[codable == TRUE, .(profile_code, pos, sentence = segment)][order(profile_code, pos)], file.path(SH, "1_sentences.csv"))
fwrite(OV[, .(profile_code)], file.path(SH, "1_profiles_language.csv"))
fwrite(G[codable == TRUE & maxJ >= 0.5 & maxJ < 1, .(profile_code, pos, jaccard = round(maxJ, 2), sentence = segment, nearest_variant = variant, diff)][
  order(profile_code, pos)], file.path(SH, "2_slots.csv"))
fwrite(OV[, .(profile_code)], file.path(SH, "2_profiles_structure.csv"))
fwrite(PR[, .(pair_code, stratum, jaccard, occupation_a, occupation_b, sentence_a, sentence_b, diff)], file.path(SH, "3_pairs.csv"))
save_numbers("16_qual_coding_sheets")
