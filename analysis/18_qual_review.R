# 18_qual_review.R -- Descriptive review of the qualitative coding.
# Run: Rscript analysis/18_qual_review.R [--codes=qual/coding/julian] [--out=<directory>]
# All outputs are internal. Dictionary concordance is a measurement diagnostic,
# not classifier accuracy or validation against an independent gold standard.

suppressPackageStartupMessages(library(data.table))

coding_specs <- function() {
  moves <- c("IDENTITY", "CREDENTIAL", "CAPABILITY", "CHARACTER", "PROCESS", "ENCOUNTER", "AGENCY", "OTHER")
  edits <- c("OCCUPATION", "TOOL/SKILL", "NUMBER", "NAME/PLACE", "VOICE", "TRUNCATION/EXTENSION", "WORDING", "FORMAT")
  cues <- c("cue_platform_vocab", "cue_marketing_copy", "cue_non_native", "cue_agency_voice", "cue_borrowed_headings", "cue_copy_artefacts")
  list(
    `1_sentences` = list(key = c("profile_code", "pos"), fields = list(move = moves, portability = c("PORTABLE", "SEMI", "NON-PORTABLE"))),
    `1_profiles_language` = list(key = "profile_code", fields = c(setNames(rep(list(c("0", "1")), 6), cues), list(
      segment = c("PREMIUM", "VOLUME", "LOW-PRICE", "MIXED", "NONE"), record_reference = c("PLATFORM", "OFF-PLATFORM", "BOTH", "NONE")))),
    `2_slots` = list(key = c("profile_code", "pos"), fields = list(slot = edits)),
    `2_profiles_structure` = list(key = "profile_code", fields = list(assembly = c("AUTHORED", "FULL-TEMPLATE", "BLOCK-CLONE", "FRAME-AND-FILL", "PATCHWORK"),
      authored_location = c("OPENING", "BODY", "CLOSING", "THROUGHOUT", "NONE"), coherence = c("COHERENT", "SEAMS", "MISFIT"))),
    `3_pairs` = list(key = "pair_code", fields = list(edit_type = edits, fit = c("FIT", "NEUTRAL", "MISFIT")))
  )
}

# Base CSV I/O preserves embedded quote runs on this data.table build; fread/fwrite
# can double them during a round trip. Stimulus identity checks must use literal text.
read_csv_chars <- function(path) as.data.table(read.csv(path, colClasses = "character",
  check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8"))

read_sheet <- function(path, spec, template) {
  if (!file.exists(path)) stop("Missing coded sheet: ", path)
  x <- read_csv_chars(path)
  required <- c(names(template), names(spec$fields))
  if (!all(required %in% names(x))) stop("Missing columns in ", path, ": ", paste(setdiff(required, names(x)), collapse = ", "))
  if (anyNA(x[, spec$key, with = FALSE])) stop("Missing identifiers in ", path)
  if (anyDuplicated(x[, spec$key, with = FALSE])) stop("Duplicate identifiers in ", path)
  unknown <- fsetdiff(x[, spec$key, with = FALSE], template[, spec$key, with = FALSE])
  missing <- fsetdiff(template[, spec$key, with = FALSE], x[, spec$key, with = FALSE])
  if (nrow(unknown)) stop("Unknown identifiers in ", path)
  if (nrow(missing)) stop("Incomplete unit coverage in ", path)
  for (field in names(spec$fields)) {
    x[, (field) := trimws(get(field))]
    if (anyNA(x[[field]]) || any(x[[field]] == "")) stop("Missing ", field, " code in ", path)
    if (any(!x[[field]] %in% spec$fields[[field]])) stop("Invalid ", field, " code in ", path)
  }
  # Preserve every canonical stimulus field, not just identifiers. Notes may be added.
  for (field in setdiff(names(template), spec$key)) {
    chk <- merge(x[, c(spec$key, field), with = FALSE], template[, c(spec$key, field), with = FALSE], by = spec$key, suffixes = c(".coded", ".source"))
    a <- chk[[paste0(field, ".coded")]]; b <- chk[[paste0(field, ".source")]]
    if (any(xor(is.na(a), is.na(b))) || any(a != b, na.rm = TRUE)) stop("Changed stimulus ", field, " in ", path)
  }
  x
}

macro_code <- function(x) fcase(toupper(x) %in% c("IDENTITY", "CREDENTIAL", "CAPABILITY"), "claim-bearing",
  toupper(x) %in% c("CHARACTER", "PROCESS"), "assurance", toupper(x) == "ENCOUNTER", "encounter formula", default = "other")

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  value <- function(key, default) {
    hit <- args[startsWith(args, paste0("--", key, "="))]
    if (!length(hit)) default else sub(paste0("^--", key, "="), "", tail(hit, 1))
  }
  valid <- grepl("^--(codes|out)=.+$", args)
  if (any(!valid)) stop("Unknown argument: ", paste(args[!valid], collapse = " "))
  codes <- value("codes", "qual/coding/julian")
  out <- value("out", file.path(codes, "review"))
  units <- "qual/coding/units"
  if (!dir.exists(codes)) stop("Coding directory does not exist: ", codes)
  if (normalizePath(out, mustWork = FALSE) %in% normalizePath(c(units, codes), mustWork = FALSE)) stop("Output must not overwrite a coding or stimulus directory")
  specs <- coding_specs()
  templates <- lapply(names(specs), function(name) read_csv_chars(file.path(units, paste0(name, ".csv"))))
  names(templates) <- names(specs)
  coded <- lapply(names(specs), function(name) read_sheet(file.path(codes, paste0(name, ".csv")), specs[[name]], templates[[name]]))
  names(coded) <- names(specs)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  write_out <- function(x, name) write.csv(x, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "", fileEncoding = "UTF-8")

  coverage <- rbindlist(lapply(names(specs), function(name) data.table(sheet = name, units = nrow(templates[[name]]),
    coded_units = nrow(coded[[name]]), fields = length(specs[[name]]$fields))))
  write_out(coverage, "coding_coverage")
  counts <- rbindlist(lapply(names(specs), function(name) rbindlist(lapply(names(specs[[name]]$fields), function(field) {
    coded[[name]][, .(n = .N), by = .(code = get(field))][, `:=`(sheet = name, field = field)]
  }))))
  counts[, share := n / sum(n), by = .(sheet, field)]
  setcolorder(counts, c("sheet", "field", "code", "n", "share")); write_out(counts, "code_counts")

  # Descriptive triangulation with computational measurements, not a new estimation model.
  sample <- read_csv_chars("qual/profile_sample_sentences.csv")
  sample[, `:=`(pos = pos_c, maxJ = as.numeric(maxJ), spec_n = as.numeric(spec_n))]
  x <- merge(coded$`1_sentences`, sample[, .(profile_code, pos, maxJ, dict_move = toupper(move), spec_n)], by = c("profile_code", "pos"))
  stopifnot(nrow(x) == nrow(coded$`1_sentences`))
  x[, reuse := fcase(maxJ >= 1, "exact shared", maxJ >= .5, "near-copy", default = "undetected at J >= .5")]
  tab <- x[, .(n = .N), by = .(reuse, move, portability)][, share_within_reuse := n / sum(n), by = reuse]
  write_out(tab, "sentence_content_by_reuse")
  write_out(x[, .(n = .N), by = .(dict_move, coded_move = move)], "dictionary_move_confusion")
  x[, `:=`(dict_function = macro_code(dict_move), coded_function = macro_code(move))]
  write_out(x[, .(n = .N), by = .(dict_function, coded_function)], "dictionary_model1_function_confusion")
  write_out(data.table(comparison = c("eight-way move", "four-way Model 1 function"), n = nrow(x),
    concordance = c(mean(x$dict_move == x$move), mean(x$dict_function == x$coded_function))), "dictionary_concordance")
  write_out(x[, .(n = .N), by = .(lexicon_specific = spec_n > 0, portability)], "specificity_portability_joint")
  write_out(x[dict_move != move, .(profile_code, pos, dict_move, coded_move = move, sentence, note)], "dictionary_disagreement_review")
  flags <- x[, .(n_sentences = .N, n_exact = sum(maxJ >= 1), n_near = sum(maxJ >= .5 & maxJ < 1)), by = profile_code]
  st <- merge(coded$`2_profiles_structure`, flags, by = "profile_code")
  write_out(st[, .(n = .N), by = .(assembly, coherence)], "assembly_coherence")
  # Formula-only reuse sensitivity; these are not hypothesis tests.
  form <- x[, .(any_reuse = any(maxJ >= .5), substantive_reuse = any(maxJ >= .5 & !move %in% c("ENCOUNTER", "OTHER"))), by = profile_code]
  write_out(form[, .(n = .N), by = .(any_reuse, substantive_reuse)], "substantive_reuse_profile_counts")
  view <- readLines(file.path(units, "1_reader.md"), warn = FALSE)
  context <- view[startsWith(view, "- · ")]
  write_out(data.table(context_segments = length(context), context_segments_over_50_words =
    sum(lengths(strsplit(sub("^- · ", "", context), "\\s+")) > 50)), "context_boundary")

  writeLines(c(paste("codes:", codes), "Method: First-author close reading with audited full-context amendments.",
    "The sample is purposive and clustered; counts are not population prevalence estimates.",
    "Dictionary concordance is diagnostic, not independent validation or population classifier accuracy.",
    "Portability concerns truth conditions, not copying barriers. Authored means undetected reuse.",
    "Co-adoption families are not verified sources; provenance, direction and claim truth remain unverified.",
    "No manuscript or estimated models are changed by this review."), file.path(out, "METHOD.txt"))
  cat("Wrote internal coding summaries and measurement diagnostics to", out, "\n")
}

if (sys.nframe() == 0L) main()
