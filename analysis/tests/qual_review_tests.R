# Software QA only; no coder-reliability tests.
# Run in Docker from the project root: analysis/run.sh analysis/tests/qual_review_tests.R
source("analysis/18_qual_review.R")

run_tests <- function() {
  base <- tempfile("qual-tests-"); dir.create(base)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = function(e) e), "error"))
  out <- file.path(base, "review")
  main(paste0("--out=", out))
  cov <- fread(file.path(out, "coding_coverage.csv"))
  counts <- fread(file.path(out, "code_counts.csv"))
  concordance <- fread(file.path(out, "dictionary_concordance.csv"))
  context <- fread(file.path(out, "context_boundary.csv"))
  stopifnot(nrow(cov) == 5L, all(cov$units == cov$coded_units), sum(cov$fields) == 16L,
    cov[sheet == "1_sentences", units] == 1401L, cov[sheet == "2_slots", units] == 100L,
    cov[sheet == "3_pairs", units] == 100L,
    all(abs(counts[, sum(share), by = .(sheet, field)]$V1 - 1) < 1e-12),
    concordance[comparison == "eight-way move", n] == 1401L,
    abs(concordance[comparison == "eight-way move", concordance] - 492/1401) < 1e-12,
    abs(concordance[comparison == "four-way Model 1 function", concordance] - 651/1401) < 1e-12,
    context$context_segments == 594L, context$context_segments_over_50_words == 125L)
  stopifnot(identical(macro_code(c("IDENTITY", "CREDENTIAL", "CAPABILITY", "CHARACTER", "PROCESS", "ENCOUNTER", "AGENCY", "OTHER")),
    c("claim-bearing", "claim-bearing", "claim-bearing", "assurance", "assurance", "encounter formula", "other", "other")))

  # Invalid/blank labels, incomplete coverage, unknown/duplicate identifiers, missing
  # columns and altered stimuli are rejected. All fixtures stay in a temporary directory.
  spec <- coding_specs()$`1_sentences`
  template <- read_csv_chars("qual/coding/units/1_sentences.csv")
  original <- read_csv_chars("qual/coding/julian/1_sentences.csv")
  path <- file.path(base, "fixture.csv")
  fixture <- function(x) write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  fixture(original); stopifnot(nrow(read_sheet(path, spec, template)) == 1401L)
  roundtrip <- read_csv_chars(path)
  stopifnot(identical(original$sentence, roundtrip$sentence))
  quotes <- data.table(text = c('quoted "name"', 'four """" quotes', 'comma, newline\ntext'))
  fixture(quotes); stopifnot(identical(quotes$text, read_csv_chars(path)$text))
  x <- copy(original); x[1, move := "not-a-code"]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- copy(original); x[1, move := NA_character_]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- original[-1]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- copy(original); x[1, profile_code := "P999"]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- rbind(original, original[1]); fixture(x); expect_error(read_sheet(path, spec, template))
  x <- copy(original); x[1, pos := NA_character_]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- copy(original); x[1, sentence := "changed stimulus"]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- copy(original); x[, sentence := NULL]; fixture(x); expect_error(read_sheet(path, spec, template))
  x <- copy(original); x[, move := NULL]; fixture(x); expect_error(read_sheet(path, spec, template))
  expect_error(read_sheet(file.path(base, "absent.csv"), spec, template))
  expect_error(main("--unknown=option"))
  expect_error(main("--out=qual/coding/julian"))
  expect_error(main("--out=qual/coding/units"))
  expect_error(main("--codes=/nonexistent/codes"))
  cat("Qualitative software QA passed: coverage, counts, diagnostics and input validation.\n")
}

run_tests()
