# Software/production QA only: not interpretive validation or a new analysis.
# Run in text-reuse-r from the project root after generating publication assets.
suppressPackageStartupMessages({ library(data.table); library(yaml) })
text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
matched <- function(pattern, x) regmatches(x, gregexpr(pattern, x, perl = TRUE))[[1]]
files <- character()
expand <- function(path) {
  stopifnot(file.exists(path), !(path %in% files))
  files <<- c(files, path)
  x <- text(path)
  inc <- matched("\\{\\{< include [^ >]+ >\\}\\}", x)
  if (length(inc)) for (s in inc) {
    p <- sub("^\\{\\{< include ([^ >]+) >\\}\\}$", "\\1", s)
    at <- as.integer(regexpr(s, x, fixed = TRUE))
    x <- paste0(substr(x, 1, at - 1), expand(p), substr(x, at + nchar(s), nchar(x)))
  }
  x
}
body <- expand("PAPER.qmd")
vars <- yaml::read_yaml("_variables.yml")
used <- matched("\\{\\{< var [^ >]+ >\\}\\}", body)
used <- unique(sub("^\\{\\{< var ([^ >]+) >\\}\\}$", "\\1", used))
stopifnot(all(used %in% names(vars)))
copied_vars <- c("m2-copied-gen", "m2-copied-spec", "m2ml-copied-gen", "m2ml-copied-spec",
                 "record-copied-gen-0-jobs", "record-copied-gen-1-9-jobs", "record-copied-gen-10-49-jobs", "record-copied-gen-50--jobs",
                 "rob-strict-copied-spec", "rob-timing-copied-gen-50", "rob-rating-copied-gen-or", "rob-hours-copied-gen")
old_composition_vars <- c("m2-shared-gen", "m2-shared-spec", "m2ml-shared-gen", "m2ml-shared-spec",
                          "record-gen-0-jobs", "record-gen-1-9-jobs", "record-gen-10-49-jobs", "record-gen-50--jobs",
                          "rob-strict-shared-spec", "rob-timing-gen-50", "rob-rating-gen-or", "rob-hours-gen")
stopifnot(all(copied_vars %in% names(vars)), all(copied_vars %in% used), !any(old_composition_vars %in% names(vars)), !any(old_composition_vars %in% used))
for (v in used) body <- gsub(paste0("{{< var ", v, " >}}"), as.character(vars[[v]]), body, fixed = TRUE)
remaining_directives <- matched("\\{\\{<[^>]*>\\}\\}", body)
stopifnot(all(remaining_directives == "{{< pagebreak >}}"))

# Bibliography keys, caption identifiers, and local image dependencies.
bib <- text("references.bib")
keys <- matched("(?m)^@[A-Za-z]+\\{[^,]+,", bib)
keys <- sub("^@[A-Za-z]+\\{([^,]+),$", "\\1", keys)
stopifnot(!anyDuplicated(keys))
citations <- unique(sub("^@", "", matched("(?<![[:alnum:]_.])@[[:alnum:]][[:alnum:]_.:-]*", body)))
refs <- sub("\\.$", "", citations[grepl("^(fig|tbl)-", citations)])
citations <- citations[!grepl("^(fig|tbl)-", citations)]
stopifnot(all(citations %in% keys))
defs <- sub("^\\{#", "", matched("\\{#(?:fig|tbl)-[A-Za-z0-9-]+", body))
stopifnot(!anyDuplicated(defs), all(refs %in% defs))
images <- matched("!\\[[^\\]]*\\]\\([^)]+\\)", body)
paths <- sub("^!\\[[^\\]]*\\]\\(([^)]+)\\)$", "\\1", images, perl = TRUE)
stopifnot(length(paths) >= 5, all(file.exists(paths)))
stopifnot(file.exists("apa.csl"), file.exists("APPENDIX.qmd"))
stopifnot(!grepl("~[[:xdigit:]]{8,}", body), !grepl("\\bP[0-9]{3}\\b", body, perl = TRUE))
stopifnot(!grepl("GenAI|generative artificial intelligence|TODO", body, ignore.case = TRUE))

# PAPER.qmd is the only project input, so bare `quarto preview` opens PAPER.html directly.
project <- yaml::read_yaml("_quarto.yml")$project
stopifnot(identical(project$render, "PAPER.qmd"), !file.exists("index.qmd"))

# All publication inputs still match the deterministic asset build snapshot.
snap <- fread("analysis/output/paper/input_snapshot.csv")
stopifnot(all(file.exists(snap$path)), identical(unname(tools::md5sum(snap$path)), snap$md5))
num <- fread("analysis/output/paper/number_audit.csv")
stopifnot(!anyDuplicated(num$variable), all(used %in% num$variable))
stopifnot(all(vapply(seq_len(nrow(num)), function(i) identical(as.character(vars[[num$variable[i]]]), num$rendered[i]), TRUE)))

# Reported coefficient strings must come from the saved estimates.
beta <- function(b, s) sprintf("%.3f (%.3f)", b, s)
m1 <- fread("analysis/output/tables/model1_coefficients.csv")[model == "M1 main (FE logit)"]
t1 <- text("analysis/output/paper/tables/model1.qmd")
stopifnot(all(vapply(seq_len(nrow(m1)), function(i) grepl(beta(m1$est[i], m1$se[i]), t1, fixed = TRUE), TRUE)))
m2 <- fread("analysis/output/tables/model2_fixed_effects_clustered.csv")
ml <- fread("analysis/output/tables/model2_fixed_effects_multilevel.csv")
t2 <- text("analysis/output/paper/tables/model2.qmd")
stopifnot(all(c("shared_gen", "shared_spec") %in% m2$term),
          all(c("shared_gen", "shared_spec", "authored_spec:d1", "authored_spec:d2", "authored_spec:d3", "d1:shared_any", "d2:shared_any", "d3:shared_any") %in% ml$term),
          grepl("Copied-generic share", t2, fixed = TRUE), grepl("Copied-specific share", t2, fixed = TRUE))
slopes <- fread("analysis/output/tables/model2_slopes_by_record.csv")
stopifnot(all(c("shared_gen_est", "shared_gen_se", "shared_spec_est", "shared_spec_se") %in% names(slopes)),
          grepl("Copied-generic", text("analysis/output/paper/tables/model2_record_slopes.qmd"), fixed = TRUE),
          grepl("Copied-specific", text("analysis/output/paper/tables/model2_record_slopes.qmd"), fixed = TRUE))
rob <- fread("analysis/output/tables/robustness_estimates.csv")
rob_summary <- fread("analysis/output/tables/robustness_summary.csv")
stopifnot(all(c("m2_shared_gen", "m2_shared_spec", "m2_shared_gen_0jobs", "m2_shared_gen_50plus") %in% names(rob)),
          all(c("P2b_commons_discount_generic", "P2b_shared_specific_discount") %in% names(rob_summary)),
          grepl("Copied-generic", text("analysis/output/paper/tables/robustness.qmd"), fixed = TRUE),
          grepl("Copied-specific", text("analysis/output/paper/tables/robustness.qmd"), fixed = TRUE))
m4 <- fread("analysis/output/tables/sig_M4_composition.csv")
stopifnot(all(c("sh_gen", "sh_spec", "or_spec") %in% m4$term))
cache_files <- list.files("analysis/output/tables/robustness_parts", pattern = "^check_[0-9]{2}\\.csv$", full.names = TRUE)
stopifnot(length(cache_files) == 11L,
          all(vapply(cache_files, function(f) all(c("m2_shared_gen", "m2_shared_spec", "m2_shared_gen_50plus") %in% names(fread(f))), TRUE)))
stopifnot(all(vapply(seq_len(nrow(m2)), function(i) grepl(beta(m2$est[i], m2$se[i]), t2, fixed = TRUE), TRUE)))
stopifnot(all(vapply(seq_len(nrow(ml)), function(i) grepl(beta(ml$est[i], ml$se[i]), t2, fixed = TRUE), TRUE)))

# Retained age control and separate experience measures must be explicit.
spec <- text("analysis/13_model2_valuation.R")
stopifnot(grepl("splines::ns(tenure_yrs, 5)", spec, fixed = TRUE),
          grepl("record + ljobs + lhours", spec, fixed = TRUE),
          grepl("five-degree-of-freedom", body, fixed = TRUE),
          grepl("not worker tenure", body, fixed = TRUE))

# Publication excerpts exist (apart from case and punctuation) in their saved source units and verbatim in the text.
# Exact excerpts sit in exact-shared units, variants at J in [.5, 1); pair excerpts sit in the sampled near-copy pairs.
q <- fread("analysis/output/paper/quotation_audit.csv")
S <- as.data.table(read.csv("qual/profile_sample_sentences.csv", stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"))
NC <- as.data.table(read.csv("qual/near_copy_pairs.csv", stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"))
norm <- function(x) trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
plain <- gsub("*", "", body, fixed = TRUE)
for (i in seq_len(nrow(q))) {
  if (q$source[i] == "sentence") {
    z <- S[profile_code == q$internal_case[i] & pos_c == q$pos[i]]
    stopifnot(nrow(z) == 1, grepl(norm(q$quotation[i]), norm(z$text), fixed = TRUE),
              if (q$kind[i] == "exact") z$maxJ == 1 else z$maxJ >= .5 && z$maxJ < 1)
  } else {
    z <- NC[pair_code == q$internal_case[i]]
    stopifnot(nrow(z) == 1, any(vapply(c(z$sentence_a, z$sentence_b), function(x) grepl(norm(q$quotation[i]), norm(x), fixed = TRUE), TRUE)))
  }
  stopifnot(grepl(q$quotation[i], plain, fixed = TRUE))
}
stopifnot(!grepl("Illustrations? [A-F][0-9]?\\b", body, perl = TRUE))
writeLines(c("Manuscript software QA passed.",
             paste0("Source files expanded: ", length(files)),
             paste0("Numerical variables resolved: ", length(used)),
             paste0("Citation keys found: ", length(citations)),
             paste0("Caption identifiers: ", length(defs)),
             paste0("Local images: ", length(paths)),
             paste0("Saved coefficients, input hashes, retained control specification, and ", nrow(q), " excerpts checked."),
             "Project preview entry and manuscript redirect target checked.",
             "This is production QA, not interpretive reliability or causal validation."),
           "analysis/output/paper/manuscript_checks.txt")
cat("Manuscript software QA passed: variables, citations, references, assets, coefficients, controls and quotations.\n")
