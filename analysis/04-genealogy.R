set.seed(123)

library(data.table)
library(dplyr)
library(igraph)
library(stringr)

cat("=============================================================\n")
cat("04-GENEALOGY: Template diffusion trees, genealogy typing,\n")
cat("              broadcast vs chain vs migratory classification\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading data...\n")
directed      <- readRDS("/home/rstudio/data/directed_pairs.rds")
sentences_df  <- readRDS("/home/rstudio/data/sentences.rds")
profile_stats <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")

cat("Directed edges:", nrow(directed), "\n")
cat("Sentences:     ", nrow(sentences_df), "\n\n")

# Profile covariates needed for genealogy
profiles_dt <- as.data.table(profile_stats)[, .(
  worker, country, primary_occupation, created_at,
  totalHours, totalJobsWorked, has_job
)]

# =============================================================================
# 2. IDENTIFY GENEALOGICALLY TRACEABLE TEMPLATES
#
#    A "template" = a sentence text that appears in >= 50 workers.
#    We use high + medium confidence edges to trace diffusion.
#    For each template sentence, reconstruct the propagation tree:
#      - Root = earliest worker to have that sentence
#      - Edges = directed copy events where that sentence was shared
# =============================================================================

cat("--- 2. IDENTIFYING TRACEABLE TEMPLATES ---\n\n")

# Sentence frequency (from 02-boilerplate output or recompute)
sents_dt <- as.data.table(sentences_df)
sent_freq <- sents_dt[, .(n_workers = uniqueN(worker),
                           workers   = list(unique(worker))),
                      by = sentence_text]

# Apply research-plan threshold: >= 50 workers, sentence > 8 words
sent_freq[, word_count := str_count(sentence_text, "\\S+")]
traceable <- sent_freq[n_workers >= 50 & word_count > 8][order(-n_workers)]

cat("Templates with >= 50 workers and > 8 words:", nrow(traceable), "\n")
cat("Top 20 by prevalence:\n")
for (i in 1:min(20, nrow(traceable))) {
  s <- traceable$sentence_text[i]
  trunc <- if (nchar(s) > 90) paste0(substr(s, 1, 87), "...") else s
  cat(sprintf("  [%2d] n=%3d  \"%s\"\n", i, traceable$n_workers[i], trunc))
}
cat("\n")

# =============================================================================
# 3. BUILD GENEALOGY TREES FOR EACH TRACEABLE TEMPLATE
#
#    For each template sentence T:
#    1. Find all workers who have sentence T
#    2. Among the directed edges between those workers, keep edges where
#       the shared text includes T (i.e. max_jac >= threshold)
#    3. Identify the root: earliest worker (by created_at) with sentence T
#    4. Characterise the tree: depth, branching factor, geographic trajectory
# =============================================================================

cat("--- 3. BUILDING GENEALOGY TREES ---\n\n")

# Work from high + medium confidence directed edges only
dir_hm <- directed[confidence %in% c("high", "medium")]

# Function to build one genealogy tree
build_genealogy <- function(template_text, sent_freq_row, dir_edges, profiles) {

  # Workers with this exact sentence
  workers_with_T <- unlist(sent_freq_row$workers)

  # Dates for those workers
  worker_dates <- profiles[worker %in% workers_with_T,
                            .(worker, created_at, country, primary_occupation,
                              totalHours)]
  if (nrow(worker_dates) < 2) return(NULL)

  # Root: earliest created_at worker
  worker_dates <- worker_dates[order(created_at)]
  root <- worker_dates$worker[1]

  # Edges among workers_with_T
  edges_in_tree <- dir_edges[
    source %in% workers_with_T & copier %in% workers_with_T
  ][, .(source, copier, max_jac, country_source, country_copier,
        occ_source, occ_copier, confidence)]

  if (nrow(edges_in_tree) == 0) return(NULL)

  # Build tree as igraph DAG
  g <- graph_from_data_frame(edges_in_tree[, .(source, copier)],
                              directed = TRUE,
                              vertices = worker_dates[, .(name = worker)])

  n_nodes  <- vcount(g)
  n_edges  <- ecount(g)
  in_degs  <- degree(g, mode = "in")
  out_degs <- degree(g, mode = "out")

  # Branching factor: mean out-degree of non-leaf nodes
  non_leaf_out <- out_degs[out_degs > 0]
  mean_branching <- if (length(non_leaf_out) > 0) mean(non_leaf_out) else 0

  # Depth: longest shortest path from root (approximate via BFS)
  if (root %in% V(g)$name) {
    dists <- distances(g, v = root, mode = "out")
    max_depth <- max(dists[is.finite(dists)])
  } else {
    max_depth <- NA_real_
  }

  # Geographic trajectory
  countries_in_tree <- worker_dates[worker %in% workers_with_T &
                                      !is.na(country)]$country
  n_countries   <- length(unique(countries_in_tree))
  top_country   <- names(sort(table(countries_in_tree), decreasing = TRUE))[1]
  top_ctry_pct  <- round(100 * max(table(countries_in_tree)) /
                           length(countries_in_tree), 1)

  cross_country_edges <- nrow(edges_in_tree[country_source != country_copier &
                                              !is.na(country_source) &
                                              !is.na(country_copier)])
  pct_cross_ctry <- if (nrow(edges_in_tree) > 0)
    round(100 * cross_country_edges / nrow(edges_in_tree), 1) else NA_real_

  # Occupation diversity
  occs_in_tree <- worker_dates$primary_occupation
  # Not directly in worker_dates — get from profiles
  occ_vec <- profiles[worker %in% workers_with_T & !is.na(primary_occupation),
                       primary_occupation]
  n_occs  <- length(unique(occ_vec))

  # Root profile info
  root_info <- profiles[worker == root]

  list(
    template_text  = template_text,
    n_workers      = length(workers_with_T),
    n_edges        = n_edges,
    root           = root,
    root_country   = if (nrow(root_info) > 0) root_info$country[1]   else NA_character_,
    root_occ       = if (nrow(root_info) > 0) root_info$primary_occupation[1] else NA_character_,
    root_hours     = if (nrow(root_info) > 0) root_info$totalHours[1] else NA_real_,
    n_countries    = n_countries,
    top_country    = top_country,
    top_ctry_pct   = top_ctry_pct,
    pct_cross_ctry = pct_cross_ctry,
    n_occs         = n_occs,
    mean_branching = round(mean_branching, 2),
    max_depth      = max_depth,
    n_non_root     = n_nodes - 1
  )
}

cat("Building genealogy trees for", nrow(traceable), "templates...\n")
genealogy_list <- vector("list", nrow(traceable))
for (i in seq_len(nrow(traceable))) {
  genealogy_list[[i]] <- build_genealogy(
    template_text  = traceable$sentence_text[i],
    sent_freq_row  = traceable[i],
    dir_edges      = dir_hm,
    profiles       = profiles_dt
  )
}
genealogy_list <- Filter(Negate(is.null), genealogy_list)
genealogy_dt <- rbindlist(genealogy_list, fill = TRUE)

cat("Trees successfully built:", nrow(genealogy_dt), "\n\n")

# =============================================================================
# 4. CLASSIFY GENEALOGY TYPES
#
#    Broadcast:  high branching factor, low depth (one source -> many)
#    Chain:      low branching factor, high depth (sequential copying)
#    Mixed/Star: intermediate
#
#    Also classify geographic scope:
#    Endemic:    single country (top_ctry_pct > 70%)
#    Migratory:  spans many countries (pct_cross_ctry > 30%)
# =============================================================================

cat("--- 4. CLASSIFYING GENEALOGY TYPES ---\n\n")

genealogy_dt[, tree_type := fcase(
  mean_branching >= 3 & max_depth <= 3,              "broadcast",
  mean_branching <  2 & max_depth >= 4,              "chain",
  mean_branching >= 2 & max_depth >= 4,              "mixed",
  is.na(mean_branching) | is.na(max_depth),          "unclassifiable",
  default =                                           "shallow"
)]

genealogy_dt[, geo_scope := fcase(
  !is.na(top_ctry_pct) & top_ctry_pct >= 70,         "endemic",
  !is.na(pct_cross_ctry) & pct_cross_ctry >= 30,     "migratory",
  default =                                           "mixed_geo"
)]

cat("Tree type distribution:\n")
print(genealogy_dt[, .N, by = tree_type][order(-N)])
cat("\n")

cat("Geographic scope distribution:\n")
print(genealogy_dt[, .N, by = geo_scope][order(-N)])
cat("\n")

cat("Tree type × geographic scope:\n")
print(dcast(genealogy_dt, tree_type ~ geo_scope, value.var = "n_workers",
            fun.aggregate = length))
cat("\n")

# =============================================================================
# 5. DETAILED PROFILES: EXAMPLES PER GENEALOGY TYPE
# =============================================================================

cat("=============================================================\n")
cat("--- 5. EXAMPLE TEMPLATES PER GENEALOGY TYPE ---\n\n")

print_template_profile <- function(row) {
  s <- row$template_text
  trunc <- if (!is.na(s) && nchar(s) > 100) paste0(substr(s, 1, 97), "...") else s
  cat(sprintf("  \"%s\"\n", trunc))
  cat(sprintf("  Workers: %d | Countries: %d | Occs: %d | Tree depth: %s | Branching: %.2f\n",
              row$n_workers, row$n_countries, row$n_occs,
              row$max_depth, row$mean_branching))
  cat(sprintf("  Root: %s | %s | %.0f hours\n",
              row$root_country, row$root_occ, row$root_hours))
  cat(sprintf("  Top country: %s (%.1f%%) | Cross-country edges: %.1f%%\n\n",
              row$top_country, row$top_ctry_pct, row$pct_cross_ctry))
}

for (ttype in c("broadcast", "chain", "mixed", "shallow")) {
  examples <- head(genealogy_dt[tree_type == ttype][order(-n_workers)], 5)
  if (nrow(examples) == 0) next
  cat(sprintf("[ %s ]\n", toupper(ttype)))
  for (i in seq_len(nrow(examples))) print_template_profile(examples[i])
}

# =============================================================================
# 6. ROOT WORKER CHARACTERISTICS
#    Are template originators different from the rest of the population?
# =============================================================================

cat("=============================================================\n")
cat("--- 6. ROOT WORKER CHARACTERISTICS ---\n\n")

root_workers  <- unique(genealogy_dt$root)
all_workers   <- profile_stats$worker

profiles_full <- as.data.table(profile_stats)

root_stats <- profiles_full[worker %in% root_workers, .(
  n               = .N,
  pct_has_job     = round(100 * mean(has_job, na.rm = TRUE), 1),
  median_hours    = round(median(totalHours, na.rm = TRUE), 1),
  mean_hours      = round(mean(totalHours, na.rm = TRUE), 1),
  median_jobs     = round(median(totalJobsWorked, na.rm = TRUE), 1)
)]

nonroot_stats <- profiles_full[!worker %in% root_workers, .(
  n               = .N,
  pct_has_job     = round(100 * mean(has_job, na.rm = TRUE), 1),
  median_hours    = round(median(totalHours, na.rm = TRUE), 1),
  mean_hours      = round(mean(totalHours, na.rm = TRUE), 1),
  median_jobs     = round(median(totalJobsWorked, na.rm = TRUE), 1)
)]

cat("Root workers (template originators):\n")
print(root_stats)
cat("\nAll other workers:\n")
print(nonroot_stats)
cat("\n")

# Country distribution of roots
cat("Country distribution of template roots:\n")
print(profiles_full[worker %in% root_workers, .N, by = country][order(-N)][1:15])
cat("\n")

cat("Occupation distribution of template roots:\n")
print(profiles_full[worker %in% root_workers,
                     .N, by = primary_occupation][order(-N)])
cat("\n")

# =============================================================================
# 7. TEMPORAL SPREAD: HOW FAST DO TEMPLATES PROPAGATE?
# =============================================================================

cat("=============================================================\n")
cat("--- 7. TEMPORAL SPREAD OF TOP TEMPLATES ---\n\n")

# For the 10 largest templates, compute the time from root to 50% adoption
top10_templates <- genealogy_dt[order(-n_workers)][1:10]

for (i in seq_len(nrow(top10_templates))) {
  tmpl  <- top10_templates$template_text[i]
  root_worker <- top10_templates$root[i]
  if (is.na(tmpl)) next

  matching_rows <- which(sents_dt$sentence_text == tmpl)
  workers_with_T <- unique(sents_dt$worker[matching_rows])
  if (length(workers_with_T) < 5) next

  worker_dates <- profiles_dt[worker %in% workers_with_T & !is.na(created_at),
                               .(worker, created_at)][order(created_at)]
  cat(sprintf("     %d workers have created_at\n", nrow(worker_dates)))
  if (nrow(worker_dates) < 5) next

  root_date  <- worker_dates$created_at[1]
  n_total    <- nrow(worker_dates)
  idx_50pct  <- ceiling(n_total * 0.5)
  idx_90pct  <- ceiling(n_total * 0.9)

  days_to_50 <- as.numeric(difftime(worker_dates$created_at[idx_50pct],
                                     root_date, units = "days"))
  days_to_90 <- as.numeric(difftime(worker_dates$created_at[idx_90pct],
                                     root_date, units = "days"))

  trunc <- if (nchar(tmpl) > 80) paste0(substr(tmpl, 1, 77), "...") else tmpl
  cat(sprintf("[%2d] n=%3d  \"%s\"\n", i, n_total, trunc))
  cat(sprintf("     Root: %s (%s) | Days to 50%%: %s | Days to 90%%: %s\n\n",
              top10_templates$root_country[i],
              format(as.Date(root_date), "%Y-%m"),
              if (!is.na(days_to_50)) round(days_to_50) else "NA",
              if (!is.na(days_to_90)) round(days_to_90) else "NA"))
}

# =============================================================================
# 8. SAVE OUTPUTS
# =============================================================================

cat("=============================================================\n")
cat("Saving outputs...\n")

saveRDS(genealogy_dt, "/home/rstudio/data/genealogy_trees.rds")
fwrite(genealogy_dt,  "/home/rstudio/data/genealogy_trees.csv")

cat("Saved:\n")
cat("  data/genealogy_trees.rds/csv — per-template genealogy metrics\n\n")

cat("=============================================================\n")
cat("04-GENEALOGY COMPLETE\n")
cat("=============================================================\n")
