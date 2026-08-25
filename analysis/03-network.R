set.seed(123)

library(data.table)
library(dplyr)
library(igraph)

cat("=============================================================\n")
cat("03-NETWORK (revised): Direction confidence, composite prestige,\n")
cat("  prestige gradient, threshold-sensitivity homophily,\n")
cat("  degree-stratified homophily, Louvain community detection\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading data...\n")
pairs_sent    <- readRDS("/home/rstudio/data/pairs_workers_30.rds")
profile_stats <- readRDS("/home/rstudio/data/profile_reuse_stats.rds")

cat("Sentence-level pairs (J>=0.3):", nrow(pairs_sent), "\n")
cat("Profiles:", nrow(profile_stats), "\n\n")

# =============================================================================
# 2. AGGREGATE TO PROFILE-LEVEL PAIRS
# =============================================================================

cat("--- 2. AGGREGATING TO PROFILE-LEVEL PAIRS ---\n\n")

pairs_dt <- as.data.table(pairs_sent)
rm(pairs_sent); gc()

profile_pairs <- pairs_dt[, .(
  n_shared   = .N,
  max_jac    = max(score),
  mean_jac   = mean(score),
  n_exact    = sum(score == 1.0)
), by = .(worker_a, worker_b)]

cat("Profile-level dyads (J>=0.3):", nrow(profile_pairs), "\n")
cat("Dyads by max Jaccard threshold:\n")
for (t in c(0.3, 0.5, 0.7, 1.0))
  cat(sprintf("  J >= %.1f : %s dyads\n", t,
              format(nrow(profile_pairs[max_jac >= t]), big.mark = ",")))
cat("\n")

rm(pairs_dt); gc()

# =============================================================================
# 3. COMPOSITE PRESTIGE MEASURE
#    Primary:   log1p(totalHours)
#    Secondary: log1p(totalJobsWorked)
#    Composite: z(log1p(totalHours)) + z(log1p(totalJobsWorked))
# =============================================================================

cat("--- 3. COMPOSITE PRESTIGE MEASURE ---\n\n")

profiles_dt <- as.data.table(profile_stats)[, .(
  worker, country, primary_occupation,
  totalHours, totalJobsWorked, rating, has_job,
  created_at, updated_at
)]

# Build prestige vars
profiles_dt[, prestige_hours   := log1p(totalHours)]
profiles_dt[, prestige_jobs    := log1p(totalJobsWorked)]
profiles_dt[, prestige_hours_z := scale(prestige_hours)[, 1]]
profiles_dt[, prestige_jobs_z  := scale(prestige_jobs)[, 1]]
profiles_dt[, prestige_composite := prestige_hours_z + prestige_jobs_z]

cat("log1p(totalHours) distribution:\n")
print(round(quantile(profiles_dt$prestige_hours,
                     probs = c(0, .1, .25, .5, .75, .9, .99, 1), na.rm = TRUE), 2))
cat("\n")

# =============================================================================
# 4. JOIN COVARIATES AND ASSIGN THREE-TIER DIRECTION CONFIDENCE
#
#    HIGH:   |created_at gap| > 90 days  AND  updated_at_copier > updated_at_source
#    MEDIUM: |created_at gap| > 90 days  AND  updated_at signals absent or inconsistent
#    LOW:    |created_at gap| <= 90 days  -> use prestige as tiebreaker; flag uncertain
#    NA_DIR: created_at missing for either worker
# =============================================================================

cat("--- 4. THREE-TIER DIRECTION CONFIDENCE ---\n\n")

# Join covariates
pp <- profile_pairs %>%
  left_join(profiles_dt, by = c("worker_a" = "worker")) %>%
  rename_with(~ paste0(., "_a"),
              c(country, primary_occupation, totalHours, totalJobsWorked,
                has_job, created_at, updated_at,
                prestige_hours, prestige_jobs, prestige_composite)) %>%
  left_join(profiles_dt, by = c("worker_b" = "worker")) %>%
  rename_with(~ paste0(., "_b"),
              c(country, primary_occupation, totalHours, totalJobsWorked,
                has_job, created_at, updated_at,
                prestige_hours, prestige_jobs, prestige_composite))

pp <- as.data.table(pp)

# Temporal gap (positive = B joined after A, i.e. A is earlier)
pp[, gap_days := as.numeric(difftime(created_at_b, created_at_a, units = "days"))]

# updated_at consistency: does copier's profile update come after source's?
# Only relevant when gap_days > 0 (A is tentative source)
pp[, upd_consistent_a_src :=
     !is.na(updated_at_a) & !is.na(updated_at_b) & updated_at_b > updated_at_a]
pp[, upd_consistent_b_src :=
     !is.na(updated_at_a) & !is.na(updated_at_b) & updated_at_a > updated_at_b]

# Assign direction + confidence
pp[, `:=`(
  source = NA_character_,
  copier = NA_character_,
  confidence = NA_character_
)]

# A joined > 90 days before B
pp[gap_days > 90,  source := worker_a]
pp[gap_days > 90,  copier := worker_b]
pp[gap_days > 90 &  upd_consistent_a_src, confidence := "high"]
pp[gap_days > 90 & !upd_consistent_a_src, confidence := "medium"]

# B joined > 90 days before A
pp[gap_days < -90, source := worker_b]
pp[gap_days < -90, copier := worker_a]
pp[gap_days < -90 &  upd_consistent_b_src, confidence := "high"]
pp[gap_days < -90 & !upd_consistent_b_src, confidence := "medium"]

# Small gap (<= 90 days): use prestige composite as tiebreaker
pp[abs(gap_days) <= 90 & !is.na(gap_days), `:=`(
  source     = fifelse(prestige_composite_a >= prestige_composite_b,
                       worker_a, worker_b),
  copier     = fifelse(prestige_composite_a >= prestige_composite_b,
                       worker_b, worker_a),
  confidence = "low"
)]

# Missing created_at
pp[is.na(gap_days), confidence := "undirected"]

cat("Direction confidence breakdown:\n")
print(pp[, .N, by = confidence][order(-N)])
cat("\n")

# Build directed frame with prestige vars for source and copier
directed <- pp[confidence != "undirected", .(
  source, copier, n_shared, max_jac, n_exact, confidence, gap_days,
  country_source     = fifelse(source == worker_a, country_a,    country_b),
  country_copier     = fifelse(source == worker_a, country_b,    country_a),
  occ_source         = fifelse(source == worker_a, primary_occupation_a, primary_occupation_b),
  occ_copier         = fifelse(source == worker_a, primary_occupation_b, primary_occupation_a),
  prestige_src       = fifelse(source == worker_a, prestige_composite_a, prestige_composite_b),
  prestige_cop       = fifelse(source == worker_a, prestige_composite_b, prestige_composite_a),
  hours_src          = fifelse(source == worker_a, prestige_hours_a, prestige_hours_b),
  hours_cop          = fifelse(source == worker_a, prestige_hours_b, prestige_hours_a),
  jobs_src           = fifelse(source == worker_a, prestige_jobs_a, prestige_jobs_b),
  jobs_cop           = fifelse(source == worker_a, prestige_jobs_b, prestige_jobs_a),
  has_job_src        = fifelse(source == worker_a, has_job_a, has_job_b),
  has_job_cop        = fifelse(source == worker_a, has_job_b, has_job_a)
)]

directed[, `:=`(
  same_country = !is.na(country_source) & !is.na(country_copier) &
                  country_source == country_copier,
  same_occ     = !is.na(occ_source) & !is.na(occ_copier) &
                  occ_source == occ_copier,
  delta_prestige = prestige_src - prestige_cop,
  delta_hours    = hours_src    - hours_cop,
  delta_jobs     = jobs_src     - jobs_cop
)]

cat("Total directed edges (all confidence levels):", nrow(directed), "\n")
cat("High-confidence edges:", nrow(directed[confidence == "high"]), "\n\n")

# =============================================================================
# 5. PRESTIGE GRADIENT ANALYSIS
# =============================================================================

cat("=============================================================\n")
cat("--- 5. PRESTIGE GRADIENT ANALYSIS ---\n\n")

report_gradient <- function(df, label) {
  dp <- df$delta_prestige[is.finite(df$delta_prestige)]
  dh <- df$delta_hours[is.finite(df$delta_hours)]
  if (length(dp) < 2) { cat("  ", label, ": insufficient data\n\n"); return() }
  tt <- t.test(dp, mu = 0)
  cat(sprintf("  %s  (n = %s edges)\n", label, format(nrow(df), big.mark = ",")))
  cat(sprintf("    delta_prestige_composite: mean=%.3f  median=%.3f  >0: %.1f%%  <0: %.1f%%  p=%.4g\n",
              mean(dp), median(dp), 100*mean(dp>0), 100*mean(dp<0), tt$p.value))
  th <- t.test(dh, mu = 0)
  cat(sprintf("    delta_log1p_hours:        mean=%.3f  median=%.3f  >0: %.1f%%  <0: %.1f%%  p=%.4g\n",
              mean(dh), median(dh), 100*mean(dh>0), 100*mean(dh<0), th$p.value))
  cat("\n")
}

cat("5a. High-confidence edges only (main result):\n")
report_gradient(directed[confidence == "high"],                         "All high-conf")
report_gradient(directed[confidence == "high" & same_country == TRUE],  "High-conf, within-country")
report_gradient(directed[confidence == "high" & same_country == FALSE], "High-conf, cross-country")
report_gradient(directed[confidence == "high" & same_occ == TRUE],      "High-conf, within-occ")
report_gradient(directed[confidence == "high" & same_occ == FALSE],     "High-conf, cross-occ")
report_gradient(directed[confidence == "high" & same_country & same_occ],  "High-conf, within both")
report_gradient(directed[confidence == "high" & !(same_country & same_occ)], "High-conf, cross at least one")

cat("5b. Robustness check — medium-confidence edges:\n")
report_gradient(directed[confidence == "medium"],                         "All medium-conf")
report_gradient(directed[confidence == "medium" & same_country == TRUE],  "Medium-conf, within-country")
report_gradient(directed[confidence == "medium" & same_country == FALSE], "Medium-conf, cross-country")

cat("5c. Prestige gradient within top countries (high-confidence, within-country):\n")
top_countries <- directed[confidence == "high" & same_country == TRUE,
                           .N, by = country_source][order(-N)][1:8, country_source]
grad_country <- directed[confidence == "high" & same_country & country_source %in% top_countries &
                           is.finite(delta_prestige), .(
  n_edges          = .N,
  mean_delta_pres  = round(mean(delta_prestige), 3),
  pct_src_higher   = round(100 * mean(delta_prestige > 0), 1),
  mean_delta_hours = round(mean(delta_hours, na.rm = TRUE), 3)
), by = country_source][order(-mean_delta_pres)]
print(grad_country)
cat("\n")

# =============================================================================
# 6. HOMOPHILY AT MULTIPLE J THRESHOLDS
# =============================================================================

cat("=============================================================\n")
cat("--- 6. HOMOPHILY ACROSS J THRESHOLDS ---\n\n")

# Expected within-group fractions under random mixing
exp_country <- profiles_dt[!is.na(country),
                            sum((.N / nrow(profiles_dt[!is.na(country)]))^2),
                            by = country][, sum(V1)]
exp_occ     <- profiles_dt[!is.na(primary_occupation),
                            sum((.N / nrow(profiles_dt[!is.na(primary_occupation)]))^2),
                            by = primary_occupation][, sum(V1)]

# Function: assortativity coefficient for a set of directed edges
assort_r <- function(p_obs, p_exp) (p_obs - p_exp) / (1 - p_exp)

cat(sprintf("Expected same-country (random):    %.1f%%\n", 100 * exp_country))
cat(sprintf("Expected same-occupation (random): %.1f%%\n\n", 100 * exp_occ))

cat(sprintf("%-8s  %-10s  %-6s  %-6s  %-8s  %-8s  %-8s  %-8s\n",
            "J_thresh", "n_edges", "p_ctry", "r_ctry", "p_occ", "r_occ",
            "p_both", "r_both"))
cat(strrep("-", 72), "\n")

thresholds <- c(0.3, 0.5, 0.7, 1.0)
homophily_rows <- list()

for (t in thresholds) {
  sub <- directed[max_jac >= t]
  if (nrow(sub) == 0) next
  p_c  <- mean(sub$same_country, na.rm = TRUE)
  p_o  <- mean(sub$same_occ,     na.rm = TRUE)
  p_b  <- mean(sub$same_country & sub$same_occ, na.rm = TRUE)
  exp_b <- exp_country * exp_occ
  cat(sprintf("%-8.2f  %-10s  %-6.3f  %-6.4f  %-8.3f  %-8.4f  %-8.3f  %-8.4f\n",
              t, format(nrow(sub), big.mark = ","),
              p_c, assort_r(p_c, exp_country),
              p_o, assort_r(p_o, exp_occ),
              p_b, assort_r(p_b, exp_b)))
  homophily_rows[[length(homophily_rows)+1]] <- data.table(
    threshold = t, n_edges = nrow(sub),
    p_same_country = p_c, r_country = assort_r(p_c, exp_country),
    p_same_occ = p_o, r_occ = assort_r(p_o, exp_occ)
  )
}
homophily_thresholds <- rbindlist(homophily_rows)
cat("\n")

# =============================================================================
# 7. DEGREE-STRATIFIED HOMOPHILY
# =============================================================================

cat("=============================================================\n")
cat("--- 7. DEGREE-STRATIFIED HOMOPHILY ---\n\n")

# Compute total (in + out) degree for copiers in the full directed network
copier_out <- directed[, .(out_deg = .N), by = copier]
source_in  <- directed[, .(in_deg  = .N), by = source]

node_deg <- merge(
  copier_out, source_in,
  by.x = "copier", by.y = "source", all = TRUE
)
node_deg[is.na(out_deg), out_deg := 0]
node_deg[is.na(in_deg),  in_deg  := 0]
node_deg[, total_deg := out_deg + in_deg]
setnames(node_deg, "copier", "worker")

deg_breaks <- c(0, 1, 5, 20, 100, Inf)
deg_labels <- c("0", "1-4", "5-19", "20-99", "100+")
node_deg[, deg_tier := cut(total_deg, breaks = deg_breaks,
                            labels = deg_labels,
                            right = FALSE, include.lowest = TRUE)]

cat("Node degree distribution by tier:\n")
print(node_deg[, .N, by = deg_tier][order(deg_tier)])
cat("\n")

# Join copier degree tier onto directed edges
directed_deg <- merge(directed, node_deg[, .(worker, deg_tier_copier = deg_tier)],
                      by.x = "copier", by.y = "worker", all.x = TRUE)

cat("Homophily by copier degree tier (all J>=0.3):\n")
cat(sprintf("  %-10s  %-10s  %-10s  %-8s  %-10s  %-8s\n",
            "deg_tier", "n_edges", "p_ctry", "r_ctry", "p_occ", "r_occ"))
cat(strrep("-", 62), "\n")
deg_homophily_rows <- list()
for (tier in deg_labels) {
  sub <- directed_deg[deg_tier_copier == tier]
  if (nrow(sub) == 0) next
  p_c <- mean(sub$same_country, na.rm = TRUE)
  p_o <- mean(sub$same_occ,     na.rm = TRUE)
  cat(sprintf("  %-10s  %-10s  %-10.3f  %-8.4f  %-10.3f  %-8.4f\n",
              tier, format(nrow(sub), big.mark = ","),
              p_c, assort_r(p_c, exp_country),
              p_o, assort_r(p_o, exp_occ)))
  deg_homophily_rows[[length(deg_homophily_rows)+1]] <- data.table(
    deg_tier = tier, n_edges = nrow(sub),
    p_country = p_c, r_country = assort_r(p_c, exp_country),
    p_occ = p_o, r_occ = assort_r(p_o, exp_occ)
  )
}
deg_homophily <- rbindlist(deg_homophily_rows)
cat("\n")

# =============================================================================
# 8. LOUVAIN COMMUNITY DETECTION
# =============================================================================

cat("=============================================================\n")
cat("--- 8. LOUVAIN COMMUNITY DETECTION ---\n\n")

cat("Building undirected graph from all directed edges (J>=0.3)...\n")

# Canonical undirected edges (merge both directions, keep max weight)
edges_undir <- rbind(
  directed[, .(u = source, v = copier, w = max_jac)],
  directed[, .(u = copier, v = source, w = max_jac)]
)[, .(w = max(w)), by = .(u = pmin(u, v), v = pmax(u, v))]

g_undir <- graph_from_data_frame(
  d        = edges_undir[, .(u, v, weight = w)],
  directed = FALSE
)
cat("Nodes:", vcount(g_undir), "  Edges:", ecount(g_undir), "\n\n")

cat("Running Louvain community detection (may take a few minutes)...\n")
t_start <- proc.time()
louvain <- cluster_louvain(g_undir, weights = E(g_undir)$weight)
t_elapsed <- proc.time() - t_start
cat(sprintf("Done in %.1f seconds.\n", t_elapsed["elapsed"]))
cat("Number of communities detected:", length(louvain), "\n")
cat("Modularity:", round(modularity(louvain), 4), "\n\n")

# Community sizes
comm_sizes <- sort(sizes(louvain), decreasing = TRUE)
cat("Community size distribution (top 20):\n")
print(head(comm_sizes, 20))
cat(sprintf("\nCommunities with >= 100 members: %d\n",
            sum(comm_sizes >= 100)))
cat(sprintf("Communities with >= 1000 members: %d\n\n",
            sum(comm_sizes >= 1000)))

# ---- Composition of top communities ----
# Map node names to membership
membership_dt <- data.table(
  worker     = V(g_undir)$name,
  community  = membership(louvain)
)

# Join with profile covariates
comm_profiles <- merge(membership_dt,
                       profiles_dt[, .(worker, country, primary_occupation)],
                       by = "worker", all.x = TRUE)

# For each community: size, top country, top occupation, concentration
top_comms <- comm_profiles[, .(
  size            = .N,
  top_country     = names(sort(table(country[!is.na(country)]), decreasing = TRUE))[1],
  top_country_pct = round(100 * max(table(country[!is.na(country)])) /
                            sum(!is.na(country)), 1),
  top_occ         = names(sort(table(primary_occupation[!is.na(primary_occupation)]),
                               decreasing = TRUE))[1],
  top_occ_pct     = round(100 * max(table(primary_occupation[!is.na(primary_occupation)])) /
                             sum(!is.na(primary_occupation)), 1)
), by = community][order(-size)]

cat("Top 25 communities by size:\n")
print(top_comms[1:min(25, .N)])
cat("\n")

# ---- NMI: how well do communities align with country / occupation? ----
cat("NMI between Louvain communities and a priori labels:\n")

shared_nodes <- comm_profiles[!is.na(country) & !is.na(primary_occupation)]
if (nrow(shared_nodes) > 0) {
  nmi_country <- igraph::compare(
    as.integer(as.factor(shared_nodes$community)),
    as.integer(as.factor(shared_nodes$country)),
    method = "nmi"
  )
  nmi_occ <- igraph::compare(
    as.integer(as.factor(shared_nodes$community)),
    as.integer(as.factor(shared_nodes$primary_occupation)),
    method = "nmi"
  )
  cat(sprintf("  NMI(communities, country):    %.4f\n", nmi_country))
  cat(sprintf("  NMI(communities, occupation): %.4f\n", nmi_occ))
  cat("  (0 = no alignment; 1 = perfect alignment)\n\n")
}

# ---- Within-community country/occupation concentration ----
cat("Distribution of top_country_pct across communities (>= 50 members):\n")
print(round(quantile(top_comms[size >= 50, top_country_pct],
                     probs = c(0, .25, .5, .75, .9, 1)), 1))
cat("Distribution of top_occ_pct across communities (>= 50 members):\n")
print(round(quantile(top_comms[size >= 50, top_occ_pct],
                     probs = c(0, .25, .5, .75, .9, 1)), 1))
cat("\n")

# =============================================================================
# 9. SAVE OUTPUTS
# =============================================================================

cat("=============================================================\n")
cat("Saving outputs...\n")

saveRDS(directed,             "/home/rstudio/data/directed_pairs.rds")
fwrite(directed,              "/home/rstudio/data/directed_pairs.csv")
fwrite(homophily_thresholds,  "/home/rstudio/data/homophily_by_threshold.csv")
fwrite(deg_homophily,         "/home/rstudio/data/homophily_by_degree.csv")
fwrite(top_comms,             "/home/rstudio/data/louvain_communities.csv")
fwrite(membership_dt,         "/home/rstudio/data/louvain_membership.csv")
saveRDS(g_undir,              "/home/rstudio/data/copy_network_undir.rds")
fwrite(grad_country,          "/home/rstudio/data/prestige_gradient_by_country.csv")

cat("Saved:\n")
cat("  data/directed_pairs.rds/csv\n")
cat("  data/homophily_by_threshold.csv\n")
cat("  data/homophily_by_degree.csv\n")
cat("  data/louvain_communities.csv\n")
cat("  data/louvain_membership.csv\n")
cat("  data/copy_network_undir.rds\n")
cat("  data/prestige_gradient_by_country.csv\n\n")

cat("=============================================================\n")
cat("03-NETWORK COMPLETE\n")
cat("=============================================================\n")
