# 07_circulation.R -- Who shares text with whom?
#  (a) homophily by similarity band (occupation, country, city, agency), vs random mixing
#  (b) cohort homophily: do sharers join the platform at the same time and place?
#  (c) status homophily: are sharers more alike in price than matched random pairs?
#  (d) the geography of templates: country-by-country lift
#  (e) precedence from non-overlapping edit intervals (no timestamp heuristics)
#
# Output: tables/circ_*.csv, numbers/07_circulation.csv

source("analysis/00_setup.R")
set.seed(77)
P  <- as.data.table(read_parquet(file.path(DERIVED, "xpairs.parquet")))[score >= 0.3]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, cid)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, specific)]
setkey(W, wid)

# worker pairs: max similarity and whether any exact-shared sentence is generic/specific
P[, spec := C$specific[match(sa, C$sid)]]
WP <- P[, .(maxJ = max(score), exact_generic = any(score >= 1 & !spec), exact_specific = any(score >= 1 & spec)), by = .(w1, w2)]
att <- function(v, col) W[[col]][match(v, W$wid)]
for (col in c("occ1", "country", "city_key", "agency", "cr", "lrate", "tenure_yrs", "ljobs", "up")) {
  WP[, paste0(col, "_1") := att(w1, col)]; WP[, paste0(col, "_2") := att(w2, col)]
}
WP[, band := cut(maxJ, c(0.3, 0.5, 0.7, 0.9, 1, 1.01), right = FALSE, labels = c("[0.3,0.5)", "[0.5,0.7)", "[0.7,0.9)", "[0.9,1)", "exact"))]
rec("worker_pairs_J03", nrow(WP), "worker pairs sharing >=1 sentence pair at J >= 0.3")
rec("worker_pairs_exact", WP[band == "exact", .N], "worker pairs sharing >=1 identical sentence")

# ---- (a) homophily by band ------------------------------------------------------------
H <- rbindlist(lapply(c("occ1", "country", "city_key", "agency"), function(a) {
  WP[, c(list(attribute = a), homophily(get(paste0(a, "_1")), get(paste0(a, "_2")))), by = band]
}))
setorder(H, attribute, band); print(H, digits = 3); save_table(H, "circ_homophily_by_band")
for (a in unique(H$attribute)) for (b in c("[0.3,0.5)", "exact")) {
  rec(paste0("lift_", a, "_", ifelse(b == "exact", "exact", "J03_05")), H[attribute == a & band == b, lift], paste("homophily lift over random mixing:", a, b))
  rec(paste0("r_", a, "_", ifelse(b == "exact", "exact", "J03_05")), H[attribute == a & band == b, r], paste("assortativity r:", a, b))
}
# robustness at exact: exclude agency members; generic-only vs specific-only sharing
E <- WP[band == "exact"]
rob <- rbindlist(list(
  E[, c(list(subset = "all exact pairs"), homophily(occ1_1, occ1_2))][, attribute := "occ1"],
  E[agency_1 == FALSE & agency_2 == FALSE, c(list(subset = "no agency members"), homophily(occ1_1, occ1_2))][, attribute := "occ1"],
  E[exact_generic & !exact_specific, c(list(subset = "generic sentences only"), homophily(occ1_1, occ1_2))][, attribute := "occ1"],
  E[exact_specific == TRUE, c(list(subset = "includes a specific sentence"), homophily(occ1_1, occ1_2))][, attribute := "occ1"],
  E[, c(list(subset = "all exact pairs"), homophily(country_1, country_2))][, attribute := "country"],
  E[agency_1 == FALSE & agency_2 == FALSE, c(list(subset = "no agency members"), homophily(country_1, country_2))][, attribute := "country"],
  E[exact_generic & !exact_specific, c(list(subset = "generic sentences only"), homophily(country_1, country_2))][, attribute := "country"],
  E[exact_specific == TRUE, c(list(subset = "includes a specific sentence"), homophily(country_1, country_2))][, attribute := "country"]))
print(rob, digits = 3); save_table(rob, "circ_homophily_robustness")
rec("lift_occ_exact_generic_only", rob[attribute == "occ1" & subset == "generic sentences only", lift], "occupational lift, pairs sharing only generic sentences")
rec("lift_country_exact_generic_only", rob[attribute == "country" & subset == "generic sentences only", lift], "country lift, pairs sharing only generic sentences")

# ---- (b) cohort homophily: same time and place -------------------------------------------
E[, gap := abs(as.numeric(difftime(cr_1, cr_2, units = "days")))]
draw_same <- function(key, n) { idx <- split(W$wid, W[[key]]); function(k) { v <- idx[[k]]; if (is.null(v) || length(v) < 2) NA_integer_ else v[sample.int(length(v), 1)] } }
coh <- function(sub, key) {
  f <- draw_same(key, 0); x <- sub[sample(.N, min(.N, 200000))]
  x[, rnd := vapply(get(paste0(key, "_2")), f, 1L)]
  x[, gap_null := abs(as.numeric(difftime(cr_1, W$cr[match(rnd, W$wid)], units = "days")))]
  x[, .(pairs = .N, within30_obs = mean(gap <= 30, na.rm = TRUE), within30_null = mean(gap_null <= 30, na.rm = TRUE),
        within7_obs = mean(gap <= 7, na.rm = TRUE), within7_null = mean(gap_null <= 7, na.rm = TRUE))]
}
ch <- rbind(cbind(level = "same city", coh(E[!is.na(city_key_1) & city_key_1 == city_key_2], "city_key")),
            cbind(level = "same country, other city", coh(E[!is.na(country_1) & country_1 == country_2 & city_key_1 != city_key_2], "country")),
            cbind(level = "other country", coh(E[!is.na(country_1) & !is.na(country_2) & country_1 != country_2], "country")))
ch[, `:=`(lift30 = within30_obs / within30_null, lift7 = within7_obs / within7_null)]
print(ch, digits = 3); save_table(ch, "circ_cohort_homophily")
for (i in seq_len(nrow(ch))) { rec(paste0("cohort_lift30_", gsub("[^a-z]", "_", ch$level[i])), ch$lift30[i], "P(created within 30 days of each other) exact-sharing pairs / matched random pairs"); rec(paste0("cohort_lift7_", gsub("[^a-z]", "_", ch$level[i])), ch$lift7[i], "same, within 7 days") }

# ---- (c) status homophily: price similarity vs pairs matched on country x occupation -------
W[, stratum := paste(country, occ1)]
E[, stratum_2 := W$stratum[match(w2, W$wid)]]
f <- draw_same("stratum", 0)
x <- E[!is.na(lrate_1) & !is.na(lrate_2)][sample(.N, min(.N, 300000))]
x[, rnd := vapply(stratum_2, f, 1L)]
x[, lrate_r := W$lrate[match(rnd, W$wid)]]
x <- x[!is.na(lrate_r)]
st <- x[, .(pairs = .N, mad_obs = mean(abs(lrate_1 - lrate_2)), mad_null = mean(abs(lrate_1 - lrate_r)),
           cor_obs = cor(lrate_1, lrate_2), cor_null = cor(lrate_1, lrate_r))]
print(st); save_table(st, "circ_status_homophily")
rec("status_cor_obs", st$cor_obs, "correlation of log asking rates within exact-sharing pairs")
rec("status_cor_null", st$cor_null, "... in pairs where the partner is replaced by a random worker from the same country x occupation")

# ---- (d) geography of templates: country x country lift --------------------------------
top <- W[n_c > 0 & !is.na(country), .N, by = country][order(-N)][1:25, country]
G <- E[country_1 %in% top & country_2 %in% top]
ends <- prop.table(table(c(G$country_1, G$country_2)))
obs <- G[, .N, by = .(a = pmin(country_1, country_2), b = pmax(country_1, country_2))][, share := N / sum(N)]
obs[, expected := ifelse(a == b, ends[a]^2, 2 * ends[a] * ends[b])][, lift := share / expected]
save_table(obs[order(-lift)], "circ_country_pair_lift")
cat("\nStrongest cross-country ties (>= 200 pairs):\n"); print(obs[a != b & N >= 200][order(-lift)][1:20])
cat("\nWithin-country lifts:\n"); print(obs[a == b][order(-lift)])

# ---- (e) precedence from non-overlapping intervals ---------------------------------------
# The current text of a profile was last written within [created_at, updated_at]. If
# updated_at(A) < created_at(B), A carried the sentence before B's profile existed.
W[, prestige := as.numeric(scale(ljobs) + scale(lhours))]
fit <- lm(prestige ~ splines::ns(tenure_yrs, 6), data = W[!is.na(tenure_yrs)])
W[!is.na(tenure_yrs), prest_adj := resid(fit)]
fr <- lm(lrate ~ splines::ns(tenure_yrs, 6) + factor(country) + factor(occ1), data = W[!is.na(lrate) & !is.na(tenure_yrs) & !is.na(country) & !is.na(occ1)])
W[as.integer(names(resid(fr))), lrate_adj := resid(fr)]
E[, `:=`(first = fifelse(up_1 < cr_2, w1, fifelse(up_2 < cr_1, w2, NA_integer_)))]
E[, second := fifelse(first == w1, w2, w1)]
D <- E[!is.na(first)]
rec("precedence_pairs", nrow(D), "exact-sharing worker pairs whose edit intervals do not overlap (order of holding the text is known)")
rec("precedence_pairs_share", nrow(D) / nrow(E), "... share of all exact-sharing worker pairs")
D[, `:=`(pa1 = W$prest_adj[match(first, W$wid)], pa2 = W$prest_adj[match(second, W$wid)], ra1 = W$lrate_adj[match(first, W$wid)], ra2 = W$lrate_adj[match(second, W$wid)])]
# null: random worker pairs with non-overlapping intervals
R <- data.table(a = sample(W$wid, 3e6, TRUE), b = sample(W$wid, 3e6, TRUE))[a != b]
R[, `:=`(up_a = W$up[match(a, W$wid)], cr_b = W$cr[match(b, W$wid)])]
R <- R[!is.na(up_a) & !is.na(cr_b) & up_a < cr_b]
R[, `:=`(pa1 = W$prest_adj[match(a, W$wid)], pa2 = W$prest_adj[match(b, W$wid)], ra1 = W$lrate_adj[match(a, W$wid)], ra2 = W$lrate_adj[match(b, W$wid)])]
pr <- data.table(set = c("exact-sharing pairs", "random pairs, same interval order"),
                 earlier_higher_prestige_adj = c(D[, mean(pa1 > pa2, na.rm = TRUE)], R[, mean(pa1 > pa2, na.rm = TRUE)]),
                 earlier_higher_rate_adj = c(D[, mean(ra1 > ra2, na.rm = TRUE)], R[, mean(ra1 > ra2, na.rm = TRUE)]))
print(pr); save_table(pr, "circ_precedence")
rec("precedence_earlier_higher_prestige_obs", pr$earlier_higher_prestige_adj[1], "share of ordered sharing pairs where the earlier holder has higher tenure-adjusted prestige")
rec("precedence_earlier_higher_prestige_null", pr$earlier_higher_prestige_adj[2], "... random ordered pairs")
rec("precedence_earlier_higher_rate_obs", pr$earlier_higher_rate_adj[1], "share where the earlier holder has the higher adjusted asking rate")
rec("precedence_earlier_higher_rate_null", pr$earlier_higher_rate_adj[2], "... random ordered pairs")
save_numbers("07_circulation")
