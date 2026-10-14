# 05_scripts.R -- The ecology of circulating sentences ("scripts" = exact-match clusters).
#  (a) scope: occupational and geographic concentration of each script's adopters,
#      relative to the population (Simpson concentration ratio);
#  (b) template families: scripts that travel together (co-adoption), via components;
#  (c) timelines: adoption dates from profiles never edited after their creation day.
#
# Output: data/derived/script_attrs.parquet, tables/scripts_*.csv, numbers/05_scripts.csv

source("analysis/00_setup.R")
set.seed(55)
SC <- as.data.table(read_parquet(file.path(DERIVED, "scripts.parquet")))
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[!is.na(cid), .(sid, wid, cid)]
C  <- as.data.table(read_parquet(file.path(DERIVED, "sent_content.parquet")))[, .(sid, move, specific, spec_n)]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))
Wf <- W[n_c > 0]
AD <- unique(merge(SR, W[, .(wid, occ1, country, city_key, agency, text_dated, cr, tenure_yrs, lrate, totalJobsWorked)], by = "wid"), by = c("cid", "wid"))

# ---- (a) scope ---------------------------------------------------------------------
simpson <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 2) return(NA_real_); t <- table(x); sum(t * (t - 1)) / (n * (n - 1)) }
base_occ <- sum(prop.table(table(Wf$occ1))^2)
base_cty <- sum(prop.table(table(Wf$country))^2)
base_city <- sum(prop.table(table(Wf$city_key))^2)
rec("baseline_same_occ", base_occ, "P(two random reuse-frame workers share first-listed occupation)")
rec("baseline_same_country", base_cty, "P(two random reuse-frame workers share country)")
rec("baseline_same_city", base_city, "P(two random reuse-frame workers share city)")
SA <- AD[, .(occ_conc = simpson(occ1) / base_occ, cty_conc = simpson(country) / base_cty, city_conc = simpson(city_key) / base_city,
             n_countries = uniqueN(country[!is.na(country)]), top_country = names(which.max(table(country))),
             top_country_share = max(prop.table(table(country))), top_occ = names(which.max(table(occ1))),
             agency_share = mean(agency, na.rm = TRUE), dated_adopters = sum(text_dated, na.rm = TRUE),
             first_created = min(cr, na.rm = TRUE), med_tenure = med(tenure_yrs), zero_job_share = mean(totalJobsWorked == 0)), by = cid]
SA <- merge(SC[, .(cid, adopters, words, text, rep_sid)], SA, by = "cid")
SA <- merge(SA, C[, .(rep_sid = sid, move, specific, spec_n)], by = "rep_sid")
SA[, `:=`(occ_scope = fifelse(occ_conc >= 2, "occupation-bound", "cross-occupational"), geo_scope = fifelse(cty_conc >= 2, "local", "translocal"))]
big <- SA[adopters >= 20]
rec("scripts_ge20", nrow(big), "scripts with >=20 adopters")
ty <- big[, .(scripts = .N, adoptions = sum(adopters)), by = .(occ_scope, geo_scope)][, adoption_share := adoptions / sum(adoptions)][order(-adoptions)]
print(ty); save_table(ty, "scripts_scope_typology")
sp <- big[, .(scripts = .N, adoptions = sum(adopters), med_occ_conc = med(occ_conc), share_occ_bound = weighted.mean(occ_scope == "occupation-bound", adopters),
              med_cty_conc = med(cty_conc), share_local = weighted.mean(geo_scope == "local", adopters)), by = .(specific)]
print(sp); save_table(sp, "scripts_scope_by_specificity")
rec("occbound_share_specific_scripts", sp[specific == TRUE, share_occ_bound], "adoption-weighted share occupation-bound, scripts with occupation-informative words (>=20 adopters)")
rec("occbound_share_generic_scripts", sp[specific == FALSE, share_occ_bound], "... scripts without occupation-informative words")
rec("local_share_specific_scripts", sp[specific == TRUE, share_local], "adoption-weighted share local (country conc >= 2), specific scripts")
rec("local_share_generic_scripts", sp[specific == FALSE, share_local], "... generic scripts")
mv <- big[, .(scripts = .N, adoptions = sum(adopters), share_occ_bound = weighted.mean(occ_scope == "occupation-bound", adopters), share_local = weighted.mean(geo_scope == "local", adopters)), by = move][order(-adoptions)]
print(mv); save_table(mv, "scripts_scope_by_move")
# generic scripts that are nevertheless occupation-bound: residual occupational channelling
gb <- big[specific == FALSE][order(-occ_conc)][1:15, .(adopters, occ_conc = round(occ_conc, 1), top_occ, cty_conc = round(cty_conc, 1), top_country, text = substr(text, 1, 100))]
print(gb); save_table(gb, "scripts_generic_but_occupation_bound")
# strongly local scripts
lc <- big[order(-cty_conc)][1:20, .(adopters, cty_conc = round(cty_conc, 1), top_country, top_country_share = round(top_country_share, 2), city_conc = round(city_conc, 1), agency_share = round(agency_share, 2), text = substr(text, 1, 100))]
print(lc); save_table(lc, "scripts_most_local")

# ---- (b) template families: scripts that travel together --------------------------------
A5 <- AD[cid %in% SA[adopters >= 5, cid], .(cid, wid)]
setkey(A5, wid)
co <- A5[A5, on = "wid", allow.cartesian = TRUE][cid < i.cid, .N, by = .(c1 = cid, c2 = i.cid)]
co <- co[N >= 3]
co <- merge(co, SA[, .(c1 = cid, n1 = adopters)], by = "c1")
co <- merge(co, SA[, .(c2 = cid, n2 = adopters)], by = "c2")
co[, overlap := N / pmin(n1, n2)]
links <- co[overlap >= 0.5]
fam <- components_dt(links$c1, links$c2)
SA <- merge(SA, fam[, .(cid = node, family = l)], by = "cid", all.x = TRUE)
SA[is.na(family), family := cid]
FA <- SA[, .(scripts = .N, script_adoptions = sum(adopters), max_script = max(adopters), lead = text[which.max(adopters)]), by = family]
fam_adopters <- merge(AD[, .(cid, wid)], SA[, .(cid, family)], by = "cid")[, .(family_adopters = uniqueN(wid)), by = family]
FA <- merge(FA, fam_adopters, by = "family")[order(-family_adopters)]
rec("families_multi", FA[scripts >= 2, .N], "template families: >=2 scripts that co-occur (overlap coefficient >= 0.5, >=3 co-adopters)")
rec("families_multi_ge5", FA[scripts >= 5, .N], "template families with >=5 scripts")
rec("adoptions_in_multi_families", SA[family %in% FA[scripts >= 2, family], sum(adopters)] / SA[, sum(adopters)], "share of all script adoptions that belong to a multi-script family")
save_table(FA[1:40, .(family, scripts, family_adopters, max_script, lead = substr(lead, 1, 110))], "families_top40")
print(FA[1:20, .(scripts, family_adopters, max_script, lead = substr(lead, 1, 90))])

# ---- (c) timelines from never-edited profiles --------------------------------------------
D <- AD[text_dated == TRUE]
D[, month := as.Date(format(cr, "%Y-%m-01"))]
base <- Wf[text_dated == TRUE, .N, by = .(month = as.Date(format(cr, "%Y-%m-01")))]
setnames(base, "N", "profiles_created")
fam_d <- merge(D, SA[, .(cid, family)], by = "cid")
topfam <- FA[1:12, family]
TL <- unique(fam_d[family %in% topfam, .(family, wid, month)])[, .N, by = .(family, month)]
TL <- merge(TL, base, by = "month")[, share := N / profiles_created]
TL <- merge(TL, FA[, .(family, lead = substr(lead, 1, 60))], by = "family")
save_table(TL[order(family, month)], "families_timelines")
# burstiness: share of a family's dated adoptions that fall in its busiest 6-month window
burst <- fam_d[, .(dated = uniqueN(wid)), by = family][dated >= 30]
mi <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))   # integer month index
peak_share <- function(m, n, width = 6L) { best <- 0; for (s in unique(m)) best <- max(best, sum(n[m >= s & m < s + width])); best / sum(n) }
bw <- unique(fam_d[family %in% burst$family, .(family, wid, m = mi(month))])
bw <- bw[, .(n = .N), by = .(family, m)]
peak6 <- bw[, .(peak6 = peak_share(m, n), span_months = uniqueN(m)), by = family]
bn <- Wf[text_dated == TRUE, .N, by = .(m = mi(as.Date(format(cr, "%Y-%m-01"))))]
bn_peak <- peak_share(bn$m, bn$N)
rec("burst_peak6_median_family", med(peak6$peak6), "median share of a family's dated adoptions inside its busiest 6-month window (families with >=30 dated adopters)")
rec("burst_peak6_baseline", bn_peak, "same statistic for all dated profile creations (platform growth baseline)")
peak6 <- merge(peak6, FA[, .(family, family_adopters, lead = substr(lead, 1, 80))], by = "family")[order(-peak6)]
save_table(peak6, "families_burstiness"); print(head(peak6, 15))
# prevalence of carrying any shared sentence, by creation cohort (dated profiles only)
WR <- as.data.table(read_parquet(file.path(DERIVED, "worker_reuse.parquet")))[, .(wid, n1)]
coh <- merge(Wf[text_dated == TRUE, .(wid, month = as.Date(format(cr, "%Y-%m-01")))], WR, by = "wid")
coh <- coh[, .(profiles = .N, any_shared = mean(n1 > 0)), by = .(quarter = paste0(format(month, "%Y"), "Q", (as.integer(format(month, "%m")) - 1) %/% 3 + 1))][order(quarter)]
save_table(coh, "cohort_prevalence_dated"); print(coh)
write_parquet(SA, file.path(DERIVED, "script_attrs.parquet"))
save_numbers("05_scripts")
