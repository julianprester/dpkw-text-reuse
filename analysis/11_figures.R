# 11_figures.R -- Figures for the analytic memo / paper, built from analysis/output/tables.
source("analysis/00_setup.R")
suppressPackageStartupMessages(library(scales))
tb <- function(x) fread(file.path(OUT, "tables", paste0(x, ".csv")))
pal <- c("#1b4f72", "#c0392b", "#7f8c8d", "#d68910", "#117a65")

# Fig 1 -- anatomy: where shared text sits, and what it says
pos <- tb("anatomy_position_slots")[, slot := factor(slot, levels = c("first", "second", "middle", "second-to-last", "last"))]
pl <- melt(pos[, .(slot, `generic sentences` = exact_generic, `occupation-specific sentences` = exact_specific)], id.vars = "slot")
p1a <- ggplot(pl, aes(slot, value, group = variable, colour = variable)) + geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(1), limits = c(0, NA)) + scale_colour_manual(values = pal[1:2], name = NULL) +
  labs(x = "Position of the sentence in the profile", y = "Share shared verbatim", title = "A. Authored openings, templated closings") +
  theme(legend.position = "bottom")
sc <- tb("lexicon_occupational_specificity")  # not plotted; kept for reference
mv <- tb("moves_overview")[order(exact_copied)][, move := factor(move, levels = move)]
p1b <- ggplot(mv, aes(exact_copied, move)) + geom_col(fill = pal[1]) + scale_x_continuous(labels = percent_format(1)) +
  labs(x = "Share shared verbatim", y = NULL, title = "B. Copying by rhetorical move")
g1 <- gridExtra_available <- requireNamespace("patchwork", quietly = TRUE)
save_fig(p1a, "fig_anatomy_position", 6.5, 4); save_fig(p1b, "fig_anatomy_moves", 5.5, 3.5)

# Fig 2 -- fidelity gradient of homophily
H <- tb("circ_homophily_by_band")[attribute %in% c("city_key", "country", "occ1")]
H[, attribute := factor(attribute, levels = c("city_key", "country", "occ1"), labels = c("same city", "same country", "same occupation"))]
H[, band := factor(band, levels = c("[0.3,0.5)", "[0.5,0.7)", "[0.7,0.9)", "[0.9,1)", "exact"))]
p2 <- ggplot(H, aes(band, lift, colour = attribute, group = attribute)) + geom_hline(yintercept = 1, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 2) + scale_colour_manual(values = pal[c(2, 1, 4)], name = NULL) +
  labs(x = "Textual similarity of the shared sentence (Jaccard band)", y = "Lift over random mixing", title = "The more verbatim the reuse, the more local and occupational") +
  theme(legend.position = "bottom")
save_fig(p2, "fig_fidelity_gradient", 6.5, 4.2)

# Fig 3 -- template slots
er <- tb("edits_rate_by_token_class")[order(edit_rate)][, cls := factor(cls, levels = cls)]
p3 <- ggplot(er, aes(edit_rate, cls)) + geom_col(fill = pal[1]) + scale_x_continuous(labels = percent_format(1)) +
  labs(x = "Share of tokens edited in near-copies (0.5 <= J < 1)", y = NULL, title = "What copiers change: numbers and occupation words")
save_fig(p3, "fig_template_slots", 6, 3.2)

# Fig 4 -- script scope
SA <- as.data.table(read_parquet(file.path(DERIVED, "script_attrs.parquet")))[adopters >= 20]
SA[, content := fifelse(specific, "contains occupation-specific words", "generic")]
p4 <- ggplot(SA, aes(occ_conc, cty_conc, size = adopters, colour = content)) + geom_point(alpha = .55) +
  geom_vline(xintercept = 2, linetype = 2, colour = "grey50") + geom_hline(yintercept = 2, linetype = 2, colour = "grey50") +
  scale_x_log10() + scale_y_log10() + scale_size_area(max_size = 8) + scale_colour_manual(values = pal[c(2, 1)], name = NULL) +
  labs(x = "Occupational concentration of adopters (x random)", y = "Country concentration of adopters (x random)", title = "Scope of 691 scripts with >= 20 adopters") +
  theme(legend.position = "bottom") + guides(size = "none")
save_fig(p4, "fig_script_scope", 6.5, 5)

# Fig 5 -- dose-response and persistence
dr <- tb("sig_M2_dose_response_bins")
dr <- rbind(data.table(bin = "0", est = 0, se = 0), dr[, .(bin, est, se)])
dr[, bin := factor(bin, levels = bin)]
p5a <- ggplot(dr, aes(bin, 100 * (exp(est) - 1))) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_pointrange(aes(ymin = 100 * (exp(est - 1.96 * se) - 1), ymax = 100 * (exp(est + 1.96 * se) - 1)), colour = pal[1]) +
  labs(x = "Share of the profile's sentences shared verbatim", y = "% difference in asking rate", title = "A. Dose-response (controls, country-clustered 95% CI)")
sb <- tb("sig_M3_substitution_by_record")[, record := factor(record, levels = record)]
p5b <- ggplot(sb, aes(record, pct_full_vs_authored)) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_pointrange(aes(ymin = 100 * (exp(share1_effect - 1.96 * se) - 1), ymax = 100 * (exp(share1_effect + 1.96 * se) - 1)), colour = pal[2]) +
  labs(x = "Verified record", y = "% gap, fully shared vs authored", title = "B. No substitution by verified reputation")
save_fig(p5a, "fig_dose_response", 7, 4); save_fig(p5b, "fig_no_substitution", 5, 3.8)

# Fig 6 -- template families over time (never-edited profiles), per 1,000 datable profiles created that month
B  <- tb("families_burstiness")
SA6 <- as.data.table(read_parquet(file.path(DERIVED, "script_attrs.parquet")))[, .(cid, family)]
SR6 <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[!is.na(cid), .(cid, wid)]
W6  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[n_c > 0 & text_dated == TRUE, .(wid, month = as.Date(format(cr, "%Y-%m-01")))]
keep <- B[family_adopters >= 90][order(-peak6)][1:6, family]
x <- unique(merge(merge(SR6, SA6, by = "cid")[family %in% keep], W6, by = "wid")[, .(family, wid, month)])
base6 <- W6[, .(created = .N), by = month]
tl <- merge(x[, .N, by = .(family, month)], base6, by = "month")[, per1000 := 1000 * N / created]
tl <- merge(tl, B[, .(family, lead = paste0(substr(lead, 1, 48), "..."))], by = "family")
grid <- CJ(family = keep, month = base6$month)
tl <- merge(grid, tl, by = c("family", "month"), all.x = TRUE)[is.na(per1000), per1000 := 0]
tl[, lead := B$lead[match(family, B$family)]][, lead := paste0(substr(lead, 1, 48), "...")]
p6 <- ggplot(tl[month >= as.Date("2020-06-01")], aes(month, per1000)) + geom_col(fill = pal[1]) + facet_wrap(~lead, ncol = 2, scales = "free_y") +
  labs(x = "Creation month (profiles never edited after creation)", y = "Adopters per 1,000 profiles created", title = "Template families arrive in waves") +
  theme(strip.text = element_text(size = 8))
save_fig(p6, "fig_family_waves", 8, 6.5)


# Fig M1 -- Model 1 (authorship allocation): odds ratios, within-profile FE logit
m1 <- tb("model1_coefficients")[model == "M1 main (FE logit)" & term != "lwords"]
lab <- c(lspec = "Occupation-specific words (log count)", number = "Quantified claim (number)", funcassurance = "Assurance (vs individuating claim)",
         `funcencounter formula` = "Encounter formula (vs individuating claim)", funcother = "Other function (vs individuating claim)",
         rel = "Position: last vs first sentence of the profile")
m1[, label := factor(lab[term], levels = rev(lab))]
pm1 <- ggplot(m1, aes(odds_ratio, label)) + geom_vline(xintercept = 1, colour = "grey50") +
  geom_pointrange(aes(xmin = exp(est - 1.96 * se), xmax = exp(est + 1.96 * se)), colour = pal[1]) + scale_x_log10() +
  labs(x = "Odds ratio of being shared verbatim (log scale; worker fixed effects)", y = NULL, title = "Model 1: which claims workers take from the commons")
save_fig(pm1, "fig_model1_authorship", 7, 3.8)

# Fig M2 -- Model 2 (market side): composition effects by verified record
sl <- tb("model2_slopes_by_record")
ml <- rbind(sl[, .(record, what = "shared generic", est = shared_gen_est, se = shared_gen_se)],
            sl[, .(record, what = "shared specific", est = shared_spec_est, se = shared_spec_se)],
            sl[, .(record, what = "authored specific", est = authored_spec_est, se = authored_spec_se)])
ml[, `:=`(record = factor(record, levels = sl$record), what = factor(what, levels = c("authored specific", "shared specific", "shared generic")))]
pm2 <- ggplot(ml, aes(record, 100 * (exp(est) - 1), colour = what, group = what)) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin = 100 * (exp(est - 1.96 * se) - 1), ymax = 100 * (exp(est + 1.96 * se) - 1)), position = position_dodge(.3)) +
  scale_colour_manual(values = pal[c(5, 4, 2)], name = "Profile made entirely of") +
  labs(x = "Verified record", y = "% difference in asking rate vs authored generic", title = "Model 2: the market reads authorship, not only reputation") +
  theme(legend.position = "bottom")
save_fig(pm2, "fig_model2_composition_by_record", 7, 4.5)

cat("figures written to", file.path(OUT, "figures"), "\n")
