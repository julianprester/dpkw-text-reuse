# Shared setup for the text-reuse pattern-surfacing pipeline.
# Sourced by every numbered script. Run all scripts from the project root.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringr)
})
setDTthreads(max(1L, parallel::detectCores() - 2L))
options(width = 160, datatable.print.nrows = 60, scipen = 6)

DERIVED <- "data/derived"
OUT     <- "analysis/output"
for (d in c(DERIVED, file.path(OUT, c("tables", "figures", "numbers")))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ---- number registry: every quantity cited in the paper/memo is written here
.N <- new.env()
.N$rows <- list()
rec <- function(key, value, note = "") {
  .N$rows[[length(.N$rows) + 1L]] <- data.table(name = key, value = if (is.numeric(value) && !is.na(value) && abs(value - round(value)) < 1e-9) format(round(value), scientific = FALSE, trim = TRUE) else as.character(signif(value, 6)), note = note)
  invisible(value)
}
save_numbers <- function(section) {
  x <- rbindlist(.N$rows)
  fwrite(x, file.path(OUT, "numbers", paste0(section, ".csv")))
  cat("\n[numbers] wrote", nrow(x), "entries to", file.path(OUT, "numbers", paste0(section, ".csv")), "\n")
}
save_table <- function(x, name) fwrite(x, file.path(OUT, "tables", paste0(name, ".csv")))

# ---- helpers
ts_parse <- function(x) as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
med <- function(x, ...) as.numeric(stats::median(x, na.rm = TRUE))
pct <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)

# Homophily of a categorical attribute on a set of pairs, relative to random mixing
# of the same pair endpoints (Newman 2003): obs = P(same), exp = sum_k a_k^2,
# lift = obs/exp, r = (obs - exp)/(1 - exp).
homophily <- function(g1, g2) {
  ok <- !is.na(g1) & !is.na(g2)
  g1 <- g1[ok]; g2 <- g2[ok]
  obs <- mean(g1 == g2)
  a <- prop.table(table(c(g1, g2)))
  expd <- sum(a^2)
  list(n = length(g1), obs = obs, exp = expd, lift = obs / expd, r = (obs - expd) / (1 - expd))
}

# Connected components by min-label propagation (avoids an igraph/glpk dependency).
components_dt <- function(a, b) {
  lab <- data.table(node = unique(c(a, b)))
  lab[, l := node]
  setkey(lab, node)
  e <- data.table(a = a, b = b)
  repeat {
    e[, la := lab[J(e$a), l]][, lb := lab[J(e$b), l]]
    m <- rbind(e[, .(node = a, l = pmin(la, lb))], e[, .(node = b, l = pmin(la, lb))])[, .(l = min(l)), by = node]
    new <- merge(lab, m, by = "node", suffixes = c("", ".m"))
    new[, l2 := pmin(l, l.m)]
    changed <- sum(new$l2 != new$l)
    lab <- new[, .(node, l = l2)]
    setkey(lab, node)
    if (changed == 0) break
  }
  lab
}

# Figure theme
if (requireNamespace("ggplot2", quietly = TRUE)) {
  suppressPackageStartupMessages(library(ggplot2))
  theme_set(theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot"))
  save_fig <- function(p, name, w = 7, h = 4.5) ggsave(file.path(OUT, "figures", paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
}
