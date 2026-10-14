# 03_content.R -- Code every codable sentence on two dimensions:
#  (1) occupational specificity: how many occupation-informative words it contains.
#      The lexicon is estimated on ORIGINAL sentences only (maxJ < 0.5), so templates
#      cannot manufacture their own specificity. A word is occupation-informative if the
#      first-listed occupations of the workers who use it diverge from the population mix
#      (Kullback-Leibler divergence, bits).
#  (2) rhetorical move: a transparent dictionary with explicit precedence.
#
# Output: data/derived/sent_content.parquet, analysis/output/tables/lexicon_*.csv,
#         analysis/output/tables/validation_sample.csv (uncoded measurement-audit stimuli)

source("analysis/00_setup.R")
suppressPackageStartupMessages(library(tokenizers))
set.seed(31)
S  <- as.data.table(read_parquet(file.path(DERIVED, "sentences.parquet")))
SR <- as.data.table(read_parquet(file.path(DERIVED, "sent_reuse.parquet")))[, .(sid, maxJ, script_adopters)]
W  <- as.data.table(read_parquet(file.path(DERIVED, "workers.parquet")))[, .(wid, occ1)]
S  <- merge(S, SR, by = "sid")

# ---- 1. occupational specificity lexicon ----------------------------------------------
tk <- tokenize_words(S$text, strip_numeric = FALSE)
TK <- data.table(sid = rep(S$sid, lengths(tk)), token = unlist(tk, use.names = FALSE))
TK[, wid := S$wid[match(sid, S$sid)]]
orig_sids <- S[maxJ < 0.5, sid]
inc <- unique(TK[sid %in% orig_sids, .(wid, token)])
inc <- merge(inc, W[!is.na(occ1)], by = "wid")
pi_k <- inc[, .(wid, occ1)] |> unique()
pi_k <- pi_k[, .N, by = occ1][, pi := N / sum(N)]
cnt <- inc[, .N, by = .(token, occ1)]
cnt[, n_t := sum(N), by = token]
cnt <- cnt[n_t >= 100]
cnt <- merge(cnt, pi_k[, .(occ1, pi)], by = "occ1")
cnt[, p := N / n_t]
lex <- cnt[, .(n_workers = n_t[1], kl = sum(p * log2(p / pi)), modal_occ = occ1[which.max(p / pi)], modal_lift = max(p / pi), modal_share = p[which.max(p / pi)]), by = token]
lex[, numeric := str_detect(token, "^[0-9]+$")]
setorder(lex, -kl)
KL_CUT <- 0.5
lex[, informative := kl >= KL_CUT & !numeric]
save_table(lex, "lexicon_occupational_specificity")
rec("lexicon_tokens", nrow(lex), "word types used by >=100 workers in original sentences")
rec("lexicon_informative", lex[informative == TRUE, .N], sprintf("occupation-informative word types (KL >= %.1f bits)", KL_CUT))
cat("\nKL quantiles:\n"); print(round(quantile(lex$kl, c(.5, .75, .9, .95, .99)), 3))
cat("\nExamples around the cut-off:\n"); print(lex[between(kl, KL_CUT - 0.05, KL_CUT + 0.05)][sample(.N, min(.N, 25)), .(token, n_workers, kl = round(kl, 2), modal_occ)])
cat("\nTop informative tokens by occupation:\n")
print(lex[informative == TRUE][order(-n_workers), head(.SD, 6), by = modal_occ][, .(modal_occ, token, n_workers, kl = round(kl, 2))])
cat("\nKL of some generic words:\n"); print(lex[token %in% c("quality", "communication", "clients", "honest", "deadline", "work", "experience", "skills", "wordpress", "logo", "translation", "excel", "seo", "autocad", "quickbooks", "brief", "freelancer")][, .(token, n_workers, kl = round(kl, 3), modal_occ)])

TK <- merge(TK, lex[, .(token, kl, informative)], by = "token", all.x = TRUE)
spec <- TK[, .(spec_n = uniqueN(token[informative %in% TRUE]), spec_max = max(c(0, kl), na.rm = TRUE),
               has_number = any(str_detect(token, "^[0-9]+$"))), by = sid]
rm(TK, inc, tk); gc()

# ---- 2. rhetorical moves (dictionary, precedence top to bottom) ------------------------
occ_nouns <- "(developer|designer|writer|copywriter|editor|translator|transcriber|assistant|va|marketer|specialist|expert|engineer|consultant|accountant|bookkeeper|analyst|scientist|manager|artist|animator|illustrator|photographer|videographer|programmer|coder|architect|lawyer|attorney|paralegal|teacher|tutor|researcher|strategist|freelancer|student|graduate|entrepreneur|representative|agent|administrator|technician|drafter|modeler|modeller|voice|seo|virtual|web|graphic|content|data|full[- ]stack|front[- ]end|back[- ]end|mobile|software|social|digital|video|customer)"
rx <- list(
  agency     = "\\bwe (are|have|provide|offer|always|specialize|specialise|deliver|help|work with|also|love|believe|pride|ensure|build|create|develop|design|value|guarantee|can provide)\\b|\\bour (team|company|agency|firm|clients|services|goal|mission|experts|developers|designers|work|expertise|professionals|staff)\\b",
  identity   = paste0("^(hi|hello|hey|greetings|dear|welcome|good day)\\b|\\bmy name is\\b|^(i am|i'm|i’m|am) (a|an) ([a-z-]+ ){0,4}", occ_nouns, "\\b"),
  character  = "\\b(honest\\w*|hard[- ]?work(ing|er)?|dedicat\\w*|reliab\\w*|trustworth\\w*|trust|passion\\w*|kind(ness)?|respect(ful)?|integrity|loyal\\w*|punctual\\w*|sincer\\w*|motivat\\w*|committed|commitment|over-?deliver\\w*|giving more|resilien\\w*|pride|proud|positive|attitude|(fast|quick) learner|self-?starter|team player|detail-?oriented|patien\\w*|responsib\\w*|work ethic|friendly|humble|perfectionist|enthusias\\w*|curious|ambitious|diligent|disciplin\\w*|energetic|caring|care about)\\b",
  encounter  = "look(ing)? forward|\\bthank(s| you)\\b|contact me|feel free|hire me|\\blet'?s (discuss|talk|work|connect|chat|get|build|create|bring|take|make|start|schedule|do|keep)\\b|reach out|hear(ing)? from you|message me|send me|drop me|invite me|don'?t hesitate|get in touch|keep in touch|invitation|best regards|right place|look no further|are you looking for|do you need|come to the right|ready to (take|get|start|work)|visiting my profile|my profile",
  process    = "\\b(communicat\\w*|deadlines?|on[- ]time|timely|quality|satisf\\w*|revisions?|unlimited|deliver\\w*|responsive\\w*|availab\\w*|working hours|long[- ]term|relationships?|budget|affordable|refund|guarantee\\w*|24/7|turnaround|transparen\\w*|updates?|confidential\\w*|accura\\w*|efficien\\w*|solutions?|flexible|start to finish|client'?s? needs?)\\b",
  credential = "\\b\\d{1,2}\\s*\\+?\\s*(years?|yrs)\\b|\\b(experience[ds]?|expertise|certified|certificat\\w*|degree|bachelor\\w*|master'?s|graduat\\w*|diploma|worked (with|for|at|on|in)|background in|portfolio)\\b",
  capability = "\\bi (can|will|could)( also)? (help (you )?(with|to|in)|do|create|design|develop|build|write|provide|offer|handle|manage|assist|make|edit|translate|convert|set up|setup|maintain|update|fix|optimi[sz]e|transcribe|research)\\b|\\b(services?|skills?|speciali[sz]\\w*|proficien\\w*|skilled|knowledge|familiar with|tools?|software)\\b"
)
S[, move := "other"]
for (m in rev(names(rx))) S[str_detect(text, rx[[m]]), move := m]   # reverse order => earlier entries take precedence
S[, macro := fcase(move %in% c("identity", "credential", "capability"), "capability-bearing",
                   move %in% c("character", "process"), "assurance",
                   move == "encounter", "encounter formula",
                   move == "agency", "agency voice", default = "other")]
S <- merge(S, spec, by = "sid", all.x = TRUE)
S[is.na(spec_n), `:=`(spec_n = 0L, spec_max = 0, has_number = FALSE)]
S[, specific := spec_n > 0]
write_parquet(S[, .(sid, wid, move, macro, spec_n, spec_max, specific, has_number)], file.path(DERIVED, "sent_content.parquet"))

tab <- S[, .(sentences = .N, share_specific = mean(specific), exact_copied = mean(maxJ >= 1)), by = .(macro, move)][order(-sentences)]
tab[, share := sentences / sum(sentences)]
print(tab); save_table(tab, "moves_overview")
for (i in seq_len(nrow(tab))) rec(paste0("move_share_", tab$move[i]), tab$share[i], "share of codable sentences coded to this move")
rec("specific_share_all", S[, mean(specific)], "share of sentences with >=1 occupation-informative word")
rec("specific_share_original", S[maxJ < 1, mean(specific)], "... among sentences without an exact cross-worker match")
rec("specific_share_copied", S[maxJ >= 1, mean(specific)], "... among exact-copied sentences")

# ---- 3. measurement-audit sample (stratified: copied / original; uncoded) -------------
val <- rbind(S[maxJ >= 1][sample(.N, 150)], S[maxJ < 0.3][sample(.N, 150)])[sample(.N)]
val <- val[, .(sid, text, dict_move = move, dict_specific = specific, copied = maxJ >= 1)]
save_table(val, "validation_sample")
save_numbers("03_content")
