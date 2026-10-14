# Analysis pipeline

The model-led manuscript is `PAPER.qmd`, with supporting detail in `APPENDIX.qmd`. Its principal
computational evidence is produced by scripts 01–07 and 12–14 below; `PATTERNS.md` and scripts 08–10 retain
earlier exploration. Publication assets are assembled by script 19 from persisted estimates, not refitted
inside Quarto. Each script writes its quantities to `analysis/output/numbers/<script>.csv`
(name, value, note), its tables to `analysis/output/tables/`, and figures to
`analysis/output/figures/`. Intermediate data go to `data/derived/` (≈ 1 GB, regenerable).

## Environment

```bash
docker build -t text-reuse-r analysis/          # rocker/tidyverse:4.6 + tidytext, textreuse, tokenizers, stopwords, fixest, lme4
analysis/run.sh analysis/01_corpus.R            # runs a script in the container, from the project root
```

Run in order (≈ 1.5 h on 16 cores; the multilevel fits in 13 and 14 dominate):

```bash
analysis/run.sh analysis/{01_corpus,02_pairs,03_content,04_anatomy,05_scripts,06_edits,07_circulation,08_signaling,09_extensions,10_mechanisms,12_model1_authorship,13_model2_valuation,14_robustness,11_figures}.R
```

## Scripts

| Script | What it does | Key outputs |
|---|---|---|
| `text-reuse.R` | Original detection step: sentences (8–50 words), word 5-gram shingles, 240-permutation MinHash, LSH with 60 bands, exact Jaccard on candidates. Produces `data/similar_pairs.csv`. Not re-run (slow); `01`/`02` rebuild and validate its sentence ids. | `data/similar_pairs.csv` |
| `00_setup.R` | Paths, number registry (`rec()`), homophily helper, connected components, figure theme. | — |
| `01_corpus.R` | Rebuilds the sentence corpus exactly as `text-reuse.R` (same filter, order, ids) and adds sentence position/offset, profile, title and metadata covariates (tenure, edit interval, never-edited flag, city, agency, …). | `workers.parquet`, `sentences.parquet` |
| `02_pairs.R` | Validates the id mapping (3,000 random pairs re-scored: 100% agreement), links pairs to workers, drops same-profile pairs, builds exact-match clusters ("scripts") and worker-level reuse measures; LSH detection probabilities. | `xpairs.parquet`, `scripts.parquet`, `worker_reuse.parquet` |
| `03_content.R` | Occupational-specificity lexicon (KL divergence of a word's occupational distribution from the population, estimated on sentences with undetected reuse at J ≥ .5) and a rhetorical-move dictionary; exports a stratified measurement-audit sample (uncoded). | `sent_content.parquet`, `tables/validation_sample.csv` |
| `04_anatomy.R` | Within-profile anatomy: how much is shared, where shared sentences sit (slot, offset vs relative position), blocks vs scatter (runs test), cloned vs assembled (best-partner coverage), long-sentence robustness. | `tables/anatomy_*` |
| `05_scripts.R` | Script ecology: occupational/geographic scope vs population baselines, template families (co-adoption components), adoption waves from never-edited profiles. | `script_attrs.parquet`, `tables/scripts_*`, `tables/families_*` |
| `06_edits.R` | Template slots: word-level alignment of 400k near-copy pairs; which token classes are edited. | `tables/edits_*` |
| `07_circulation.R` | Homophily by similarity band (occupation, country, city, agency) vs random mixing; cohort homophily; status homophily vs matched nulls; country-pair lifts; precedence from non-overlapping edit intervals. | `tables/circ_*` |
| `08_signaling.R` | Worker-level models (country-clustered SEs): who shares, dose-response, substitution by verified record/rating, composition (shared/authored × generic/specific), script popularity, ratings, first-job access, informativeness of scripts (ICC), cloning vs assembling. | `tables/sig_*` |
| `09_extensions.R` | Robustness/extension probes: ICC with disjoint groups and net of city, standardised gaps, shared moves, copied openings, agency exclusion. | `tables/ext_*` |
| `10_mechanisms.R` | Discriminating probes: gap by tenure, volume at a given price, gap by occupation. | `tables/mech_*` |
| `12_model1_authorship.R` | **Model 1 (sender side).** Sentence-level logit with worker fixed effects: which claims are shared vs authored (specificity, quantification, function, position). LPM check. | `tables/model1_*` |
| `13_model2_valuation.R` | **Model 2 (market side).** Worker-level model of the log asking rate on profile composition (authored/shared × generic/specific) and its interaction with the verified record; fixed-effects form with country-clustered SEs and multilevel form with city and template-family random intercepts (provenance share, LR test). | `tables/model2_*` |
| `14_robustness.R` | One robustness table for both models (11 checks; provenance re-estimated for 3); cached per check in `tables/robustness_parts/`. | `tables/robustness_*` |
| `15_qual_samples.R` | Theoretically sampled materials for the qualitative strand: 150 profiles in 10 cells (full text + sentence markup), 100 near-copy pairs with word diffs, provenance worksheet for 64 template families. Contains full public profile texts — internal use only. | `qual/*.csv` |
| `16_qual_coding_sheets.R` | Three-pass readers and canonical stimulus tables, with no blank coder fields or comparison subset. Does not classify the texts. | `qual/coding/units/` |
| `17_qual_assemble.py` | Validates and assembles persisted first-author judgments and audited amendments; coverage, hashes and rule checks. Python standard library only. Does not generate qualitative labels. | `qual/coding/julian/` |
| `18_qual_review.R` | Internal coding summaries and dictionary/specificity diagnostics. No coder comparison or reliability calculations. | `qual/coding/julian/review/` |
| `tests/qual_review_tests.R` | Software QA: coverage, counts, diagnostic arithmetic, invalid labels/identifiers and altered-stimulus rejection. Fixtures use R's temporary directory only. | Console/test log |
| `20_occupation_sensitivity.R` | Sensitivity of occupational homophily to using only the first-listed occupation: scores worker pairs by overlap of full category sets (any overlap, Jaccard, number shared) against random mixing. Descriptive; no manuscript model is refitted. | `tables/occsens_*` |
| `21_collection_counts.py` | Collection accounting for the Methods and Appendix A: requested, duplicate, usable and inaccessible profiles. Reads the separate data repository (`python3 -I analysis/21_collection_counts.py PATH/TO/platform-careers-data`), inspects only filenames, first bytes and the small error stubs, and checks that the analysis input equals the valid saved responses. Not part of `run.sh`; requires that repository. | `output/numbers/21_collection_counts.csv` |
| `19_manuscript_assets.R` | Publication tables/figures, numerical variable/source audit, input hashes, and short-quotation checks. Uses saved estimates; does not fit models or classify texts. | `output/paper/`, `../_variables.yml` |
| `tests/manuscript_tests.R` | Production QA: numerical shortcodes, citation/caption keys, image dependencies, input hashes, coefficient strings, retained control specification, and checked excerpts. Not interpretive validation. | `output/paper/manuscript_checks.txt` |
| `11_figures.R` | Figures (run last). | `figures/*.png` |

## Qualitative coding

See `qual/coding/README.md` and `qual/coding/julian/MEMO.md` for the completed first-author close reading.
Independent coding and reliability testing have been canceled; provenance/community tracing remains pending.
The initial reader hid computational labels/metadata; audited corrections followed full-context and pass-2 review.
Final judgments are not fully blinded. Dictionary concordance is a diagnostic against provisional close-reading codes, not
independent validation or population classifier accuracy; the original 300-sentence validation sample remains uncoded.

```bash
python3 analysis/17_qual_assemble.py
analysis/run.sh analysis/18_qual_review.R
analysis/run.sh analysis/tests/qual_review_tests.R
```

Rerunning `16` regenerates stimulus tables/readers; `17` assembles the same persisted judgments without
classifying text. Review options are only `--codes=<directory>` and `--out=<directory>`; pass options directly
to Rscript in Docker because `run.sh` accepts script paths, not script arguments. The old independent-coding
folders, blank coder fields, subset flags, comparison commands and reliability calculations have been removed.
The separate 300-sentence audit sample is retained as uncoded stimuli, not an active double-coding worksheet.

## Manuscript production

```bash
analysis/run.sh analysis/19_manuscript_assets.R
analysis/run.sh analysis/tests/manuscript_tests.R
analysis/render.sh             # HTML and DOCX; optionally pass html or docx
```

For live preview (opens `PAPER.html` directly):

```bash
GIT_DIR=/dev/null quarto preview PAPER.qmd
```

`PAPER.qmd` is the only project input. Bare `quarto preview` is not supported: a default-type project
has no project-wide preview without an `index` page, and that page was an extra redirect step. The
command-local `GIT_DIR` setting only prevents Quarto's automatic `.gitignore` creation and does not alter
repository configuration.

Quarto reads local `references.bib` and `apa.csl`, includes the rebuilt `APPENDIX.qmd`, and consumes
precomputed tables and figures. No host R is needed for rendering. Outputs go to `output/manuscript/`.
The wrapper prevents Quarto's automatic `.gitignore` insertion and removes its transient project cache;
it does not change Git configuration or ignore tracked research materials.
The publication file quotes passages without case labels; `output/paper/quotation_audit.csv` (internal case codes
and source units for every excerpt), full profile texts, and coding materials remain internal, not publication supplements.

The original age spline and estimates are retained. An attempted removal was interrupted at the author's
request; original model scripts, coefficient tables, robustness outputs, and numerical registries were
restored byte-for-byte. The aborted partial results are labeled in `output/archive/no_description_age_aborted/`
and are not used by the manuscript. The pre-interruption checkpoint is `output/archive/description_age_spec/`.

## Conventions

* **Shared (exact)** = the sentence has a cross-worker partner with Jaccard = 1 on word 5-gram sets; operationally verbatim apart from case/punctuation, but this is shingle-set equality, not character equality.
* **Scripts** are connected components of exact-match pairs; **adopters** are distinct workers holding a script.
* **Reuse frame** = 353,099 workers with at least one codable sentence (8–50 words).
* Author-confirmed timestamps concern description creation/editing. The legacy `tenure_yrs` field is
  description/profile age measured to the latest observed description edit (2023-09-08), not account tenure.
  The original Model 2 age spline is retained separately from the completed-job and work-hour experience controls.
  The `text_dated` flag permits an edit gap up to one day; it does not establish that text preceded all work.
* All regressions are descriptive associations on a cross-section; Model 1 clusters by worker and fixed-effects rate models by country. The multilevel rate model uses city/family effects.
