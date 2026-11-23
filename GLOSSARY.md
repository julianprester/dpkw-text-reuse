# Glossary and terminology decisions

Working document for making `PAPER.qmd` and `APPENDIX.qmd` consistent. It is not part of the manuscript.

How to read it:

- **Use** is the term we propose to keep. Anything marked **DECIDE** is an open choice.
- **Also in the text** lists the variants found, with counts in `PAPER.qmd` (P) and `APPENDIX.qmd` (A).
  Counts are case-insensitive substring matches outside citations, so they are approximate.
- **Action** says what to do with each variant. "Retire" means replace it. "Restrict" means keep it only in the stated place.
- Section 14 lists words that carry several meanings in the manuscript. Those are the biggest readability risk.
- Section 15 lists the decisions we still owe. The manuscript text has not been edited to follow it yet.

Several decisions depend on parked model items: the conditions box, the P1 relation and the circulation-families box.
Those entries say so.

---

## 1. Setting

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The market type | **online labor market** | The setting in general: digitally mediated markets that match workers and clients | digital labor platform (P1), labor platform (P2), online service market (P1), gig work (P1) | Retire the variants in our own prose. Keep them only when paraphrasing a cited study that uses them |
| The operator and its features | **platform** | Upwork, or the platform operator, and its fields and policies (profile fields, records, ratings) | platform (P39, A7) | Restrict to the operator and its features. Do not use it for the market |
| The data source | **Upwork** | The platform we study | Upwork (P6, A2) | Keep |
| Our data | **input file**, then **reuse frame** | See section 12 | see section 12 | See section 12 |

## 2. People

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The profile owner | **worker** | A person with a public profile | worker (P145, A39); freelancer (P8); provider (P6); service provider (P1); contractor (P1); applicant (P1); person (P13); individual (P18) | Retire freelancer, provider, contractor and applicant in our own prose. Restrict "individual" to "individual worker" and contrasts such as "individual vs relational". Keep "freelancer" inside quotations |
| The hiring side | **client** | A prospective buyer reading a profile | client (P49, A4); employer (P3); audience (P8) | Restrict "employer" to paraphrases of hiring studies. Restrict "audience" to self-presentation theory (Goffman) |
| The signaling-theory role | **sender** | Worker, when we speak in signaling theory's terms | sender (P11) | Restrict to theory sentences. Every use should be replaceable by "worker" |
| A worker with at least one exact-shared sentence | **sharer** | Operational: at least one exact-shared qualifying sentence (the Q indicator in Model 2) | sharer (P8, A4); copier (code only) | Keep "sharer". Never "copier" in the paper: we cannot identify direction |
| A worker holding a particular script | **adopter** | One of the distinct workers whose profile contains the script (counts per script) | adopter (P5, A3); holder (P1) | Keep "adopter". Retire "holder" |

## 3. The text and its parts

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The public page | **profile** | Description plus platform-recorded fields | profile (P150, A33) | Keep |
| The editable text field | **description** | The worker's free-text field | description (P59, A27); profile description (P2, A1); profile text (P12, A1) | Use "description" for the field and when measuring (description age). Use **profile text** for the text as a generic or theoretical object. Retire "profile description" after its definition |
| The activity | **self-presentation** | The worker's presentation of themselves through profile content | self-presentation (P16) | Keep. It is the activity, not the object |
| The analysis unit | **qualifying sentence**, then **sentence** | A sentence of 8–50 words in the reuse frame | qualifying sentence (P13, A7); sentence (P96, A50); unit (P9, A11); eligible unit (P3, A4); codable (README) | Define "qualifying sentence" once, then use "sentence". Retire "unit" and "eligible unit" for sentences. Keep "unit" only in "unit of analysis" and "full-unit contrast" |
| One occurrence of an exact-shared sentence | **sentence instance** | A script's appearance in one worker's description | sentence instance (P1, A3) | Keep; define it |
| Recurring text of any length | **passage** | Text that recurs across workers, sentence or longer | passage (P14, A6) | Keep for recurrence. Use "sentence" for the analysis unit |
| The words used | **wording** | The lexical form of text, regardless of unit | wording (P53); language (P8, A4); prose (P3); phrase (P1) | Keep "wording". Retire language, prose and phrase as synonyms |
| What a sentence asserts | **claim** | A statement about the worker or the service | claim (P43, A16); statement (P1, A2); formula (P9) | Keep "claim". Restrict "formula" to "encounter formula". Retire "statement" |
| Consecutive shared sentences | **block** | A run of consecutive exact-shared sentences | block (P3, A1); frame (P14, A6) | Keep "block" if defined. Retire "frame" as a noun for text |
| Tokenizer output | **token** | A tokenizer unit in the content measures | token (P18, A24); word (running prose) | Keep "token" for measurement. Use "word" in prose and for word counts |
| Distinct word forms | **word type**, **informative type** | A distinct occupation-informative word form | type (P12, A3); informative type (P4, A1) | Keep both; define "informative type" in the specificity measure |
| Comparison unit for detection | **word five-gram**, **shingle set** | A sentence's set of word five-grams | five-gram (P1, A1); shingle (P2, A4) | Use "word five-gram" in the paper. Use "shingle set" only in Appendix B |

## 4. Reuse: the measure, the practice and the pattern

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The method | **text-reuse detection** | The sequence-preserving method that finds shared wording | text-reuse (P5, A3); reuse (P23, A5) | Keep. Use "reuse" as the general phenomenon |
| The measured category | **exact-shared** | A qualifying sentence with a cross-worker match at J = 1 | shared (P106, A15); exact-shared (P9, A4); copied (P19, A1); non-exact-shared (P4, A1); unshared (P4) | Use "exact-shared" for the measured label and "exact sharing" for the measure. "Shared" is acceptable shorthand inside compound labels (shared-generic) |
| The complementary category | **authored** | No detected exact cross-worker match. It does not establish independent composition | authored (P35, A6); non-exact-shared (P4, A1); unshared (P4); original (P14, A10) | Use "authored" with its caveat defined once. Retire "non-exact-shared" and "unshared". Restrict "original" to "original source", "original author" and "original detection pipeline" |
| The practice of taking others' wording | **copying** | A worker uses wording that already circulates. Direction is not observed | copying (P29, A3); copied (P19, A1); duplication (none) | Use "copying" for the practice in theory text. Never as a measured label. Retire "duplicate" |
| The practice of writing wording | **authoring** | A worker produces own wording | authoring (P10); authorship (P2, A3); writing (P9, A1); composing (P3) | Keep "authoring". Retire "authorship" except in "original authorship" |
| The property of being easy to reproduce | **reproduction**, **reproducible** | Used as "textual reproduction" (defined construct) and "easily reproducible" | reproduction (P4, A2) | Restrict to the property or construct. Do not use it for the act |
| The spread of shared wording | **circulation** | How exact-shared wording is distributed across profiles, places and occupations | circulation (P64, A7); adoption (P3) | Keep "circulation" for the structure |
| Reuse that is not exact | **near-copy** | A sentence pair with .5 ≤ J < 1 | near-copy (P8, A5); variant (P7, A7); near-exact (P1); adapted template (P4) | Use "near-copy" for the pair or relation and "variant" for each side's wording. Retire "near-exact" |
| The profile-level measure | **profile composition shares** | The four shares of a worker's qualifying sentences | composition shares, composition (P110) | See section 5 |

## 5. Profile composition (the central construct)

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The construct | **profile composition** | The combination of authoring and copying in one description | profile composition (P23); composition (P110, A14); compose (P14) | Define "profile composition" once. Use "composition" alone only when the meaning is clear |
| Selective use of the two practices | **selective composition** | Workers combine authored and exact-shared wording by sentence content (P1) | selective composition (P6); selective profile composition (heading) | Keep. **DECIDE (parked):** whether it becomes the relation between authoring and copying in the figure |
| The measured categories | **authored-generic, authored-specific, shared-generic, shared-specific** | Authored or exact-shared × generic or specific. Authored-generic is the omitted reference | composition shares | Keep exactly these four |
| The estimates for the categories | **association**, **contrast** | A conditional association relative to authored-generic | premium, discount (P4/P11); full-unit contrast (P5) | "Premium" and "discount" are shorthand for the association sign. Always define them as relative to authored-generic |

## 6. Specificity and rhetorical function

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The construct | **occupational specificity** | How concentrated a sentence's wording is on particular occupations | occupation-specific (P2, A1); occupationally specific (P2); occupation-informative (P4, A1); occupational detail (P2, A1); occupational content (P1) | Use "occupational specificity" for the construct. Use "occupation-specific" for the adjective. Use "occupation-informative" only for tokens. Retire "occupationally specific", "occupational detail" and "occupational content" |
| The two sentence classes | **specific** / **generic** | Specific: at least one occupation-informative type. Generic: none | generic (P8, A1); specific (P37, A14) | Define at first use, with the threshold |
| The reference rhetorical group | **claim-bearing** | Identity, credential and capability moves | claim-bearing (P5, A2); individuating claim (P1, A1, estimation label); capability-bearing (code) | **DECIDE:** one label for both text and tables. Proposal: "claim-bearing" everywhere in the paper and tables; "individuating claim" only in code |
| The other rhetorical groups | **assurance**, **encounter formula**, **other** | Assurance: character and process moves. Encounter formula: contact invitations and courtesies. Other: agency and unmatched | assurance (P18, A5); encounter formula (P8, A1) | Keep. Use only these labels in Findings |
| Qualitative labels | evidentiary account, testimonial lead-in, risk-reversal offer, virtue block | Descriptive labels from the sampled profiles | each used once or twice | Do not present them as new categories. Gerit's concern: map each to the four function groups or mark it as a purely descriptive label. **DECIDE** the mapping |
| Applicability of a claim | **claim portability** | The range of workers for whom a claim stays applicable | portability (P4, A2) | Keep; it is defined in Constructs and Measurements |
| Adapting a sentence | **personalization** | A near-copy with a worker-specific element inserted | personalization (P2) | Keep; use only for near-copy pairs |

## 7. Templates and circulation structure

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| A recurring exact-shared sentence form | **script** | A connected component of exact-sharing sentences held by at least two workers | script (P84, A62); template (P9, A2); form (P41, A12); frame (P14, A6) | Use "script" for the measured object. Restrict "template" to vernacular or interpretation and to quotations. Retire "form" and "frame" as noun synonyms |
| The collective stock | **repertoire** | The set of scripts workers can draw on | repertoire (P7) | Keep, defined as the set of scripts |
| Groups of scripts | **circulation family** | A co-adoption component of scripts | family (P44, A22); circulation family (P2); template family (A1, code); family-linked (P2c label) | Say "circulation family" at first use, then "family". Retire "template family". **DECIDE (parked):** P2c wording and the family box (see section 15) |
| A worker's family | **dominant family** | The family covering most of a worker's shared sentence instances | dominant family (P2, A1) | Keep |
| Circulation relationships of shared text | **provenance** | Circulation relationships that a worker's shared wording indexes | provenance (P11, A1); origin (P21, A11); source (P17, A12) | Restrict "provenance" to the construct. Restrict "origin" and "source" to the unobserved external source |
| What we cannot observe | **copying direction**, **external source** | Which profile copied which, and where wording first came from | direction (P4, A3); transmission (P1, A2) | Use only in limitation-style sentences |

## 8. Outcomes and records

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The outcome construct | **market position** | The standing a profile occupies in the market | market position (P23) | Keep for the construct |
| Its measure | **posted asking rate** | Log posted hourly rate, USD 3–500 | asking rate (P23, A1); posted asking rate (P7); posted rate (P2); hourly rate (P2, A2); price position (P3); price (P13); rate (P40, A14) | Say "posted asking rate" at the definition and "asking rate" after. Retire "posted rate", "price position" and "price", except in "price-peer" and in paraphrases |
| Others' outcomes | hiring, earnings, wage | Outcomes in other studies | earnings (P10); wage (P6) | Restrict to paraphrases of other work. Our paper does not measure them |
| The record construct | **verified record** | Platform-recorded work, which cannot be copied | verified record (P16); record (P95, A20); reputation (P11); platform-recorded (P3) | Use "verified record" for the construct. "Reputation" only for literature and for "reputation building" in the figure |
| The record measure | **completed-job group** | Completed jobs in groups 0, 1–9, 10–49, 50+ | completed-job group (P7, A1); record group (P6, A2); completed jobs (P8, A2) | Use "completed-job group" for the measure. Retire "record group" |
| Experience | **completed jobs** | Count of jobs completed on the platform | experience (P19, A7) | Do not use "experience" for completed jobs. Reserve it for claims in the text and for years |
| Non-displacement | **non-substitution**, **substitution** | Whether records displace text | non-substitution (P6, A1); substitution (P1); displace (P3) | Use "substitute" and "non-substitution". Retire "displace" |

## 9. Conditions

All entries here depend on the pending decision about the conditions box (see section 15).

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| Copyability | **copyability** | Public visibility combined with inexpensive textual reproduction. A property of the platform's information environment | copyability (P24) | Keep |
| The pressure to stand out | **DECIDE** | The external need to stand out to a hurried audience | differentiation pressure (P8); pressure to differentiate (P3); need to differentiate (P1, in the RQ); differentiation (P12) | One term across the paper, once the conditions box is settled |
| Status of the conditions | **conditions of the setting** | Drawn from the literature, not measured | condition (P17, A3); scope condition (P1); boundary condition (none) | Use "conditions of the setting"; retire "scope condition" |

## 10. Signaling vocabulary

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| Signaling-theory object | **signal** | An observable feature that conveys information about hidden attributes | signal (P34); cue (P8, A2); credential (P5, A5); cheap talk (P1) | Use "signal" for signaling-theory objects. Restrict "cue" to cited work. Restrict "credential" to certifications |
| Signal kinds | **assessment signal**, **conventional signal** | Cited distinction | each (P1–2) | Keep; always cited |
| Combination of signals | **signal portfolio**, **signal portfolio building** | The set of signals a client reads together (Steigenberger & Wilhelm 2018). Signal portfolio building is the joint practice of profile composition and reputation building (theory figure) | new | Use "signal portfolio" for the combination and "signal portfolio building" for the joint practice. Keep "profile composition" for the textual practice |
| "Informative" | **informative** | In the signaling sense, a signal carries information about the sender | informative (P15, A4); informational (P9); uninformative (P4) | Gerit asked us to scale back the "adds information" framing. Avoid it where a neutral word works: use "associated with", "differs by". Do not use "informative" for token types except in "occupation-informative" |

## 11. Geography

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The construct | **geographic proximity** | Whether two workers share a city or a country. In Model 2, controlled through country fixed effects (all forms) and a city random intercept nested in country (2c only), so that circulation families are estimated net of it | city (P18, A4); country (P19, A13); geographic (P8, A1); place (P9); local (P6); proximity (P2, A1); region (A3) | Use "geographic proximity" for the construct. Use "city" and "country" for the variables. Restrict "place" to literature framing. Retire "locality", "local" and "region" as synonyms. The proximity-rank analysis (UN sub-regions, `analysis/22_geographic_proximity.R`) was dropped |
| Agency | **agency affiliation** | Whether the worker is part of an agency | agency (P8, A8); affiliation (P8, A1); organizational (P6, A2) | Say "agency affiliation". Do not mix with "organizational" |
| Description age | **description age** | Age of the description at the latest observed update | tenure (P1, A3, code legacy); description age (P3, A1) | Use "description age". Keep "tenure" only inside code names |

## 12. Data and samples

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The raw file | **input file** | Worker-profile records retrieved on 21 Sep 2023 | input file (P2); retrieved-profile cross-section (P2, A1); profile records | Keep "input file" and use "retrieved-profile cross-section" only for the sampling caveat |
| The sentence corpus | **sentence corpus** | All qualifying sentences | corpus (P11, A5) | Keep |
| The workers in it | **reuse frame** | Workers with at least one qualifying sentence | reuse frame (P2, A2); qualifying frame; measured profiles | Use "reuse frame". Retire "qualifying frame" and "measured profiles" |
| Model 1 sample | **identifying sample** | Sentences from mixed profiles that identify the fixed-effects slopes | identifying sample (P1, A2); mixed profiles (P3, A1); eligible units | Use "identifying sample" and say "mixed profile" once as a definition |
| Model 2 sample | **rate sample** | Workers with eligible asking rate and observed description age | rate sample (A1); Model 2 workers | Use "rate sample" |

## 13. Models and theory

| Concept | Use | Definition | Also in the text | Action |
|:---|:---|:---|:---|:---|
| The two models | **Model 1**, **Model 2** (forms 2a, 2b, 2c) | See Research Method | Model 1 (P13, A16); Model 2 (P19, A13); sentence-level / worker-level model; sender side / market side (code) | Keep. Headings should be parallel across Method and Findings (see section 15). Retire "sender side" and "market side" outside code |
| What the estimates show | **association** | Conditional, not causal | association (P43); effect (P28, A15); relationship (P17); contrast (P20, A7) | Use "association" for estimates. Keep "effect" in "fixed effect" and "random effect" only |
| Theory output | **proposition** | P1–P3 | proposition (P16); mechanism (P6, A1) | Keep |
| The figure | **process model** | Conditions, practices, outcome, in a limited sense | process model (P6); account (P28, A3) | Keep "process model". Restrict "account" to "evidentiary account" or the theoretical argument. Do not use it for both |
| Parts of the argument | **component** | Content, profile composition, provenance | components (P4); dimensions (P6) | Say "components" for the three-way distinction and "dimensions" only in headings |
| Second practice | **reputation building** | Observed only as completed jobs | new in the figure | Keep as figure label; always qualify as "observed as verified records" |
| Qualitative strand | **supplementary qualitative analysis**, **sampled profiles**, **excerpts** | Close readings of theoretically sampled profiles | qualitative analysis (P7, A2); sentence codes (P2, A3); close reading (README); contextual material (P1); cases | Use the formal name for the analysis, "sampled profiles" for the profiles, "excerpts" for quotes. Retire "cases" and "contextual material" |

## 14. Words with more than one meaning

| Word | Meanings found | Proposal |
|:---|:---|:---|
| **record** | (a) verified record (platform-recorded work), (b) "public profile records" (input data rows), (c) "record group" | (a) "verified record"; (b) say "profile record" and use it only in the data section; (c) "completed-job group" |
| **shared** | (a) exact-shared (measured), (b) "shared by many" in prose, (c) shared-generic (category) | Use "exact-shared" for the measure and "shared" only inside compound labels |
| **content** | (a) the Content component, (b) "content tokens/words" in the tokenizer, (c) "other content words" in the alignment classes | (a) "content" with capital C at definition; (b)/(c) say "content word" |
| **form** | (a) model forms 2a/2b/2c, (b) "recurring form" of text, (c) "form" of a signal | (a) "model form"; (b) "script"; (c) avoid |
| **family** | (a) circulation family, (b) dominant family, (c) "family-linked" | Say "circulation family" at first use |
| **frame** | (a) reuse frame (sample), (b) text frame, (c) "frame" as verb | (a) keep; (b) retire; (c) fine |
| **unit** | (a) qualifying sentence, (b) "full-unit contrast", (c) "unit of analysis" | (a) retire; (b)/(c) keep |
| **specific** | (a) occupation-specific, (b) "specific claim" (quantified), (c) service-specific | Define once: specific = at least one occupation-informative type |
| **account** | (a) our argument, (b) the account in a profile ("evidentiary account"), (c) "account of services" | Restrict to (b) and (c) |
| **original** | (a) original author, (b) original detection pipeline, (c) original vs copied profile | Keep (a) and (b) only as stated; (c) is the dichotomy we reject |
| **informative** | (a) occupation-informative (tokens), (b) signaling sense, (c) "informative" in ordinary prose | (a) keep; (b)/(c) see section 10 |
| **practice** | (a) authoring and copying, (b) reputation building (figure) | Define both as practices in the theory section |
| **provenance** | (a) circulation relationships (construct), (b) source/origin | Restrict to (a) |
| **template** | (a) vernacular for reusable text, (b) "template family" (code), (c) quotations | Restrict to (a) and (c) |

## 15. Open decisions

1. **Conditions box** (pending, Gerit's alternatives): which conditions appear, if any, and how they are named. It decides section 9.
2. **P1 and the authoring↔copying relation** (parked).
3. **Circulation-families box and the P2c label** (parked). Gerit suggests "differences between circulation families".
4. **Claim-bearing vs individuating claim:** one label in the paper and tables.
5. **Qualitative labels** (evidentiary account, lead-in, and so on): map to the four function groups or mark as descriptive.
6. **Heading parity.** Method says "Model 1: Within-Profile Composition" and Findings says "Model 1: Selective Profile Composition". Model 2 headings are also different in the two places. Pick one title per model and use it in both.
7. **Free-standing "profile description" and "profile text":** whether to keep both or only "description" plus "profile text".
8. **Gerit's text sections** (abstract, introduction, background) still use the variants listed here, especially "digital labor platform", "freelancer", "sender" and "differentiation". Apply this glossary to those once they are reworked.

## 16. Code names vs paper terms

| Code or output name | Paper term |
|:---|:---|
| `wid`, `worker` | worker |
| `shared_gen`, `shared_spec`, `authored_spec` | shared-generic, shared-specific, authored-specific share |
| `shared_any`, `sharer` | share of exact-shared sentences, sharer |
| `family`, `template family` | circulation family |
| `cid` | script identifier |
| `spec_n`, `specific` | number of distinct informative types, specific sentence |
| `macro` | rhetorical function |
| `city_key`, `country_f`, `city_f` | city, country, city random intercept |
| `tenure_yrs` | description age |
| `maxJ` | maximum sentence similarity (Jaccard) |
| `totalJobsWorked`, `record` | completed jobs, completed-job group |
| `lrate`, `hourlyRate` | log posted asking rate, posted hourly rate |
