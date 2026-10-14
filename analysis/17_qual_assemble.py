#!/usr/bin/env python3
"""Validate and assemble qualitative codes from canonical stimulus tables.

Run from the project root: python3 analysis/17_qual_assemble.py [--pass1-only]
Raw decisions, amendments, QA and output CSVs live in qual/coding/julian/.
This is an assembler, not an automated qualitative classifier.
"""
import argparse
import csv
import hashlib
import json
import re
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SH = ROOT / "qual/coding/units"
JULIAN = ROOT / "qual/coding/julian"
MOVE = dict(ID="IDENTITY", CRED="CREDENTIAL", CAP="CAPABILITY", CHAR="CHARACTER", PROC="PROCESS", ENC="ENCOUNTER", AG="AGENCY", OTH="OTHER")
PORT = dict(P="PORTABLE", S="SEMI", N="NON-PORTABLE")
SEG = dict(PREM="PREMIUM", VOL="VOLUME", LOW="LOW-PRICE", MIX="MIXED", NONE="NONE")
REC = dict(PLAT="PLATFORM", OFF="OFF-PLATFORM", BOTH="BOTH", NONE="NONE")
SLOT = dict(OCC="OCCUPATION", TOOL="TOOL/SKILL", NUM="NUMBER", NAME="NAME/PLACE", VOICE="VOICE", TRUNC="TRUNCATION/EXTENSION", WORD="WORDING", FORMAT="FORMAT")
ASSEMBLY = dict(AUTH="AUTHORED", FULL="FULL-TEMPLATE", BLOCK="BLOCK-CLONE", FRAME="FRAME-AND-FILL", PATCH="PATCHWORK")
LOC = dict(OPEN="OPENING", BODY="BODY", CLOSE="CLOSING", THRU="THROUGHOUT", NONE="NONE")
COH = dict(COH="COHERENT", SEAMS="SEAMS", MISFIT="MISFIT")
CUES = ["cue_platform_vocab", "cue_marketing_copy", "cue_non_native", "cue_agency_voice", "cue_borrowed_headings", "cue_copy_artefacts"]
SPECS = {
    "1_sentences": (["profile_code", "pos"], {"move": MOVE, "portability": PORT}, "note"),
    "1_profiles_language": (["profile_code"], {**{c: {"0": "0", "1": "1"} for c in CUES}, "segment": SEG, "record_reference": REC}, "memo_language"),
    "2_slots": (["profile_code", "pos"], {"slot": SLOT}, "note"),
    "2_profiles_structure": (["profile_code"], {"assembly": ASSEMBLY, "authored_location": LOC, "coherence": COH}, "memo"),
    "3_pairs": (["pair_code"], {"edit_type": SLOT, "fit": {x: x for x in ("FIT", "NEUTRAL", "MISFIT")}}, "note"),
}


def read_csv(path):
    with path.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        return list(reader), reader.fieldnames


def write_csv(path, rows, fields):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def parse_raw(pass1_only):
    decisions = {name: {} for name in SPECS}
    files = sorted((JULIAN / "raw").glob("p1_*.txt"))
    if not pass1_only:
        files += [JULIAN / "raw/p2.txt", JULIAN / "raw/p3.txt"]
    for path in files:
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            if not line.strip() or line.startswith("#"):
                continue
            parts = line.split()
            try:
                if path.name.startswith("p1_"):
                    if parts[1] == "L":
                        name, key = "1_profiles_language", (parts[0],)
                        assert re.fullmatch(r"[01]{6}", parts[2])
                        values = dict(zip(CUES, parts[2]))
                        values.update(segment=SEG[parts[3]], record_reference=REC[parts[4]], memo_language=" ".join(parts[5:]))
                    else:
                        name, key = "1_sentences", (parts[0], str(int(parts[1])))
                        values = dict(move=MOVE[parts[2]], portability=PORT[parts[3]], note=" ".join(parts[4:]))
                elif path.name == "p2.txt":
                    if parts[1] == "S":
                        name, key = "2_profiles_structure", (parts[0],)
                        values = dict(assembly=ASSEMBLY[parts[2]], authored_location=LOC[parts[3]], coherence=COH[parts[4]], memo=" ".join(parts[5:]))
                    else:
                        name, key = "2_slots", (parts[0], str(int(parts[1])))
                        values = dict(slot=SLOT[parts[2]], note=" ".join(parts[3:]))
                else:
                    name, key = "3_pairs", (parts[0],)
                    values = dict(edit_type=SLOT[parts[1]], fit=parts[2], note=" ".join(parts[3:]))
                assert key not in decisions[name], f"duplicate {name} {key}"
                for field, levels in SPECS[name][1].items():
                    assert values[field] in levels.values(), f"invalid {field}: {values[field]}"
                decisions[name][key] = values
            except (IndexError, KeyError, AssertionError, ValueError) as exc:
                raise ValueError(f"{path.relative_to(ROOT)}:{lineno}: {line}: {exc}") from exc
    return decisions, files


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pass1-only", action="store_true")
    args = parser.parse_args()
    decisions, files = parse_raw(args.pass1_only)
    names = list(SPECS)[:2] if args.pass1_only else list(SPECS)
    outputs = {}
    amendments = JULIAN / "amendments.csv"
    amendment_rows = read_csv(amendments)[0] if amendments.exists() else []
    for name in names:
        keyfields, categories, _ = SPECS[name]
        rows, fields = read_csv(SH / f"{name}.csv")
        assert not (set(fields) & (set(categories) | {SPECS[name][2]})), f"stimulus table contains coding fields: {name}"
        fields += list(categories) + [SPECS[name][2]]
        keys = [tuple(row[k] for k in keyfields) for row in rows]
        assert len(keys) == len(set(keys)), f"duplicate sheet keys in {name}"
        assert set(keys) == set(decisions[name]), f"coverage {name}: missing={set(keys)-set(decisions[name])}, extra={set(decisions[name])-set(keys)}"
        for row, key in zip(rows, keys):
            row.update(decisions[name][key])
        lookup = {tuple(row[k] for k in keyfields): row for row in rows}
        for change in amendment_rows:
            if change["sheet"] != name:
                continue
            key = (change["profile_code"], change["pos"]) if "pos" in keyfields else (change.get("profile_code") or change["pair_code"],)
            row = lookup[key]
            field = change["field"]
            assert field in categories or field == SPECS[name][2], f"invalid amendment field {field}"
            assert row[field] == change["old"], f"stale amendment for {name} {key} {field}"
            if field in categories:
                assert change["new"] in categories[field].values(), f"invalid amendment value {change['new']}"
            assert change["reason"].strip(), "amendment must document reason"
            row[field] = change["new"]
        write_csv(JULIAN / f"{name}.csv", rows, fields)
        outputs[name] = rows

    # Repeated strings are a review aid, not a rule that context-dependent moves must match.
    groups = defaultdict(list)
    for row in outputs["1_sentences"]:
        norm = re.sub(r"\s+", " ", row["sentence"].lower()).strip()
        groups[norm].append(row)
    conflicts = []
    for rows in groups.values():
        if len({(r["move"], r["portability"]) for r in rows}) > 1:
            conflicts += [{k: r[k] for k in ("profile_code", "pos", "sentence", "move", "portability", "note")} for r in rows]
    write_csv(JULIAN / "qa/repeated_text_review.csv", conflicts, ["profile_code", "pos", "sentence", "move", "portability", "note"])
    summary = []
    for name, rows in outputs.items():
        for field in SPECS[name][1]:
            counts = Counter(row[field] for row in rows)
            summary += [dict(sheet=name, field=field, code=code, n=n, share=round(n/len(rows), 6)) for code, n in sorted(counts.items())]
    write_csv(JULIAN / "summary_counts.csv", summary, ["sheet", "field", "code", "n", "share"])
    if "2_profiles_structure" in outputs:
        flags, _ = read_csv(ROOT / "qual/profile_sample_sentences.csv")
        sharing = {(r["profile_code"], r["pos_c"]): float(r["maxJ"]) for r in flags}
        profile_sentences = defaultdict(list)
        for row in outputs["1_sentences"]:
            profile_sentences[row["profile_code"]].append(row)
        checks = []
        for row in outputs["2_profiles_structure"]:
            sentences = profile_sentences[row["profile_code"]]
            n = len(sentences)
            eligible = [r for r in sentences if sharing[(r["profile_code"], r["pos"])] < .5
                        and r["move"] in ("IDENTITY", "CREDENTIAL", "CAPABILITY")]
            # Equal-sized thirds, with surplus sentences in the earlier thirds.
            bins = {min(2, (int(r["pos"]) - 1) * 3 // n) for r in eligible}
            location = "NONE" if not bins else ("THROUGHOUT" if len(bins) > 1 else ["OPENING", "BODY", "CLOSING"][next(iter(bins))])
            fraction = sum(sharing[(r["profile_code"], r["pos"])] >= .5 for r in sentences) / n
            substantive_authored = any(sharing[(r["profile_code"], r["pos"])] < .5
                                       and r["move"] not in ("ENCOUNTER", "OTHER") for r in sentences)
            full_ok = fraction >= .9 and not substantive_authored
            assert row["authored_location"] == location, f"authored-location review required for {row['profile_code']}"
            assert row["assembly"] != "FULL-TEMPLATE" or full_ok, f"full-template rule review required for {row['profile_code']}"
            checks.append(dict(profile_code=row["profile_code"], sentences=n, reused_fraction=round(fraction, 6),
                               assembly=row["assembly"], eligible_authored_positions=";".join(r["pos"] for r in eligible),
                               authored_location=row["authored_location"], location_rule_passed=1, full_rule_eligible=int(full_ok)))
        write_csv(JULIAN / "qa/structure_rule_checks.csv", checks, list(checks[0]))

    # Enriched pass-2 view; keep judgments separate from canonical stimulus tables.
    sent = {(r["profile_code"], r["pos"]): r for r in outputs["1_sentences"]}
    out = []
    pc = None
    for line in (SH / "2_reader.md").read_text().splitlines():
        match = re.match(r"## (P\d+)$", line)
        if match:
            pc = match[1]
        match = re.match(r"- \*\*\[(\d+)\]\*\*", line)
        if match:
            r = sent[(pc, match[1])]
            line += f" **Code: {r['move']}/{r['portability']}**"
        out.append(line)
    (JULIAN / "views/2_reader_coded.md").write_text("\n".join(out) + "\n")
    pair_view = []
    for row in read_csv(SH / "3_pairs.csv")[0]:
        pair_view += [f"## {row['pair_code']} | {row['occupation_a']} <-> {row['occupation_b']}",
                      f"A: {row['sentence_a']}", f"B: {row['sentence_b']}", f"diff: {row['diff']}"]
    (JULIAN / "views/3_reader.md").write_text("\n".join(pair_view) + "\n")
    inputs = files + [SH / f"{n}.csv" for n in names] + [ROOT / "qual/CODEBOOK.md", ROOT / "qual/profile_sample_sentences.csv", Path(__file__)]
    if amendments.exists():
        inputs.append(amendments)
    manifest = {
        "method": "First-author close reading, not independently coded or verified provenance",
        "blinding": "Initial pass1 reader hid sharing flags and metadata; theory context was known. Audited amendments followed full-context and pass2 review. Final codes are not fully blinded.",
        "codebook": "v1", "complete_passes": [1] if args.pass1_only else [1, 2, 3],
        "rows": {n: len(rows) for n, rows in outputs.items()},
        "amendments": len(amendment_rows),
        "inputs_sha256": {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs},
        "outputs_sha256": {str((JULIAN / f"{n}.csv").relative_to(ROOT)): hashlib.sha256((JULIAN / f"{n}.csv").read_bytes()).hexdigest() for n in names},
    }
    (JULIAN / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print(json.dumps({"coverage": manifest["rows"], "repeated_text_review_rows": len(conflicts), "amendments": len(amendment_rows)}, indent=2))


if __name__ == "__main__":
    main()
