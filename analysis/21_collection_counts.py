"""Collection accounting for the Appendix A / Methods flags.

Reads the separate data repository (ids.csv and the 2023 profile/ responses) and the analysis input data/profiles.csv.
Writes analysis/output/numbers/21_collection_counts.csv. Only filenames and the first bytes of each saved response
are inspected, and only the small error stubs are parsed. Nothing in the data repository is executed.
Usage (from the project root): python3 -I analysis/21_collection_counts.py PATH/TO/platform-careers-data
"""
import collections, csv, json, os, sys

repo = os.path.join(sys.argv[1], "data")
rows = list(csv.reader(open(os.path.join(repo, "ids.csv"), encoding="utf-8")))[1:]
ciphers = [r[0] for r in rows if r]
ids = set(ciphers)

valid, errors, present = set(), collections.Counter(), set()
pdir = os.path.join(repo, "profile")
for f in os.listdir(pdir):
    if not f.endswith(".json"):
        continue
    c = f[:-5]
    present.add(c)
    with open(os.path.join(pdir, f), "rb") as fh:
        head = fh.read(60)
    if head.startswith(b'{"profile"'):
        valid.add(c)
    elif head.startswith(b'{"error"'):
        with open(os.path.join(pdir, f), encoding="utf-8") as fh:
            errors[json.load(fh).get("error")] += 1
    else:
        raise SystemExit("unexpected response format: " + f)

with open("data/profiles.csv", encoding="utf-8", newline="") as fh:
    rd = csv.reader(fh)
    h = next(rd)
    mine = {r[h.index("worker_id")] for r in rd if r}

assert present == ids, "every requested identifier should have exactly one saved response"
assert mine == valid, "analysis input should equal the valid saved profiles"

out = [
    ("ids_rows", len(ciphers), "rows in ids.csv (excluding header)"),
    ("ids_unique", len(ids), "unique worker identifiers requested"),
    ("ids_duplicate_rows", len(ciphers) - len(ids), "duplicate identifier rows removed"),
    ("profiles_usable", len(valid), "responses containing a profile; equals the analysis input rows"),
    ("profiles_inaccessible", sum(errors.values()), "responses containing an error instead of a profile"),
    ("profiles_inaccessible_share", sum(errors.values()) / len(ids), "inaccessible responses / unique identifiers requested"),
    ("profiles_error_removed", errors["This profile is no longer available."], "error: profile no longer available"),
    ("profiles_error_notfound", errors["Freelancer profile not found"], "error: freelancer profile not found"),
]
assert sum(errors.values()) == len(ids) - len(valid)
with open("analysis/output/numbers/21_collection_counts.csv", "w", encoding="utf-8", newline="") as fh:
    w = csv.writer(fh)
    w.writerow(["name", "value", "note"])
    for n, v, note in out:
        w.writerow([n, round(v, 6) if isinstance(v, float) else v, note])
for n, v, _ in out:
    print(n, v)
