#!/usr/bin/env python3
"""Fail if llms.txt misses a public name or an example program.

Every identifier in a PUBLIC statement of src/rationals.f90, every procedure
bound to the type, and every example/*.f90 (verbatim) must appear in llms.txt.
"""
import pathlib
import re
import sys

root = pathlib.Path(__file__).resolve().parent.parent
llms = (root / "llms.txt").read_text()
source = (root / "src" / "rationals.f90").read_text()

names = []
for line in source.splitlines():
    match = re.match(r"\s*public\s*::\s*(.*)", line, re.IGNORECASE)
    if match:
        names += re.split(r"\s*,\s*", match.group(1).strip())
# Type-bound procedures are public without a PUBLIC statement.
for body in re.findall(r"^\s*type\s*::.*?^\s*end\s+type", source, re.M | re.S | re.I):
    for match in re.finditer(r"^\s*procedure\s*::\s*(.*)", body, re.M | re.I):
        names += re.split(r"\s*,\s*", match.group(1).strip())

errors = []
for name in names:
    if re.fullmatch(r"\w+", name) and not re.search(rf"\b{name}\b", llms):
        errors.append(f"public name not in llms.txt: {name}")

for example in sorted((root / "example").glob("*.f90")):
    if example.read_text().strip() not in llms:
        errors.append(f"example not verbatim in llms.txt: {example.name}")

print("\n".join(errors) or "llms.txt covers all public names and examples")
sys.exit(1 if errors else 0)
