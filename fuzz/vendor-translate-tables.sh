#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

src_dir="liblouis/tables"
dst_dir="fuzz/data/translate_tables"

mkdir -p "$dst_dir"

python3 - "$src_dir" "$dst_dir" <<'PY'
from pathlib import Path
import shutil
import sys

src_dir = Path(sys.argv[1])
dst_dir = Path(sys.argv[2])

if not src_dir.is_dir():
    raise SystemExit(f"missing table source dir: {src_dir} (clone liblouis first)")

def includes(path):
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0] == "include":
            yield parts[1]

root_table = dst_dir / "fuzz_maximal.ctb"
if not root_table.is_file():
    raise SystemExit(f"missing root table: {root_table}")

seen = set()
stack = list(includes(root_table))
if not stack:
    raise SystemExit(f"no include directives in {root_table}")

while stack:
    name = stack.pop()
    if name in seen:
        continue
    seen.add(name)
    src = src_dir / name
    if not src.exists():
        raise SystemExit(f"missing source table: {src}")
    stack.extend(includes(src))

preserve = {"README.md", root_table.name}
for path in dst_dir.iterdir():
    if path.name in preserve:
        continue
    if path.is_dir():
        shutil.rmtree(path)
        continue
    if not path.is_file():
        continue
    if path.name not in seen:
        path.unlink()

for name in sorted(seen):
    shutil.copy2(src_dir / name, dst_dir / name)
PY
