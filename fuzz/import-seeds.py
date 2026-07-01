#!/usr/bin/env python3
"""Import table seeds for the louis-rs fuzz targets from a liblouis checkout.

Requires liblouis/ to be cloned next to the repo root:

    git clone --depth 1 https://github.com/liblouis/liblouis liblouis

Produces fuzz/seeds/table/upstream__* — one file per liblouis table, with
`include` directives stripped (from_table_source does not support them).
"""

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
LIBLOUIS_DIR = REPO_ROOT / "liblouis"

TABLE_EXTENSIONS = {".ctb", ".utb", ".cti", ".uti", ".tbl", ".dis"}
TABLE_SEED_DIR = REPO_ROOT / "fuzz" / "seeds" / "table"


def main() -> None:
    if not LIBLOUIS_DIR.is_dir():
        sys.exit(
            f"error: liblouis not found at {LIBLOUIS_DIR}\n"
            f"Clone it with: git clone --depth 1 https://github.com/liblouis/liblouis liblouis"
        )

    TABLE_SEED_DIR.mkdir(parents=True, exist_ok=True)

    for old in TABLE_SEED_DIR.glob("upstream__*"):
        old.unlink()

    written = 0
    for path in sorted((LIBLOUIS_DIR / "tables").iterdir()):
        if path.suffix not in TABLE_EXTENSIONS:
            continue
        lines = []
        for line in path.read_text(encoding="utf-8", errors="replace").splitlines(
            keepends=True
        ):
            if re.match(r"^\s*include(\s|$)", line):
                continue
            lines.append(line)
        out = TABLE_SEED_DIR / f"upstream__{path.name}"
        out.write_text("".join(lines), encoding="utf-8")
        written += 1

    print(f"Wrote {written} table seeds to {TABLE_SEED_DIR.relative_to(REPO_ROOT)}/")


if __name__ == "__main__":
    main()
