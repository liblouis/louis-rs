import os
from pathlib import Path

TABLES = Path(__file__).parent / "tables"

# louis resolves tables via the search_path crate, which reads LOUIS_TABLE_PATH.
os.environ["LOUIS_TABLE_PATH"] = str(TABLES)
