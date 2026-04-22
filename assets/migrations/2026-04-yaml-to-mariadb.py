#!/usr/bin/env python3
"""Migrate the YAML file-based database into MariaDB."""

import os
import sys

TABLES = ["book", "dance", "person", "set", "source", "tune", "user", "version"]


def esc(s):
    return s.replace("\\", "\\\\").replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <database-directory>", file=sys.stderr)
        sys.exit(1)

    db_dir = sys.argv[1]

    for table in TABLES:
        table_dir = os.path.join(db_dir, table)
        if not os.path.isdir(table_dir):
            continue

        for entry_id in sorted(os.listdir(table_dir)):
            entry_dir = os.path.join(table_dir, entry_id)
            meta_path = os.path.join(entry_dir, "meta.yaml")
            if not os.path.isfile(meta_path):
                continue

            with open(meta_path) as f:
                yaml_content = f.read()

            cover_path = os.path.join(entry_dir, "cover.webp")
            if table == "source" and os.path.isfile(cover_path):
                with open(cover_path, "rb") as f:
                    cover_hex = f.read().hex()
                print(
                    f"INSERT INTO `{table}` (`id`, `yaml`, `cover`) "
                    f"VALUES ('{esc(entry_id)}', '{esc(yaml_content)}', X'{cover_hex}');"
                )
            else:
                print(
                    f"INSERT INTO `{table}` (`id`, `yaml`) "
                    f"VALUES ('{esc(entry_id)}', '{esc(yaml_content)}');"
                )


if __name__ == "__main__":
    main()
