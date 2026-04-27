#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3Packages.psycopg2 python3Packages.pyyaml
"""
Convert all `yaml` columns from YAML to JSON in the Dancelor PostgreSQL database.

Usage:
    ./2026-04-yaml-to-json.py
    sudo -u dancelor sh -c 'HOME=$(mktemp -d) PGDATABASE=dancelor PGUSER=dancelor /yaml-to-json.py'

Connects using the standard PG* environment variables (PGHOST, PGDATABASE, PGUSER, etc.).
"""

import datetime
import json
import psycopg2
import yaml

TABLES = ["book", "dance", "person", "set", "source", "tune", "user", "version"]

class DateTimeEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, (datetime.datetime, datetime.date)):
            return obj.isoformat()
        return super().default(obj)

def yaml_to_json(s):
    return json.dumps(yaml.safe_load(s), cls=DateTimeEncoder, ensure_ascii=False, separators=(",", ":"))

def main():
    conn = psycopg2.connect()
    cur = conn.cursor()

    for table in TABLES:
        cur.execute(f'SELECT "id", "yaml" FROM "{table}"')
        rows = cur.fetchall()
        updated = 0
        for (id, content) in rows:
            try:
                converted = yaml_to_json(content)
            except Exception as e:
                print(f"  WARNING: {table}/{id}: {e}")
                continue
            cur.execute(
                f'UPDATE "{table}" SET "yaml" = %s WHERE "id" = %s',
                (converted, id),
            )
            updated += 1
        print(f"{table}: {updated}/{len(rows)} rows converted")

    conn.commit()

    print("Converting column types and renaming...")
    for table in TABLES:
        cur.execute(f'ALTER TABLE "{table}" ALTER COLUMN "yaml" TYPE JSON USING "yaml"::json')
        cur.execute(f'ALTER TABLE "{table}" RENAME COLUMN "yaml" TO "json"')
    conn.commit()

    cur.close()
    conn.close()
    print("Done.")

if __name__ == "__main__":
    main()
