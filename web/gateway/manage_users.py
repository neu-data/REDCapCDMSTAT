"""Create, list or remove gateway users.

  python manage_users.py add <name> <dm|stats|monitor|pi|viewer>   # prints the API key once
  python manage_users.py list
  python manage_users.py remove <name>

Only a SHA-256 hash of each key is stored.
"""

import json
import os
import secrets
import sys
from pathlib import Path

from main_hash import hash_key

USERS_FILE = Path(os.environ.get("CDMSTAT_USERS_FILE", Path(__file__).resolve().parent / "users.json"))
ROLES = {"dm", "stats", "monitor", "pi", "viewer"}


def load() -> dict:
    return json.loads(USERS_FILE.read_text()) if USERS_FILE.exists() else {}


def save(users: dict) -> None:
    USERS_FILE.write_text(json.dumps(users, indent=2))


def main(argv: list[str]) -> None:
    if len(argv) >= 3 and argv[0] == "add":
        name, role = argv[1], argv[2]
        if role not in ROLES:
            raise SystemExit(f"role must be one of {sorted(ROLES)}")
        users = {h: u for h, u in load().items() if u["name"] != name}
        key = "nx_" + secrets.token_urlsafe(24)
        users[hash_key(key)] = {"name": name, "role": role}
        save(users)
        print(key)
    elif argv[:1] == ["list"]:
        for u in load().values():
            print(f"{u['name']}\t{u['role']}")
    elif len(argv) >= 2 and argv[0] == "remove":
        save({h: u for h, u in load().items() if u["name"] != argv[1]})
    else:
        print(__doc__)


if __name__ == "__main__":
    main(sys.argv[1:])
