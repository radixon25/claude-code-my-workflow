#!/usr/bin/env python
"""
Block accidental edits to protected files.

Cross-platform replacement for protect-files.sh.
Customize PROTECTED_FILES below for your project.

Hook Event: PreToolUse (matcher: Edit|Write)
Returns: Exit code 2 to block, 0 to allow.
"""

import json
import os
import sys


# ============================================================
# CUSTOMIZE: Add filenames you want to protect from edits.
# Uses basename matching — add full paths for more precision.
# ============================================================
PROTECTED_FILES = [
    "Bibliography_base.bib",
    "settings.json",
]


def main():
    try:
        hook_input = json.load(sys.stdin)
    except (json.JSONDecodeError, IOError):
        sys.exit(0)

    tool_name = hook_input.get("tool_name", "")
    file_path = hook_input.get("tool_input", {}).get("file_path", "")

    # No file path = not a file operation, allow
    if not file_path:
        sys.exit(0)

    basename = os.path.basename(file_path)

    if basename in PROTECTED_FILES:
        print(
            f"Protected file: {basename}. "
            f"Edit manually or remove protection in .claude/hooks/protect-files.py",
            file=sys.stderr
        )
        sys.exit(2)

    sys.exit(0)


if __name__ == "__main__":
    main()
