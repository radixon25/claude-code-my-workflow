#!/usr/bin/env python
"""
Cross-platform hook launcher for Claude Code.

Resolves CLAUDE_PROJECT_DIR from the environment (or falls back to cwd)
and runs the named hook script with the correct Python interpreter.
Passes stdin through so hooks receive their JSON input.

Usage (in settings.json):
    "command": "python .claude/hooks/run_hook.py <hook_name>.py"
"""

import os
import sys
import subprocess


def main():
    if len(sys.argv) < 2:
        sys.exit(0)

    hook_name = sys.argv[1]

    # Resolve project directory
    project_dir = os.environ.get("CLAUDE_PROJECT_DIR", os.getcwd())

    # Build path to the hook script
    hook_path = os.path.join(project_dir, ".claude", "hooks", hook_name)

    if not os.path.exists(hook_path):
        # Hook not found — fail silently (don't block Claude)
        sys.exit(0)

    # Run the hook with the same Python interpreter, passing stdin through
    result = subprocess.run(
        [sys.executable, hook_path],
        stdin=sys.stdin
    )
    sys.exit(result.returncode)


if __name__ == "__main__":
    main()
