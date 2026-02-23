#!/usr/bin/env python
"""
Post-merge hook: Reflect on learnings from this session.

Cross-platform replacement for post-merge.sh.
Prompts user to add [LEARN] entries to appropriate memory files.
Does NOT auto-append (user maintains control).

Hook Event: Post-merge (git)
"""

import sys


def main():
    print("=== SESSION MERGED TO MAIN ===")
    print()
    print("Take a moment to reflect on learnings from this session.")
    print()
    print("Where should learnings go?")
    print()
    print("  MEMORY.md (committed, synced across machines)")
    print("     -> Generic patterns applicable to ALL academic workflows")
    print("     -> Examples: workflow improvements, design principles, documentation patterns")
    print("     -> Format: [LEARN:category] pattern -> benefit")
    print()
    print("  .claude/state/personal-memory.md (gitignored, local only)")
    print("     -> Machine-specific learnings (file paths, tool versions, edge cases)")
    print("     -> Examples: 'XeLaTeX on macOS requires TEXINPUTS=../Preambles'")
    print("     -> Stays on this machine, doesn't clutter template for other users")
    print()
    print("Consider adding [LEARN] entries if:")
    print("  [ ] You corrected a mistake that might recur")
    print("  [ ] You discovered a pattern applicable to similar projects")
    print("  [ ] You solved a problem through trial and error")
    print("  [ ] You received user feedback on approach or quality")
    print()
    print("Not every session needs entries -- only capture reusable insights.")
    print()

    sys.exit(0)


if __name__ == "__main__":
    main()
