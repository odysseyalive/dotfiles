#!/bin/bash
# Awareness Ledger: trigger-pattern capture reminder
# PreToolUse hook on Task and Bash
# Exits 0 always (awareness, not blocking)
# Pattern-matches tool input against capture trigger patterns
# Only outputs when a trigger pattern matches — eliminates reminder fatigue

TOOL_NAME="$1"

# Only trigger on Task and Bash (debugging often reveals capturable knowledge)
if [[ "$TOOL_NAME" != "Task" && "$TOOL_NAME" != "Bash" ]]; then
    exit 0
fi

# Check if ledger exists
LEDGER_DIR=".claude/skills/awareness-ledger/ledger"
if [[ ! -d "$LEDGER_DIR" ]]; then
    exit 0
fi

INPUT=$(cat)

# Incident triggers
if echo "$INPUT" | grep -qiE '(roll\s*back|revert|undo|broke|regression|root\s*cause|what\s*went\s*wrong|caused\s*by|the\s*fix\s*was|lesson\s*learned)'; then
    echo "--- AWARENESS LEDGER ---" >&2
    echo "Capture trigger detected: incident-related language" >&2
    echo "Suggested record type: INC (Incident)" >&2
    echo "After resolving this issue, consider recording with /awareness-ledger record" >&2
    echo "--- END LEDGER ---" >&2
    exit 0
fi

# Decision triggers
if echo "$INPUT" | grep -qiE '(chose.*because|should\s*use.*instead|trade-?off|downside\s*of\s*this|going\s*forward|from\s*now\s*on|the\s*pattern\s*should)'; then
    echo "--- AWARENESS LEDGER ---" >&2
    echo "Capture trigger detected: decision/trade-off language" >&2
    echo "Suggested record type: DEC (Decision)" >&2
    echo "After completing this task, consider recording with /awareness-ledger record" >&2
    echo "--- END LEDGER ---" >&2
    exit 0
fi

# Pattern triggers
if echo "$INPUT" | grep -qiE '(keeps\s*happening|every\s*time\s*we|i.ve\s*noticed|turns\s*out|the\s*trick\s*is|what\s*works\s*is|except\s*when|doesn.t\s*apply)'; then
    echo "--- AWARENESS LEDGER ---" >&2
    echo "Capture trigger detected: recurring pattern language" >&2
    echo "Suggested record type: PAT (Pattern)" >&2
    echo "After completing this task, consider recording with /awareness-ledger record" >&2
    echo "--- END LEDGER ---" >&2
    exit 0
fi

# Flow triggers
if echo "$INPUT" | grep -qiE '(first\s*it\s*does.*then|when\s*the\s*user\s*does|the\s*flow\s*is|only\s*happens\s*when|requires.*to\s*be\s*running)'; then
    echo "--- AWARENESS LEDGER ---" >&2
    echo "Capture trigger detected: flow/process description" >&2
    echo "Suggested record type: FLW (Flow)" >&2
    echo "After completing this task, consider recording with /awareness-ledger record" >&2
    echo "--- END LEDGER ---" >&2
    exit 0
fi

# No trigger matched — stay silent
exit 0
