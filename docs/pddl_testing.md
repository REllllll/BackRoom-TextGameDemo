# PDDL Environment Testing Guide

This document explains how to validate that the PDDL environment is working and integrated with the Prolog game.

## Quick test (recommended)

Run the shell script:

```bash
./scripts/test_pddl.sh
```

It checks:

1. SWI-Prolog is installed
2. A common planner is available (e.g. `ff`)
3. PDDL files exist
4. Prolog integration modules exist
5. Domain file syntax (if a planner is available)
6. A small Prolog integration test

## Run the Prolog integration test directly

```bash
swipl -s scripts/test_pddl_integration.pl -g test_pddl_integration -t halt
```

Or from the Prolog REPL:

```prolog
?- [scripts/test_pddl_integration].
?- test_pddl_integration.
```

## Common issues

### Planner not found

Ensure the configured planner command (default: `ff`) is available in `PATH`, or set an absolute path in `prolog/liminal_logic_pddl_interface.pl`.

### No plan generated

Possible reasons:

- the planner found no solution (may be valid depending on the current state)
- domain/problem syntax errors
- command-line mismatch for your planner

To inspect output:

```bash
cat pddl/problems/plan.txt
```

### Entity not updating

Check:

- the plan is non-empty and actions parse correctly
- the entity is not already at the goal location



