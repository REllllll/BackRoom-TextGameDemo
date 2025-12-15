# Liminal Logic: Escape from Level 0

A text-adventure escape game inspired by **The Backrooms**, built with **SWI-Prolog** (knowledge representation + rules) and **PDDL** planning (enemy behavior).

## Overview

You have noclipped out of reality into **Level 0**. Find the **Manila Room** and escape before you lose your sanity or get caught by the entity.

## Key features

- **Prolog knowledge base**: static world facts + dynamic game state as logic predicates
- **PDDL planning**: automated planning to drive the adversary (The Howler)
- **Realtime interaction**: Prolog generates a PDDL problem from the current state, calls a planner, then applies the plan
- **Rule-based constraints**: logical checks (e.g., dark rooms require a flashlight)

## Repository layout

```
BackRoom-TextGameDemo/
├── README.md
├── prolog/
│   ├── knowledge_base.pl          # Static world facts (rooms, connections, items)
│   ├── game_state.pl              # Dynamic state (player/entity position, sanity, inventory)
│   ├── game_logic.pl              # Commands + rules (move/take/drop/use/look)
│   ├── win_conditions.pl          # Win/lose checks
│   ├── pddl_interface.pl          # Prolog <-> PDDL integration
│   ├── http_server.pl             # REST API server for the playground UI
│   └── main.pl                    # CLI game entry point
├── pddl/
│   ├── domains/
│   │   └── adversary_domain.pddl  # Adversary domain definition
│   └── problems/
│       ├── initial_problem.pddl   # Template (runtime uses dynamically generated problems)
│       └── .gitkeep
├── scripts/
│   ├── dev.sh                     # Dev container helper
│   ├── run_game.sh                # Game launcher (host or container)
│   └── test_pddl.sh               # PDDL environment check
├── docs/
│   ├── design.md                  # Design notes
│   ├── pddl_integration.md        # Integration details
│   └── pddl_testing.md            # Testing guide
└── playground/
    ├── index.html
    ├── app.js
    ├── style.css
    └── api/proxy/[...path].js     # Vercel Serverless proxy
```

## Requirements

- **Docker + Docker Compose** (recommended)
- Or local installation:
  - **SWI-Prolog** (7+ recommended)
  - Optional: a **PDDL planner** (the project supports Fast-Forward style `ff`)

## Run (recommended): Docker dev container

Start the dev container:

```bash
./scripts/dev.sh start
```

Enter the container:

```bash
./scripts/dev.sh shell
```

Run the CLI game inside the container:

```bash
swipl -s prolog/main.pl -g start
```

## Run (host): launcher script

On the host, the launcher will prefer running via the dev container if available:

```bash
./scripts/run_game.sh
```

## Game commands

- `move(direction)` - move `north|south|east|west`
- `take(item)` - pick up an item
- `drop(item)` - drop an item you are holding
- `use(item)` - use an item
- `inventory` / `inv` - show what you are carrying (max 2 items)
- `look` - describe current room
- `quit` - exit the game

## Mechanics (high level)

- **Sanity**: decreases as you move; can be restored by items (e.g. almond water).
- **Dark rooms**: require a flashlight.
- **Win**: reach `manila_room` (optionally requiring `key`, depending on rule configuration).
- **Lose**: sanity reaches 0, or the entity reaches your room.

## PDDL integration

The entity behavior is driven by PDDL planning:

1. Prolog serializes the current game state into a PDDL problem file
2. A planner is executed
3. The plan is parsed back into actions
4. Prolog applies the actions to update the entity

See `docs/pddl_integration.md` for details.

## Test PDDL setup

Use the helper script:

```bash
./scripts/test_pddl.sh
```

It checks for SWI-Prolog, common planners, required files, and runs a small integration test. See `docs/pddl_testing.md`.

## License

Course project / demo.

## References

- The Backrooms Wiki: `https://backrooms.fandom.com/`
- SWI-Prolog documentation: `https://www.swi-prolog.org/pldoc/`
- Planning Wiki: `https://planning.wiki/`



