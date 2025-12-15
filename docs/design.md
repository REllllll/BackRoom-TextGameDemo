# Design Document

## Project overview

**Liminal Logic: Escape from Level 0** is a text adventure inspired by *The Backrooms*, combining:

- **Prolog** for world modeling, rules, and game state
- **PDDL** planning to drive an adversary entity (The Howler)

## Design goals

### Why this setting works well

1. **Natural graph structure**: rooms and connections map cleanly to Prolog relations
2. **Atmosphere-first gameplay**: text adventures can deliver tension without heavy graphics
3. **Constraint-rich rules**: great fit for logic inference (e.g. "you need light to enter a dark room")

## Architecture

High-level flow:

```
Prolog game loop
  -> update game state
  -> generate PDDL problem from state
  -> call planner
  -> parse plan
  -> apply actions to move the entity
  -> continue loop
```

## Knowledge representation (Prolog)

### Static knowledge

- Room definitions: `room/1`
- Connections: `connect/3`
- Item properties: `item_property/2`

### Dynamic knowledge

- Player location: `at_player/1`
- Entity location: `at_entity/1`
- Sanity: `sanity/1`
- Inventory: `holding/1`

## Planning (PDDL)

The domain encodes actions for the adversary, such as:

- `move`: move between connected locations
- `listen`: observe / infer player location signals
- `chase`: pursue a known player location
- `roam`: default patrol behavior

## Mechanics

### Sanity

- Starts at 100
- Drops over time / actions
- Some items can restore it (e.g. almond water)

### Constraints

Example: entering a dark room requires a flashlight.

## Future work

- More rooms and items
- Richer adversary strategies
- Puzzle system
- Save/load
- Optional graphical UI



