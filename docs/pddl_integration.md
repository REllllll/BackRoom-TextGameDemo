# Prolog <-> PDDL Integration

This document describes how `prolog/liminal_logic_pddl_interface.pl` connects Prolog game state to a PDDL planner.

## What the module does

`liminal_logic_pddl_interface.pl` provides:

1. **Problem generation**: serialize current Prolog game state into a PDDL problem file
2. **Planner invocation**: run an external PDDL planner (Fast-Forward style supported)
3. **Plan parsing**: extract action lines from the planner output
4. **State update**: apply the actions back into Prolog to update the entity position

## Typical usage

In the main loop, entity updates are triggered via:

```prolog
update_entity :-
    update_entity_from_pddl.
```

`update_entity_from_pddl/0` typically performs:

1. read player and entity locations from Prolog predicates
2. generate `pddl/problems/current_problem.pddl`
3. call the configured planner
4. parse the plan and update `at_entity/1`

## Configuration

Paths can be configured inside the module (example):

```prolog
pddl_domain_path('pddl/domains/adversary_domain.pddl').
pddl_problem_path('pddl/problems/current_problem.pddl').
pddl_plan_path('pddl/problems/plan.txt').
```

Planner command configuration:

```prolog
pddl_planner_command('ff').
% Or an absolute path:
% pddl_planner_command('/usr/local/bin/ff').
```

## Planner command format

The current implementation supports Fast-Forward style:

```
ff -o DOMAIN -f PROBLEM > PLAN
```

If you use a different planner with a different CLI, adjust the command construction logic in `call_pddl_planner/2`.

## Plan parsing

The parser accepts both:

- `move howler electrical_room the_hub`
- `(move howler electrical_room the_hub)`

## Error handling

Common cases are handled gracefully:

- missing files (domain/problem)
- planner failure (non-zero exit code)
- no plan found (entity stays in place)



