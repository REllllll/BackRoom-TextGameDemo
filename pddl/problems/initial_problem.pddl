(define (problem backrooms_initial)
  ;; =========================================================================
  ;; initial_problem.pddl
  ;; =========================================================================
  ;; Initial problem file template
  ;; Contains the initial state and goal state
  ;; Note: during gameplay, this file is generated dynamically
  ;; =========================================================================

  (:domain adversary)

  ;; -------------------------------------------------------------------------
  ;; Object definitions
  ;; -------------------------------------------------------------------------
  (:objects
    ; Entity
    howler - entity
    
    ; Player
    player1 - player
    
    ; Locations
    start_point - location
    yellow_hallway - location
    dark_corridor - location
    electrical_room - location
    the_hub - location
    manila_room - location
    supply_closet - location
    dead_end - location
  )

  ;; -------------------------------------------------------------------------
  ;; Initial state
  ;; -------------------------------------------------------------------------
  (:init
    ; Initial entity position
    (at howler electrical_room)
    
    ; Initial player position (updated at runtime)
    (at_player player1 start_point)
    
    ; Room connections (bidirectional)
    (connected start_point yellow_hallway)
    (connected yellow_hallway start_point)
    (connected yellow_hallway dark_corridor)
    (connected dark_corridor yellow_hallway)
    (connected yellow_hallway the_hub)
    (connected the_hub yellow_hallway)
    (connected yellow_hallway supply_closet)
    (connected supply_closet yellow_hallway)
    (connected dark_corridor dead_end)
    (connected dead_end dark_corridor)
    (connected the_hub manila_room)
    (connected manila_room the_hub)
    (connected the_hub electrical_room)
    (connected electrical_room the_hub)
  )

  ;; -------------------------------------------------------------------------
  ;; Goal state
  ;; -------------------------------------------------------------------------
  (:goal (or
    (trapped player1)
    (at howler manila_room)
  ))

  ;; -------------------------------------------------------------------------
  ;; Optimization metric (optional)
  ;; -------------------------------------------------------------------------
  ;; (:metric minimize (total-cost))
)

