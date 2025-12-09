(define (domain adversary)
  ;; =========================================================================
  ;; adversary_domain.pddl
  ;; =========================================================================
  ;; PDDL domain definition for the entity (The Howler)
  ;; Defines entity actions: stay in place, move step by step
  ;; =========================================================================

  (:requirements :strips :typing :conditional-effects)

  ;; -------------------------------------------------------------------------
  ;; Type Definitions
  ;; -------------------------------------------------------------------------
  (:types
    location    ; Room location
    entity      ; Entity (The Howler)
    player      ; Player
  )

  ;; -------------------------------------------------------------------------
  ;; Predicate Definitions
  ;; -------------------------------------------------------------------------
  (:predicates
    ;; Location-related
    (at ?e - entity ?l - location)      ; Entity is at a location
    (at_player ?p - player ?l - location) ; Player is at a location
    
    ;; Connections
    (connected ?from - location ?to - location) ; Connection between rooms
    
    ;; Sensing
    (noise_at ?l - location)            ; Noise at a location
    (player_known ?p - player ?l - location) ; Known player location
    (player_attempted_door ?p - player ?from - location ?to - location) ; Player attempted a door
    
    ;; Goal state
    (trapped ?p - player)                ; Player trapped (entity and player at same location)
  )

  ;; -------------------------------------------------------------------------
  ;; Action: Stay
  ;; Entity keeps its current position
  ;; -------------------------------------------------------------------------
  (:action stay
    :parameters (?e - entity ?loc - location)
    :precondition (and
      (at ?e ?loc)
    )
    :effect (and
      (at ?e ?loc)
    )
  )

  ;; -------------------------------------------------------------------------
  ;; Action: Move
  ;; Entity moves to an adjacent room (step by step)
  ;; -------------------------------------------------------------------------
  (:action move
    :parameters (?e - entity ?from - location ?to - location)
    :precondition (and
      (at ?e ?from)
      (connected ?from ?to)
    )
    :effect (and
      (not (at ?e ?from))
      (at ?e ?to)
    )
  )
)

