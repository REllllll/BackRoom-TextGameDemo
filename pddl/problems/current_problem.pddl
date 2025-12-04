(define (problem backrooms_current)
  (:domain adversary)

  (:objects
    howler - entity
    player1 - player
    start_point yellow_hallway dark_corridor electrical_room
    the_hub manila_room supply_closet dead_end - location
  )

  (:init
    (at howler electrical_room)
    (at_player player1 start_point)
    (connected start_point yellow_hallway)
    (connected yellow_hallway start_point)
    (connected yellow_hallway start_point)
    (connected start_point yellow_hallway)
    (connected yellow_hallway dark_corridor)
    (connected dark_corridor yellow_hallway)
    (connected yellow_hallway the_hub)
    (connected the_hub yellow_hallway)
    (connected yellow_hallway supply_closet)
    (connected supply_closet yellow_hallway)
    (connected dark_corridor yellow_hallway)
    (connected yellow_hallway dark_corridor)
    (connected dark_corridor dead_end)
    (connected dead_end dark_corridor)
    (connected dead_end dark_corridor)
    (connected dark_corridor dead_end)
    (connected the_hub yellow_hallway)
    (connected yellow_hallway the_hub)
    (connected the_hub manila_room)
    (connected manila_room the_hub)
    (connected the_hub electrical_room)
    (connected electrical_room the_hub)
    (connected electrical_room the_hub)
    (connected the_hub electrical_room)
    (connected manila_room the_hub)
    (connected the_hub manila_room)
    (connected supply_closet yellow_hallway)
    (connected yellow_hallway supply_closet)
  )

  (:goal (or
    (trapped player1)
    (at howler manila_room)
  ))

)
