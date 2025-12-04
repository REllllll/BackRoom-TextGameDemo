(define (problem backrooms_initial)
  ;; =========================================================================
  ;; initial_problem.pddl
  ;; =========================================================================
  ;; 初始问题文件模板
  ;; 包含初始状态和目标状态
  ;; 注意：实际游戏运行时，此文件会被动态生成
  ;; =========================================================================

  (:domain adversary)

  ;; -------------------------------------------------------------------------
  ;; 对象定义 (Object Definitions)
  ;; -------------------------------------------------------------------------
  (:objects
    ; 实体
    howler - entity
    
    ; 玩家
    player1 - player
    
    ; 房间位置
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
  ;; 初始状态 (Initial State)
  ;; -------------------------------------------------------------------------
  (:init
    ; 实体初始位置
    (at howler electrical_room)
    
    ; 玩家初始位置（会在运行时更新）
    (at_player player1 start_point)
    
    ; 房间连接关系（双向）
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
  ;; 目标状态 (Goal State)
  ;; -------------------------------------------------------------------------
  (:goal (or
    (trapped player1)
    (at howler manila_room)
  ))

  ;; -------------------------------------------------------------------------
  ;; 优化指标（可选）
  ;; -------------------------------------------------------------------------
  ;; (:metric minimize (total-cost))
)

