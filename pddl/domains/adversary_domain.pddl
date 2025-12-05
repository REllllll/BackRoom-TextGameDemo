(define (domain adversary)
  ;; =========================================================================
  ;; adversary_domain.pddl
  ;; =========================================================================
  ;; 实体（The Howler）的 PDDL 域定义
  ;; 定义实体的动作：移动、监听、追逐
  ;; =========================================================================

  (:requirements :strips :typing :conditional-effects)

  ;; -------------------------------------------------------------------------
  ;; 类型定义 (Type Definitions)
  ;; -------------------------------------------------------------------------
  (:types
    location    ; 房间位置
    entity      ; 实体（The Howler）
    player      ; 玩家
  )

  ;; -------------------------------------------------------------------------
  ;; 谓词定义 (Predicate Definitions)
  ;; -------------------------------------------------------------------------
  (:predicates
    ;; 位置相关
    (at ?e - entity ?l - location)      ; 实体在某个位置
    (at_player ?p - player ?l - location) ; 玩家在某个位置
    
    ;; 连接关系
    (connected ?from - location ?to - location) ; 房间之间的连接
    
    ;; 感知相关
    (noise_at ?l - location)            ; 某个位置有噪音
    (player_known ?p - player ?l - location) ; 已知玩家位置
    
    ;; 目标状态
    (trapped ?p - player)                ; 玩家被困（实体和玩家在同一位置）
  )

  ;; -------------------------------------------------------------------------
  ;; 动作：移动 (Action: Move)
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

  ;; -------------------------------------------------------------------------
  ;; 动作：监听 (Action: Listen)
  ;; 实体可以听到玩家在相邻房间的声音
  ;; -------------------------------------------------------------------------
  (:action listen
    :parameters (?e - entity ?entity_loc - location ?adjacent_loc - location ?p - player)
    :precondition (and
      (at ?e ?entity_loc)
      (connected ?entity_loc ?adjacent_loc)
      (at_player ?p ?adjacent_loc)
    )
    :effect (and
      (player_known ?p ?adjacent_loc)
    )
  )
  
  ;; -------------------------------------------------------------------------
  ;; 动作：监听噪音 (Action: Listen to Noise)
  ;; 实体可以听到噪音并发现玩家位置
  ;; -------------------------------------------------------------------------
  (:action listen_noise
    :parameters (?e - entity ?l - location ?p - player)
    :precondition (and
      (at ?e ?l)
      (noise_at ?l)
      (at_player ?p ?l)
    )
    :effect (and
      (player_known ?p ?l)
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 动作：追逐 (Action: Chase)
  ;; -------------------------------------------------------------------------
  (:action chase
    :parameters (?e - entity ?from - location ?to - location ?p - player)
    :precondition (and
      (at ?e ?from)
      (connected ?from ?to)
      (player_known ?p ?to)
    )
    :effect (and
      (not (at ?e ?from))
      (at ?e ?to)
      (when (at_player ?p ?to) (trapped ?p))
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 动作：巡逻 (Action: Roam)
  ;; -------------------------------------------------------------------------
  (:action roam
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

