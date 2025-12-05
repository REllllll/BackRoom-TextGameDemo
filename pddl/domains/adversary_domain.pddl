(define (domain adversary)
  ;; =========================================================================
  ;; adversary_domain.pddl
  ;; =========================================================================
  ;; 实体（The Howler）的 PDDL 域定义
  ;; 定义实体的动作：留在原地、移动（一步步移动）
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
    (player_attempted_door ?p - player ?from - location ?to - location) ; 玩家尝试开门
    
    ;; 目标状态
    (trapped ?p - player)                ; 玩家被困（实体和玩家在同一位置）
  )

  ;; -------------------------------------------------------------------------
  ;; 动作：留在原地 (Action: Stay)
  ;; 实体保持当前位置不变
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
  ;; 动作：移动 (Action: Move)
  ;; 实体移动到相邻房间（一步步移动）
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

