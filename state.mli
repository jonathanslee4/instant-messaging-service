(*
   Representation of dynamic instant messaging state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(********************************************************************** *)

(** The abstract type of values representing the program state. *)
type t 

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Adventure.t -> t

(** The type representing the result of an attempted movement. *)
type result = Valid of t | Invalid

(** [next_menu input st] is [Valid t] if [input] from the current menu in state 
    [st] is not invalid. An input is only invalid if the current menu is
    [Login] or [Plaza] and the input is not a pre-existing username. *)
val next_menu : string -> t -> result 


(********************************************************************** *)

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_room_id : t -> string

(** [visited st] is a set-like list of the room identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)
val visited : t -> string list

(** [current_score st] is the current score of the game in [st] *)
val current_score : t -> int


(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(** [get_state ex adv st] is the new game state after the adventurer
    has attempted movement through [ex] in [st] and [adv]  *)
val get_state : Adventure.exit_name -> Adventure.t -> t -> t

(** [take itm adv st] is [result] if attempting to take [itm] in state [st] 
    and adventure [adv]. If [itm] is an item from the adventurer's current room,
    then [result] is [Legal st'], where in [st'], [itm] is now removed from the 
    current_room and in the adventurer's inventory. *)
val take : Adventure.item_name -> Adventure.t -> t -> result

val current_ir_tracker : t -> (Adventure.room_id * Adventure.item_name list) list

val get_items_names : (Adventure.room_id * Adventure.item_name list) 
    list -> Adventure.room_id ->  Adventure.item_name list

val drop : Adventure.item_name -> Adventure.t -> t -> result

val current_inventory : t -> Adventure.item_name list

val is_win : Adventure.room_id -> Adventure.t -> t -> bool 
