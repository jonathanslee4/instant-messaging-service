(*
   Representation of dynamic instant messaging state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(********************************************************************** *)

(** The abstract type of values representing the program state. *)
type t 
type menu
type message

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Yojson.Basic.t -> t

(** The type representing the result of an attempted movement. *)
type result = Valid of t | Invalid

val get_current_menu : t -> menu
val get_current_user : t -> string
val get_current_contacts : t -> string list
val get_current_chat : t -> message list

(** [next_menu input st] is [Valid t] if [input] from the current menu in state 
    [st] is not invalid. An input is only invalid if the current menu is
    [Login] or [Plaza] and the input is not a pre-existing username. *)
val next_menu : string -> t -> result 
