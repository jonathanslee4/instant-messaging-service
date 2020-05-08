(*
   Representation of dynamic instant messaging state.

   This module represents the state of the instant messaging system as it is
   being used, including the user's username, current menu, 
   conversation history/details, and contacts.
*)

(********************************************************************** *)

(** The abstract type of values representing the program state. *)
type t 

(** The type representing a menu window. *)
type menu

(** [init_state] is the initial state of the instant messaging system, which 
    represents the login menu. *)
val init_state : t

(** The type representing the result of an attempted transition. *)
type result = Valid of t | Invalid

(** TODO:Jonny *)
type result_prime = PValid of t 
                  | Invalid_Existless 
                  | Invalid_Add_Already_Added
                  | Invalid_Add_Already_Friended 
                  | Invalid_Add_Pending
                  | Invalid_Unrecognizable

(** [get_current_menu st] is the menu of the instant messaging interface in 
    which the user is currently located in [st]. *)
val get_current_menu : t -> menu

(** [get_menu_id menu] is the identifier that correlates to [menu]. *)
val get_menu_id : menu -> string

(** [get_current_user st] is the username of the current user in [st]. *)
val get_current_user : t -> string

(** [get_current_contacts st] is the list of friends that the user can currently
    chat with in [st]. *)
val get_current_contacts : t -> string list

(** [get_current_chat st] is message list representing the conversation history 
    between the user and the person the user has chosen to chat with in [st]. *)
val get_current_chat : t -> Readingjson.message list

(** [change_state input st] is [Valid t] if [input] from the current menu in 
    [st] is not invalid. An input can be invalid for several reasons:
    - the user is in the Login menu and inputs a username that doesn't exist
    - the user is in the LoginVerify menu and inputs wrong password 
    - the user is in the SignUpUsername menu and tries to create an account
        w/ a username that already exists
    - the user is in the Plaza and tries to chat with someone who isn't their 
        friend *)
val change_state : string -> t -> result 

(** TODO:Jonny *)
val interact_with_request : string -> string -> t -> result_prime

(** TODO:Jonny *)
val go_back : t -> result