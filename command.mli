(**
   Parsing of player commands.
*)

(** The type [phrase] represents the phrase that is a 
    player input.  
    A [phrase] is not permitted to be the empty string. *)
type phrase = string


(** The type [command] represents a player input which depends on the current
    menu. *)
type command = 
  | Username of phrase
  | Engage of phrase
  | Send of phrase
  | Quit

(** Raised when an empty command is parsed when a user is in the Login menu. *)
exception Empty_Username

(** Raised when an empty command is parsed when a user is in the Plaza menu. *)
exception Empty_Engage

(** Raised when an empty command is parsed when a user is in the Send menu. *)
exception Empty_Send

(** Raised when a malformed command is encountered in the Login menu. *)
exception Malformed_Username

(** Raised when a malformed command is encountered in the Plaza menu. *)
exception Malformed_Engage


(** [parse str] parses a player's input into a [command], as follows. 
    Examples: 
    - [parse "how have you beeen?"] within the Chat menu is 
      [Send "how have you been?"].
    - [parse "Jessica"] within the Plaza menu is [Engage "Jessica"]. 
    - [parse "/back"] is [Back]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {malformed} if the phrase is more than one word in the Login or Plaza
    menus. *)
val parse : string -> string -> command

(* END DO NOT CHANGE
 **********************************************************************)
