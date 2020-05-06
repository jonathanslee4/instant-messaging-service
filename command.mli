(**
   Parsing of player commands.
*)

(** The type [phrase] represents the phrase that is a 
    player input.  
    A [phrase] is not permitted to be the empty string. *)
type phrase = string
type request_tag

(** The type [command] represents a player input which depends on the current
    menu. *)
type command = 
  | Sign_Up
  | New_Username of phrase
  | New_Password of phrase
  | Login_As of phrase
  | Chat_With of phrase
  | Send of phrase
  | Open_Requests
  | Move_Request of request_tag * phrase
  | Back
  | Quit

(** Raised when an empty command is parsed when a user is in the Login menu. *)
exception Empty_Login_Id

(** Raised when an empty command is parsed when a user is in the Plaza menu. *)
exception Empty_Chat_With_Id

(** Raised when an empty command is parsed when a user is in the Send menu. *)
exception Empty_Send

(** Raised when a malformed command is encountered in the Login menu. *)
exception Malformed_Login_Id

(** Raised when a malformed command is encountered in the Plaza menu. *)
exception Malformed_Chat_With

(** Raised when a malformed command is encountered in the Plaza menu. 
    Handles the specific case where you chat yourself. *)
exception Malformed_Chat_With_Self

(** Raised when username input is empty in SignUpUsername.*)
exception Empty_New_Username

(** Raised when password input is empty in SignUpPassword.*)
exception Empty_New_Password

(** Raised when malformed username is entered in SignUpUsername.*)
exception Malformed_New_Username

(** Raised when malformed password is entered in SignUpPassword.*)
exception Malformed_New_Password


val get_tag_id : request_tag -> string

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
val parse : string -> string -> string -> command
