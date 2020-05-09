(**
   COMMAND MODULE: Parsing of player commands.
*)

(** The type [command] represents a player input which depends on the current
    menu. *)
type command = 
  | Sign_Up
  | New_Username of string
  | New_Password of string
  | Login_As of string
  | Login_Password of string
  | Chat_With of string
  | Send of string
  | Open_Requests
  | Move_Request of string * string
  | Back
  | Quit
  | Current

(** Raised when an empty command is parsed when a user is in the Login menu. *)
exception Empty_Login_Id

(** Raised when an empty command is parsed when a user is in the LoginVerify 
    menu. *)
exception Empty_Login_Password

(** Raised when an empty command is parsed when a user is in the Plaza menu. *)
exception Empty_Chat_With_Id

(** Raised when an empty command is parsed when a user is in the Send menu. *)
exception Empty_Send

(** Raised when an empty command is parsed when a user is in the SignUpUsername
    menu. *)
exception Empty_New_Username

(** Raised when an empty command is parsed when a user is in the SignUpPassword
    menu. *)
exception Empty_New_Password

(** Raised when empty command is parsed in the Connect menu. *)
exception Empty_Connect

(** Raised when a malformed command is encountered in the Login menu. *)
exception Malformed_Login_Id

(** Raised when a malformed command is encountered in the LoginVerify menu. 
    AKA the input for the password had a space. *)
exception Malformed_Login_Password

(** Raised when a malformed command is encountered in the Plaza menu. *)
exception Malformed_Chat_With

(** Raised when a malformed command is encountered in the Plaza menu. 
    Handles the specific case where you chat yourself. *)
exception Malformed_Chat_With_Self

(** Raised when a malformed command is encountered in the SignUpUsername menu.*)
exception Malformed_New_Username

(** Raised when a malformed command is encountered in the SignUpPassword menu.*)
exception Malformed_New_Password

(** Raised when a malformed command is encountered in the Connect menu. *)
exception Malformed_Connect

(** [parse str] parses a player's input into a [command], as follows. 
    Examples: 
    - [parse "how have you been?"] within the Chat menu is 
      [Send "how have you been?"].
    - [parse "Jessica"] within the Plaza menu is [Chat_With "Jessica"]. 
    - [parse "/back"] is [Back]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed_<spec>] if the command is 
    malformed, where [spec] depends on the specific menu/case that caused the 
    malformed command to occur. A command is {malformed} if the phrase is more
    than one word in Login, LoginVerify, Plaza, SignUpUsername, SignUpPassword,
    or Connect. *)
val parse : string -> string -> string -> command
