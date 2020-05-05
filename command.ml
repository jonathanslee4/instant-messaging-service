
type phrase = string

type request_tag = Add | Accept | Deny
type command = 
  | Login_As of phrase
  | Chat_With of phrase
  | Send of phrase
  | Open_Requests
  | Move_Request of request_tag * phrase
  | Back
  | Quit

exception Empty_Login_Id
exception Empty_Chat_With_Id
exception Empty_Send

exception Malformed_Login_Id
exception Malformed_Chat_With
exception Malformed_Chat_With_Self

(** [remove_empty slst] is a string list with all empty strings in slst
    removed *)
let rec remove_empty slst =
  match slst with
  | [] -> []
  | hd::tl -> if hd = "" then remove_empty tl else hd :: remove_empty tl

(** [striplist str] is a string list whose elements are words formed from str.
    A word is consecutive sequence of non-space characters. If str is
    empty, an Empty exception is raised *)
let striplist str = 
  if str = "" then [] else
    str |> String.split_on_char ' ' |> remove_empty

let get_string strlist=
  match strlist with
  | [] -> failwith "string list must have 2 elements"
  | hd :: tl -> hd

let get_tag_id tag = 
  match tag with
  | Add -> "add"
  | Accept -> "accept"
  | Deny -> "deny"

let parse current_menu_id current_user_id str =
  let strlist = striplist str in
  match strlist with
  | [] -> (if current_menu_id = "login" then raise Empty_Login_Id else
           if current_menu_id = "plaza" then raise Empty_Chat_With_Id else
             raise Empty_Send)
  | hd :: tl -> 
    if hd = "/quit" then Quit else
    if hd = "/back" then Back else
    if current_menu_id = "login" then 
      (if List.length strlist <> 1  then raise Malformed_Login_Id else
         Login_As hd) else
    if current_menu_id = "plaza" then
      (if hd = "/connect" then Open_Requests else
       if List.length strlist <> 1 then raise Malformed_Chat_With else
       if hd = current_user_id then raise Malformed_Chat_With_Self else 
         Chat_With hd)
      (* Current menus is Chat *) else
    if current_menu_id = "connect" then 
      (if List.length strlist <> 2 then failwith "havent imp exceptions yet" else
       if hd = "add" then Move_Request (Add, get_string tl) else
       if hd = "accept" then Move_Request (Accept,get_string tl) else
         Move_Request (Deny, get_string tl))
    else Send (String.concat " " (strlist))
