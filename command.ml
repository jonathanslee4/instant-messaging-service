
type phrase = string

type command = 
  | Sign_Up
  | New_Username of phrase
  | New_Password of phrase
  | Login_As of phrase
  | Chat_With of phrase
  | Send of phrase
  | Open_Requests
  | Move_Request of phrase * phrase
  | Back
  | Quit

exception Empty_Login_Id
exception Empty_Chat_With_Id
exception Empty_Send
exception Empty_New_Username
exception Empty_New_Password
exception Empty_Connect

exception Malformed_Login_Id
exception Malformed_Chat_With
exception Malformed_Chat_With_Self
exception Malformed_New_Username
exception Malformed_New_Password
exception Malformed_Connect

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

let parse current_menu_id current_user_id str =
  let strlist = striplist str in
  match strlist with
  | [] -> (if current_menu_id = "login" then raise Empty_Login_Id else
           if current_menu_id = "plaza" then raise Empty_Chat_With_Id else
           if current_menu_id = "sign_up_username" then raise Empty_New_Username else
           if current_menu_id = "sign_up_password" then raise Empty_New_Password else
           if current_menu_id = "connect" then raise Empty_Connect else 
             raise Empty_Send)
  | hd :: tl -> 
    if hd = "/quit" then Quit else
    if hd = "/back" then Back else
    if current_menu_id = "login" then 
      (if List.length strlist <> 1  then raise Malformed_Login_Id else
       if hd = "/signup" then Sign_Up else
         Login_As hd) else
    if current_menu_id = "sign_up_username" then 
      (if List.length strlist <> 1 then raise Malformed_New_Username else
         New_Username hd) else 
    if current_menu_id = "sign_up_password" then 
      (if List.length strlist <> 1 then raise Malformed_New_Password else
         New_Password hd) else 
    if current_menu_id = "plaza" then
      (if hd = "/connect" then Open_Requests else
       if List.length strlist <> 1 then raise Malformed_Chat_With else
       if hd = current_user_id then raise Malformed_Chat_With_Self else 
         Chat_With hd)
      (* Current menus is Chat *) else
    if current_menu_id = "connect" then 
      (if List.length strlist <> 2 then raise Malformed_Connect else
       if hd = "add" then Move_Request ("add", get_string tl) else
       if hd = "accept" then Move_Request ("accept",get_string tl) else
         Move_Request ("deny", get_string tl))
    else Send (String.concat " " (strlist))
