
type phrase = string
type command = 
  | Username of phrase
  | Engage of phrase
  | Send of phrase
  | Quit

exception Empty_Username
exception Empty_Engage
exception Empty_Send

exception Malformed_Username
exception Malformed_Engage
exception Malformed_Engage_Identity 

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

let parse current_menu_id current_user_id str =
  let strlist = striplist str in
  match strlist with
  | [] -> (if current_menu_id = "login" then raise Empty_Username else
           if current_menu_id = "plaza" then raise Empty_Engage else
             raise Empty_Send)
  | hd :: tl -> 
    if current_menu_id = "login" then 
      (if List.length strlist <> 1  then raise Malformed_Username else
         Username hd) else
    if current_menu_id = "plaza" then
      (if List.length strlist <> 1 then raise Malformed_Engage else
       if hd = current_user_id then raise Malformed_Engage_Identity else 
       if hd = "/quit" then Quit else
         Engage hd)
      (* Current menus is Chat *)     
    else if hd = "/quit" then Quit else
      Send (String.concat " " (strlist))
