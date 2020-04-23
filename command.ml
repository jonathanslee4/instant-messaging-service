
type phrase = string
type command = 
  | Engage of phrase
  | Send of phrase
  | Username of phrase
  | Back

exception Empty

exception Malformed

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
  if str = "" then raise Empty else
    str |> String.split_on_char ' ' |> remove_empty

(** headify [slist] is the stringlist whose head is the head of slist, and whose
    tail stores the string concatenation of the tail of slist. *)
let headify slist = 
  match slist with
  | [] -> raise Empty
  | hd :: tl -> hd :: [String.concat " " tl]

let parse current_menu str =
  match striplist str |> headify with
  | [] -> raise Empty
  | hd :: tl -> 
    if current_menu = Login then 
      (if tl <> [] then raise Malformed else
         Username hd) else
    if current_menu = Plaza then
      (if tl <> [] then raise Malformed else
       if hd = "/back" then Back else
         Engage hd)
      (* Current menus is Chat *)     
    else if hd = "/back" then Back else
      Send (String.concat " " (hd :: tl))
