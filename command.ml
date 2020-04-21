type object_phrase = string list

type command = 
  | Engage of object_phrase
  | Send of object_phrase
  | Username of object_phrase
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

let headify slist = 
  match slist with
  | [] -> 0
  | hd :: tl -> hd

let parse str current_menu =
  match striplist str with
  | [] -> raise Empty
  | hd :: tl ->
    if current_menu = Signin then 
      if isValid hd then Username tl






          if hd <> "" && hd <> "go" && hd <> "score" && hd <> "take" 
             && hd <> "inventory" && hd <> "drop" && hd <> "use"
          then raise Malformed else
          if (hd = "quit" || hd = "score" || hd = "inventory") && tl <> [] 
          then raise Malformed else
          if (hd = "go" || hd = "take" || hd = "drop" || hd = "use") && tl = [] then 
            raise Malformed else 
          if hd = "quit" then Quit else
          if hd = "score" then Score else
          if hd = "take" then Take tl else
          if hd = "inventory" then Inventory else
          if hd = "drop" then Drop tl else
          if hd = "use" then Use tl else
            Go tl

