type username = string

type users = username list

type text = {
  sender : string; 
  message : string;
}

type menu = 
  | Login
  | Plaza
  | Chat of username

type t = {
  current_menu : menu;
  current_json : Reader.t;
  recent_texts : text list;
  (* new_user : username ; *)
}

let init_state rdr = {
  current_menu = Login;
  current_json = Reader.getjson;
  recent_texts = []; 
}

let get_current_menu st = 
  st.current_menu

let get_recent_texts st = 
  st.recent_texts

type result = Valid of t | Invalid

(** [is_valid_username input] is true if [input] is an existing username. *)
(******* MERGE WITH WEMI *******)

(* let is_valid_username input = 
   List.mem input Reader.users; *)

let next_menu input st = 
  match st.current_menu with 
  | Login -> 
    if (is_valid_username input) then 
      Valid {current_menu = Plaza; recent_texts = st.recent_texts;}
    else Invalid
  | Plaza -> 
    if input = "/back" then 
      Valid {current_menu = Login; recent_texts = st.recent_texts } else
    if (is_valid_username input) then 
      Valid {current_menu = Chat input; recent_texts = st.recent_texts;}
    else Invalid
  | Chat username -> 
    if input = "/back" then 
      Valid {current_menu = Login; recent_texts = st.recent_texts } 
    else Valid {current_menu = Login; recent_texts = {sender = username; message = input} :: st.recent_texts}






