open Readingjson

type menu = 
  | Login
  | Plaza
  | Chat 

type t = {
  current_menu : menu;
  current_chat : convo;
  current_contacts : string list;
  current_user : string;
  current_receiver : string;
}

let init_state (*j*) = { (* shouldn't take in a json *)
  current_menu = Login;
  current_chat = [];
  (* current_contacts = Readingjson.contacts_from_json j; *)
  current_contacts = "contacts.json" 
                     |> Yojson.Basic.from_file 
                     |> Readingjson.contacts_from_json;
  current_user = "";
  current_receiver = "";
}

let get_current_menu st = 
  st.current_menu

let get_menu_id menu =
  match menu with
  | Login -> "login"
  | Plaza -> "plaza"
  | Chat -> "chat"

let get_current_chat st = 
  st.current_chat

let get_current_contacts st = 
  st.current_contacts

let get_current_user st =
  st.current_user

type result = Valid of t | Invalid

(** [is_existing_username input st] is true if the username already exists 
    in the database aka the username is valid. *)
let is_existing_username input st = 
  List.mem input st.current_contacts

let next_menu input st = 
  match st.current_menu with 
  | Login -> 
    if (is_existing_username input st) then 
      Valid { 
        current_menu = Plaza; 
        current_chat = st.current_chat; 
        current_contacts = st.current_contacts;
        current_user = input;
        current_receiver = ""; }
    else Invalid (** UPDATE in MS2: should go to a create account page *)
  | Plaza ->
    if (input = "/back") then 
      Valid {
        current_menu = Login; 
        current_chat = [];
        current_contacts = st.current_contacts;
        current_user = "";
        current_receiver = "";
      } else 
    if (is_existing_username input st) then 
      let filename = 
        st.current_user |> Jmodule.id_creator input |> Jmodule.json_creator in
      (if (Sys.file_exists filename) then
         Valid {
           current_menu = Chat; 
           current_chat = 
             filename |> Yojson.Basic.from_file |> Readingjson.convo_from_json;
           current_contacts = st.current_contacts;
           current_user = st.current_user;
           current_receiver = input;}
       else 
         Valid {
           current_menu = Chat; 
           current_chat = 
             st.current_chat; (* The current_chat is empty *)
           current_contacts = st.current_contacts;
           current_user = st.current_user;
           current_receiver = input;})
    else Invalid
  | Chat -> 
    if (input = "/back") then 
      Valid {
        current_menu = Plaza; 
        current_chat = [];
        current_contacts = st.current_contacts;
        current_user = st.current_user;
        current_receiver = "";
      }
    else 
      let file = st.current_user |> Jmodule.id_creator st.current_receiver in
      (Jmodule.editing_json st.current_user input file;
       Valid {
         current_menu = Chat; 
         current_chat = file 
                        |> Jmodule.json_creator 
                        |> Yojson.Basic.from_file 
                        |> Readingjson.convo_from_json;
         current_contacts = st.current_contacts;
         current_user = st.current_user;
         current_receiver = st.current_receiver; } )







