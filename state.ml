type username = string

type users = username list

type text = {
  sender : string; 
  message : string;
}

type menu = 
  | Login
  | Plaza
  | Chat of string

type t = {
  current_menu : menu;
  current_chat : Readingjson.convo;
  current_contacts : Readingjson.contacts;
  current_user : string;
}

let init_state j = {
  current_user = "";
  current_menu = Login;
  current_chat = [];
  current_contacts = Readingjson.contacts_from_json j;

}

let get_current_menu st = 
  st.current_menu

let get_current_chat st = 
  st.current_chat

let get_current_contacts st = 
  st.current_contacts

let get_current_user st =
  st.current_user

type result = Valid of t | Invalid

let is_valid_username input st = 
  List.mem input st.current_contacts

let next_menu input st = 
  match st.current_menu with 
  | Login -> 
    if (is_valid_username input st) then 
      Valid {current_user = input;
             current_menu = Plaza; 
             current_chat = []; 
             current_contacts = st.current_contacts}
    else Invalid
  | Plaza -> 
    if input = "/back" then 
      Valid {current_user = ""; 
             current_menu = Login;
             current_chat = [];
             current_contacts = st.current_contacts } else
    if (is_valid_username input st && input <> st.current_user) then
      if Sys.file_exists (Jmodule.id_creator input st.current_user) then
        Valid {current_user = st.current_user; 
               current_menu = Chat input; 
               current_chat = Readingjson.convo_from_json
                   (Yojson.Basic.from_file 
                      (Jmodule.id_creator input st.current_user)); 
               current_contacts = st.current_contacts}
      else 
        Valid {current_user = st.current_user; 
               current_menu = Chat input; 
               current_chat = []; 
               current_contacts = st.current_contacts
              }
    else Invalid
  | Chat username -> 
    if input = "/back" then 
      Valid {current_user = st.current_user; 
             current_menu = Plaza; 
             current_chat = []; 
             current_contacts = st.current_contacts} 
    else 
      Valid {current_user = st.current_user; 
             current_menu = Plaza; 
             current_chat = []; 
             current_contacts = st.current_contacts}






