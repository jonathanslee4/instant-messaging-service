open Readingjson
open Readingaccounts

type menu = 
  | Login
  | SignUpUsername
  | SignUpPassword
  | Plaza
  | Chat
  | Connect

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
  current_contacts = [];
  current_user = "";
  current_receiver = "";
}

let get_current_menu st = 
  st.current_menu

let get_menu_id menu =
  match menu with
  | Login -> "login"
  | SignUpUsername -> "sign_up_username"
  | SignUpPassword -> "sign_up_password"
  | Plaza -> "plaza"
  | Chat -> "chat"
  | Connect -> "connect"

let get_current_chat st = 
  st.current_chat

let get_current_contacts st = 
  st.current_contacts

let get_current_user st =
  st.current_user

type result = Valid of t | Invalid


let change_state input st = 
  match st.current_menu with 
  | Login -> 
    if (user_exists input) then 
      Valid { 
        current_menu = Plaza; 
        current_chat = st.current_chat; 
        current_contacts = get_accepted_friends input;
        current_user = input;
        current_receiver = "";
      }
    else if input = "create account" then
      Valid {
        current_menu = SignUpUsername; 
        current_chat = st.current_chat; 
        current_contacts = st.current_contacts;
        current_user = "";
        current_receiver = "";
      }
    else 
      Invalid (** this case in main should be handled by prompting user to type/signup*)
  | SignUpUsername ->
    if (not(user_exists input)) then 
      Valid {
        current_menu = SignUpPassword; 
        current_chat = st.current_chat; 
        current_contacts = st.current_contacts;
        current_user = input;
        current_receiver = "";
      } else Invalid
  | SignUpPassword ->
    Jmodule.account_json_add st.current_user input;
    Valid {
      current_menu = Login; 
      current_chat = st.current_chat; 
      current_contacts = [];
      current_user = "";
      current_receiver = "";
    }
  | Plaza ->
    if (input = "open_requests") then 
      Valid {
        current_menu = Connect; 
        current_chat = [];
        current_contacts = st.current_contacts;
        current_user = st.current_user;
        current_receiver = "";
      } else
    if (List.mem input (get_accepted_friends input)) then 
      let filename = 
        st.current_user |> Jmodule.id_creator input |> Jmodule.json_creator in
      (if (Sys.file_exists filename) then
         Valid {
           current_menu = Chat; 
           current_chat = 
             filename |> Yojson.Basic.from_file |> Readingjson.convo_from_json;
           current_contacts = st.current_contacts;
           current_user = st.current_user;
           current_receiver = input;
         }
       else 
         Valid {
           current_menu = Chat; 
           current_chat = 
             st.current_chat; (* The current_chat is empty *)
           current_contacts = st.current_contacts;
           current_user = st.current_user;
           current_receiver = input;
         })
    else Invalid
  | Chat -> 
    let file = st.current_user |> Jmodule.id_creator st.current_receiver in
    (Jmodule.editingtext_json st.current_user input file;
     Valid {
       current_menu = Chat; 
       current_chat = file 
                      |> Jmodule.json_creator 
                      |> Yojson.Basic.from_file 
                      |> Readingjson.convo_from_json;
       current_contacts = st.current_contacts;
       current_user = st.current_user;
       current_receiver = st.current_receiver; 
     }) 
  | Connect -> 
    failwith "should not happen"

let interact_with_request tag_id input st = 
  let accepted_friends = get_accepted_friends input in 
  let pending_friends = get_pending_friends input in

  if not(user_exists input) then Invalid else
  if tag_id = "add" && not(List.mem input accepted_friends) then
    (print_string "attempting to pfp add";
     (Jmodule.pfp_add (Jmodule.id_creator input st.current_user);
      Valid {
        current_menu = Connect;
        current_chat = st.current_chat;
        current_contacts = st.current_contacts;
        current_user = st.current_user;
        current_receiver = st.current_receiver;
      })) else 
  if tag_id = "accept" && (List.mem input pending_friends) then
    (Jmodule.afp_add (Jmodule.id_creator input st.current_user);
     Valid {
       current_menu = Connect;
       current_chat = st.current_chat;
       current_contacts = st.current_contacts;
       current_user = st.current_user;
       current_receiver = st.current_receiver;
     }) else
  if tag_id = "deny" && (List.mem input pending_friends) then
    Valid {
      current_menu = Connect;
      current_chat = st.current_chat;
      current_contacts = st.current_contacts;
      current_user = st.current_user;
      current_receiver = st.current_receiver;
    } 
  else Invalid

let go_back st = 
  match st.current_menu with 
  | Login -> exit 0
  | SignUpUsername -> 
    Valid {
      current_menu = Login; 
      current_chat = [];
      current_contacts = st.current_contacts;
      current_user = "";
      current_receiver = "";
    }
  | SignUpPassword -> 
    Valid {
      current_menu = SignUpUsername; 
      current_chat = [];
      current_contacts = st.current_contacts;
      current_user = "";
      current_receiver = "";
    }
  | Plaza ->
    Valid {
      current_menu = Login; 
      current_chat = [];
      current_contacts = st.current_contacts;
      current_user = st.current_user;
      current_receiver = "";
    } 
  | Connect ->
    Valid {
      current_menu = Plaza; 
      current_chat = [];
      current_contacts = st.current_contacts;
      current_user = st.current_user;
      current_receiver = "";
    } 
  | Chat -> 
    Valid {
      current_menu = Plaza; 
      current_chat = [];
      current_contacts = st.current_contacts;
      current_user = st.current_user;
      current_receiver = "";
    } 