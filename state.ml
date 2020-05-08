open Readingjson
open Readingaccounts

type menu = 
  | Login
  | LoginVerify
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
  | LoginVerify -> "password_verification"
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

type result_prime = PValid of t 
                  | Invalid_Existless 
                  | Invalid_Add_Already_Added
                  | Invalid_Add_Already_Friended 
                  | Invalid_Unrecognizable

let change_state input st = 
  match st.current_menu with 
  | Login -> 
    if (user_exists input) then 
      Valid { 
        current_menu = LoginVerify; 
        current_chat = st.current_chat; 
        current_contacts = st.current_contacts;
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
  | LoginVerify-> 
    if "logindetails.json" 
       |> Yojson.Basic.from_file 
       |> accounts_from_json 
       |> (is_verified_password st.current_user input) 
    then 
      Valid {
        current_menu = Plaza; 
        current_chat = st.current_chat; 
        current_contacts = get_accepted_friends input;
        current_user = input;
        current_receiver = "";
      }
    else Invalid (* this case in main should be handled by telling user that is the incorrect password and to try again *)
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
  let pending_friends_with_tag = "pfp.json" |> Yojson.Basic.from_file |> 
                                 Readingjson.pending_friend_pairs_from_json in
  if tag_id = "add" then 
    if not(user_exists input) then Invalid_Existless else
    if List.mem input accepted_friends then Invalid_Add_Already_Friended else
    if List.mem ((Jmodule.id_creator st.current_user input)^"&"^
                 st.current_user) pending_friends_with_tag then Invalid_Add_Already_Added else
      let pfpnum = 
        "pfp.json" |> Yojson.Basic.from_file |> 
        Readingjson.pending_friend_pairs_from_json |> List.length in
      if pfpnum = 0 then 
        (Jmodule.pfp_empty 
           ((Jmodule.id_creator input st.current_user)^"&"^st.current_user)) 
      else
        (Jmodule.pfp_add 
           ((Jmodule.id_creator input st.current_user)^"&"^st.current_user));
      PValid {
        current_menu = Connect;
        current_chat = st.current_chat;
        current_contacts = st.current_contacts;
        current_user = st.current_user;
        current_receiver = st.current_receiver;
      } else 
  if tag_id = "accept" then
    if not(List.mem input pending_friends) then Invalid_Existless else
      (Jmodule.afp_add (Jmodule.id_creator input st.current_user);
       PValid {
         current_menu = Connect;
         current_chat = st.current_chat;
         current_contacts = st.current_contacts;
         current_user = st.current_user;
         current_receiver = st.current_receiver;
       }) else
  if tag_id = "deny" then
    if not(List.mem input pending_friends) then Invalid_Existless else
      PValid {
        current_menu = Connect;
        current_chat = st.current_chat;
        current_contacts = st.current_contacts;
        current_user = st.current_user;
        current_receiver = st.current_receiver;
      } 
  else Invalid_Unrecognizable

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
  | LoginVerify -> 
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