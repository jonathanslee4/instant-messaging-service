open Command
open State
open Readingjson
open Jmodule


let rec print_slist slist =
  match slist with
  | [] -> print_string ""
  | hd :: tl -> (print_string hd); print_slist tl

let rec print_contacts st con_list =
  match con_list with
  |[] -> print_string ""
  |hd::tl-> 
    if hd = get_current_user st then print_contacts st tl
    else
      (print_endline(hd); 
       print_contacts st tl)
(* print_endline(hd); print_contacts st tl  *)

let rec print_convo t=
  match t with
  |[]->print_string ""
  |x::xs-> 
    (output_convo_line x; (print_convo xs))

let print_login st =
  ANSITerminal.(print_string [green]
                  "\nWelcome to the login page. What is your username?\n>> ")

let print_new_username st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease enter a new one-word username. This is how others will see you on the service.\n>> ")

let print_new_password1 st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease enter a new one-word password.\n>> ") 

let print_new_password2 st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease enter this password again, to confirm it is the same.\n>> ")

let print_plaza st =                      
  ANSITerminal.(print_string [magenta] "\nWho would you like to chat with? Type /back to log out.\n");
  print_contacts st ((get_current_contacts st));
  ANSITerminal.(print_string [magenta] "\n>> ")

let print_connect st =
  let friend_request_num = List.length (get_pending_friends (get_current_user st)) in
  if friend_request_num = 0 then
    ANSITerminal.(print_string [magenta]
                    "\nYou haven't received any new friend requests yet. Type add followed by a name to send a friend request! When you have received a request, you can type accept or deny followed by that person's name. \n>> ")
  else 
    (ANSITerminal.(print_string [magenta] "\nYou have new friend request(s)!"); 
     print_contacts st (get_pending_friends (get_current_user st)))


let print_whole_chat st=
  print_string "\n";
  ANSITerminal.erase Screen ;
  print_convo (List.rev(get_current_chat st));

  ANSITerminal.(print_string [magenta] "Type your message or /back to return to your contacts.\n>> ")

let print_new_message st=
  print_string "\n";
  (match (get_current_chat st) with
   |[]->print_string ""
   |h::t->output_convo_line h);
  ANSITerminal.(print_string [magenta] "\n>> ")


let rec transition st = 
  try (
    let menu = st |> State.get_current_menu in 
    let command = (Command.parse (State.get_menu_id menu) (get_current_user st) (read_line ())) in
    match command with
    | Sign_Up -> 
      (match change_state "create account" st with 
       | Valid t ->
         print_new_username st; transition t
       | Invalid -> failwith "should not get here")
    | New_Username str -> 
      (match change_state str st with 
       | Valid t -> print_new_password1 st; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    "\n\nSorry! That username is already taken! Try typing another!\n\n>> ");
         transition st
      )
    | New_Password str -> 
      (match change_state str st with 
       | Valid t -> ANSITerminal.(print_string [green]
                                    "Congratulations! You've successfully created an account"); print_login st;transition t 
       | Invalid -> failwith ("should not get here ")
      )
    | Login_As str -> 
      (match change_state str st with
       | Valid t -> 
         print_plaza t; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    ("\n\nThat username doesn't exist. Type " ^ "/signup " ^ "to create a new account. \n\n>>"));
         transition st)
    | Chat_With str ->
      (* display contents of previous chat, reverse list *)
      (match change_state str st with
       | Valid t -> 
         (* if ((t |> get_current_menu |> get_menu_id) = "login")
            then (
             ANSITerminal.(print_string [green]
                             "\nWelcome to the login page. What is your username?\n>> "); *)
         print_whole_chat t; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    "\n\nYou don't have a contact with that name. \n");
         transition st)
    | Send str ->
      (* display most recent chat *)
      (match change_state str st with
       | Valid t -> 
         (* if t |> get_current_menu |> get_menu_id = "chat" 
            then ((print_new_message t);transition t ) *)
         (print_new_message t); transition t
       | Invalid -> failwith "should not happen send")
    | Open_Requests ->
      (match change_state "open_requests" st with
       | Valid t -> print_connect t; transition t
       | Invalid -> failwith "should not happen open requests")
    | Move_Request (tag,str) -> 
      (match interact_with_request tag str st with
       | Valid t -> print_connect t; transition t
       | Invalid -> failwith "invalid")
    | Back ->
      (match go_back st with
       | Valid t -> 
         let next_menu_id = 
           t |> get_current_menu |> get_menu_id in 
         if next_menu_id = "login" then
           (print_login t; transition t) else
         if next_menu_id = "plaza" then
           (print_plaza t; transition t) else
         if next_menu_id = "sign_up_username" then 
           (print_new_username t; transition t) else
           exit 0
       | Invalid -> failwith "should not happen")
    | Quit -> exit 0 )
  with 
  | Empty_Login_Id -> ANSITerminal.(print_string [red] 
                                      "Whoops! You didn't enter anything. Enter a valid username to login!"); 
    ANSITerminal.(print_string [magenta] "\n\n>> ");
    transition st
  | Empty_Chat_With_Id -> ANSITerminal.(print_string [red] "Whoops! You can't chat someone with no name!!" );
    ANSITerminal.(print_string [magenta] "\n\n>> "); transition st
  | Empty_Send -> ANSITerminal.(print_string [red] 
                                  "Uh oh..you didn't type anything to send! Try something more meaningful.");
    ANSITerminal.(print_string [magenta] "\n\n>> ");
    transition st
  | Empty_New_Username -> ANSITerminal.(print_string [red] 
                                          "Your username can't be empty"); ANSITerminal.(print_string [magenta] "\n\n>> ");transition st 
  | Empty_New_Password -> ANSITerminal.(print_string [red] 
                                          "Whoops! You didn't enter anything. Enter a valid username to login!"); 
    ANSITerminal.(print_string [magenta] "\n\n>> ");transition st
  | Empty_Connect ->  ANSITerminal.(print_string [red] "You must type add, accept, or deny followed by a name."); ANSITerminal.(print_string [magenta] "\n\n>> ");transition st
  | Malformed_Login_Id -> ANSITerminal.(print_string [red]
                                          "Uh oh that username doesn't exist. "); ANSITerminal.(print_string [magenta] "\n\n>> "); transition st
  | Malformed_Chat_With -> ANSITerminal.(print_string [red]
                                           "Uh oh that person doesn't exist. Try one of your existing contacts!"); ANSITerminal.(print_string [magenta] "\n\n>>"); transition st
  | Malformed_Chat_With_Self -> ANSITerminal.(print_string [red]
                                                "Ummmm you can't chat with yourself! Silly"); ANSITerminal.(print_string [magenta] "\n\n>> "); transition st
  | Malformed_New_Username ->  ANSITerminal.(print_string [red] 
                                               "Your username can only be one word, no spaces"); ANSITerminal.(print_string [magenta] "\n\n>> ");transition st 
  | Malformed_New_Password ->  ANSITerminal.(print_string [red] 
                                               "Your password can only be one word no spaces"); ANSITerminal.(print_string [magenta] "\n\n>> ");transition st 
  | Malformed_Connect -> ANSITerminal.(print_string [red] "You must type add, accept, or deny followed by a name."); ANSITerminal.(print_string [magenta] "\n\n>> ");transition st

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let state = State.init_state in
  print_login state;
  transition state 

(* Execute the game engine. *)
let () = main ()
