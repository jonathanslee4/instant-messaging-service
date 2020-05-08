open Command
open State
open Readingjson
open Jmodule

(** [print_slist slist] prints each element of slist. *)
let rec print_slist slist =
  match slist with
  | [] -> print_string ""
  | hd :: tl -> (print_string hd); print_slist tl

(** [print_contacts st con_list] prints every element of con_list that is not
    the current user id. *)
let rec print_contacts st con_list =
  match con_list with
  |[] -> print_string ""
  |hd::tl-> 
    if hd = get_current_user st then print_contacts st tl
    else
      (print_endline(hd); 
       print_contacts st tl)

(** [print_convo t] outputs each text message in t, separating the user name and
    text body components by color. *)
let rec print_convo texts=
  match texts with
  |[]->print_string ""
  |x::xs-> 
    (output_convo_line x; (print_convo xs))

(** [print_login] prints the login menu opening text. *)
let print_login () =
  ANSITerminal.(print_string [green]
                  "\nWelcome to the login page. What is your username?\n>> ")

(** [print_login_password st] prints a message asking for the current user's
    password. *)
let print_login_password st  = 
  ANSITerminal.(print_string [green]
                  ("\nWelcome " ^ get_current_user st ^ "! \
                                                         What is your \
                                                         password?\n>> ") )

(** [print_new_username st] prints a message asking for a new username. *)
let print_new_username st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease enter a new one-word username. \
                   This is how others will see you on the service.\n>> ")

(** [print_new_password st] prints a message asking for a new password. *)
let print_new_password1 st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease create a one-word password.\n>> ") 

(** [print_new_password2 st] prints a message asking for a password 
    confirmation. *)
let print_new_password2 st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease enter the password again to confirm.\n>> ")

(** [print_plaza st] prints a message asking who the user 
    would like to chat with, along with the user's friends. *)
let print_plaza st =                      
  ANSITerminal.(print_string [cyan] "\nWho would you like to chat with? \
                                     Type /back to log out.\n");
  print_contacts st ((get_current_contacts st));
  ANSITerminal.(print_string [cyan] "\n>> ")

(** [print_connect st] prints a message instructing the user on how to deal with
    friend requests. *)
let print_connect st =
  let friend_request_num = 
    List.length (get_pending_friends (get_current_user st)) in
  if friend_request_num = 0 then
    ANSITerminal.(print_string [yellow]
                    "\nYou haven't received any new friend requests yet. \
                     Type add followed by a name to send a friend request! \
                     When you have received a request, you can type accept or \
                     deny followed by that person's name.
                     \n>> ")
  else if friend_request_num = 1 then
    (ANSITerminal.(print_string [yellow] 
                     "\nYou have a pending friend request from:\n"); 
     print_contacts st (get_pending_friends (get_current_user st));
     ANSITerminal.(print_string [yellow] "\n>> "))
  else 
    (ANSITerminal.(print_string [yellow] 
                     "\nYou have pending friend requests from:\n"); 
     print_contacts st (get_pending_friends (get_current_user st));
     ANSITerminal.(print_string [yellow] "\n>> "))

let print_successful_add str = 
  ANSITerminal.(print_string [yellow] ("\n"^str^" has been sent a friend \
                                                 request. We'll let you know \
                                                 as soon as she accepts!\n>> "))

let print_successful_accept str = 
  ANSITerminal.(print_string [green]
                  ("\nYou're now friends with "^str^"! If you want to start a \
                                                     new conversation with \
                                                    "^str^", type /back \
                                                           to view your \
                                                           contacts."));
  ANSITerminal.(print_string [yellow] "\n\n>> ")



(** [print_whole_chat st] prints the conversation between the user and the
    receiver. *)
let print_whole_chat st=
  print_string "\n";
  ANSITerminal.erase Screen;
  print_convo (List.rev(get_current_chat st));
  ANSITerminal.(print_string [magenta] 
                  "Type your message or /back to return to your contacts.\n>> ")

(** [print_new_message st] prints the most recent text messa *)
let print_new_message st=
  print_string "\n";
  (match (get_current_chat st) with
   |[]->print_string ""
   |h::t->output_convo_line h);
  ANSITerminal.(print_string [magenta] "\n>> ")

(** [print_exception_message msg] prints a formatted exception message that 
    says [msg]. *)
let print_exception_message msg = 
  ANSITerminal.(print_string [red] ("\n"^msg)); 
  ANSITerminal.(print_string [red] "\n>> ")

let rec transition st = 
  try (
    let menu = st |> State.get_current_menu in 
    let command = (Command.parse (State.get_menu_id menu)
                     (get_current_user st) (read_line ())) in
    match command with
    | Sign_Up -> 
      (match change_state "create account" st with 
       | Valid t ->
         ANSITerminal.erase Screen;
         print_new_username st; transition t
       | Invalid -> failwith "should not get here")
    | New_Username str -> 
      (match change_state str st with 
       | Valid t -> print_new_password1 st; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    "\n\nSorry! That username is already \
                                     taken! Try typing another!\n\n>> ");
         transition st
      )
    | New_Password str -> 
      (match change_state str st with 
       | Valid t -> ANSITerminal.(print_string [green]
                                    "You've successfully created \
                                     your new account! Welcome!\n"); 
         ANSITerminal.erase Screen;
         print_login ();transition t 
       | Invalid -> failwith ("should not get here ")
      )
    | Login_As str -> 
      (match change_state str st with
       | Valid t -> 
         print_login_password t; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    ("\nThat username doesn't exist. \
                                      Type " ^ "/signup " ^ "to \
                                                             create a new \
                                                             account. \n>> "));
         transition st)
    | Login_Password str ->
      (match change_state str st with
       | Valid t -> 
         ANSITerminal.erase Screen;
         ANSITerminal.(print_string [green]
                         "\nYou are now logged in!\n");print_plaza t; 
         transition t
       | Invalid ->  ANSITerminal.(print_string [red]
                                     ("\nIncorrect password. \
                                       \n>> ")); transition st)
    | Chat_With str ->
      (* display contents of previous chat, reverse list *)
      (match change_state str st with
       | Valid t -> 
         print_whole_chat t; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    "\nYou don't have a contact with \
                                     that name. Type /connect to add \
                                     new contacts, or chat with a \
                                     friend!\n>> ");
         transition st)
    | Send str ->
      (* display most recent chat *)
      (match change_state str st with
       | Valid t -> 
         (print_new_message t); transition t
       | Invalid -> failwith "should not happen send")
    | Open_Requests ->
      (match change_state "open_requests" st with 
       | Valid t -> print_connect t; transition t
       | Invalid -> failwith "should not happen open requests") 
    | Move_Request (tag,str) -> 
      (match interact_with_request tag str st with
       | PValid t -> 
         if tag = "add" then print_successful_add str
         else if tag = "accept" then print_successful_accept str;
         transition t
       | Invalid_Unrecognizable -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou must type add, accept, or \
                          deny followed by a name.\n>> ");
         transition st
       | Invalid_Add_Already_Friended -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou are already friends with this user. \n>> ");
         transition st
       | Invalid_Add_Already_Added -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou have already added this user. \n>> ");
         transition st
       | Invalid_Add_Pending-> 
         ANSITerminal.(print_string [magenta]
                         "\nYou have already received a friend request from \
                          this user! Type accept <name> to accept the request.\
                          \n>> ");
         transition st
       | Invalid_Existless -> 
         ANSITerminal.(print_string [magenta]
                         "\nThis user doesn't exist. \n>> ");
         transition st)
    | Back ->
      (match go_back st with
       | Valid t -> 
         let next_menu_id = 
           t |> get_current_menu |> get_menu_id in
         ANSITerminal.erase Screen;
         (if next_menu_id = "login" then
            (print_login ()) else
          if next_menu_id = "plaza" then
            (print_plaza t) else
          if next_menu_id = "sign_up_username" then 
            (print_new_username t) else
            exit 0);
         transition t
       | Invalid -> failwith "should not happen")
    | Quit -> exit 0 )
  with 
  | Empty_Login_Id -> 
    print_exception_message "Whoops! You didn't enter anything. \
                             Enter a valid username to login!"; 
    transition st
  | Empty_Login_Password -> 
    print_exception_message  "You entered a blank password! Try again";
    transition st 
  | Empty_Chat_With_Id -> 
    print_exception_message "Whoops! You can't chat someone with no name!!";
    transition st 
  | Empty_Send -> 
    print_exception_message "Uh oh..you didn't type anything to send! \
                             Try something more meaningful.";
    transition st
  | Empty_New_Username -> 
    print_exception_message "Your username can't be empty";
    transition st 
  | Empty_New_Password ->
    print_exception_message "Whoops! You didn't enter anything. \
                             Enter a valid username to login!";
    transition st
  | Empty_Connect ->  
    print_exception_message  "You must type add, accept, or deny followed by a \
                              name.";
    transition st
  | Malformed_Login_Id -> 
    print_exception_message "That username doesn't exist.";
    transition st
  | Malformed_Login_Password -> 
    print_exception_message "Passwords should only have one space, so that \
                             can't be it.";
    transition st
  | Malformed_Chat_With ->
    print_exception_message "Uh oh that person doesn't exist. Try one of your \
                             existing contacts!";
    transition st
  | Malformed_Chat_With_Self -> 
    print_exception_message "Ummmm you can't chat with yourself! Silly";
    transition st
  | Malformed_New_Username ->  
    print_exception_message "Your username can only be one word, no spaces";
    transition st 
  | Malformed_New_Password ->  
    print_exception_message "Your password can only be one word no spaces";
    transition st 
  | Malformed_Connect -> 
    print_exception_message "You must type add, accept, or deny followed by \
                             a name.";
    transition st

(** [main ()] prompts for the instant messaging interface to play, then starts it. *)
let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1;
  let state = init_state in
  print_login ();
  transition state 

(* Execute the game engine. *)
let () = main ()
