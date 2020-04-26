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

let print_login =
  ANSITerminal.(print_string [green]
                  "\nWelcome to the login page. What is your username?\n>> ")

let print_plaza st =
  ANSITerminal.(print_string [magenta] "\nWho would you like to chat with? Type /back to log out.\n");
  print_contacts st ((get_current_contacts st));
  ANSITerminal.(print_string [magenta] "\n>> ")

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
    | Username str -> 
      (match next_menu str st with
       | Valid t -> 
         print_plaza t; transition t
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    "\n\nThat username doesn't exist. \n\n>> ");
         transition st)
    | Engage str ->
      (* display contents of previous chat, reverse list *)
      (match next_menu str st with
       | Valid t -> if ((t |> get_current_menu |> get_menu_id) = "login")
         then (
           ANSITerminal.(print_string [green]
                           "\nWelcome to the login page. What is your username?\n>> ");
           transition t) 
         else (print_whole_chat t; transition t)
       | Invalid -> ANSITerminal.(print_string [magenta]
                                    "\n\nYou don't have a contact with that name. \n");
         transition st)
    | Send str ->
      (* display most recent chat *)
      (match next_menu str st with
       | Valid t -> if t |> get_current_menu |> get_menu_id = "chat" 
         then ((print_new_message t);transition t )
         else (print_plaza t); transition t
       | Invalid -> failwith "should not happen")
    | Quit -> exit 0 )
  with 
  | Empty_Username -> ANSITerminal.(print_string [red] 
                                      "Whoops! You didn't enter anything. Enter a valid username to login!"); 
    ANSITerminal.(print_string [magenta] "\n\n>> ");
    transition st
  | Empty_Engage -> ANSITerminal.(print_string [red] "Whoops! You can't chat someone with no name!!" );
    ANSITerminal.(print_string [magenta] "\n\n>> "); transition st
  | Empty_Send -> ANSITerminal.(print_string [red] 
                                  "Uh oh..you didn't type anything to send! Try something more meaningful.");
    ANSITerminal.(print_string [magenta] "\n\n>> ");
    transition st
  | Malformed_Username -> ANSITerminal.(print_string [red]
                                          "Uh oh that username doesn't exist. "); ANSITerminal.(print_string [magenta] "\n\n>> "); transition st
  | Malformed_Engage -> ANSITerminal.(print_string [red]
                                        "Uh oh that person doesn't exist. Try one of your existing contacts!"); ANSITerminal.(print_string [magenta] "\n\n>>"); transition st
  | Malformed_Engage_Identity -> ANSITerminal.(print_string [red]
                                                 "Ummmm you can't chat with yourself! Silly"); ANSITerminal.(print_string [magenta] "\n\n>> "); transition st


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let state = State.init_state in
  print_login;
  transition state 

(* Execute the game engine. *)
let () = main ()
