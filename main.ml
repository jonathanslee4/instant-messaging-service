open Command
open State
open Readingjson
open Jmodule


let rec print_contacts st con_list =
  match con_list with
  |[] -> print_string ""
  |hd::tl-> 
    (* if hd = get_current_user st then (print_endline("skipped");
                                      print_contacts st tl)
       else
       print_endline(hd); print_contacts st tl *)
    print_endline(hd); print_contacts st tl 

let rec print_convo t=
  match t with
  |[]->print_endline ""
  |x::xs-> 
    output_convo_line x; (print_convo xs)

let print_login =
  ANSITerminal.(print_string [green]
                  "\nWelcome to the login page. What is your username?\n>> ")

let print_plaza st =
  ANSITerminal.(print_string [magenta] "\nWho would you like to chat with?\n");
  print_contacts st ((get_current_contacts st));
  ANSITerminal.(print_string [magenta] "\n>> ")

let print_whole_chat st=
  print_convo (List.rev(get_current_chat st))

let print_new_message st=
  match (get_current_chat st) with
  |[]->print_string ""
  |h::t->output_convo_line h

let rec transition st = 

  let menu = st |> State.get_current_menu in 
  let command = (Command.parse (State.get_menu_id menu) (read_line ())) in
  match command with
  | Username str -> 
    (match next_menu str st with
     | Valid t -> print_plaza t; transition t
     | Invalid -> ANSITerminal.(print_string [magenta]
                                  "\n\n That username doesn't exist. \n");
       transition st)
  | Engage str ->
    (* display contents of previous chat, reverse list *)
    (match next_menu str st with
     | Valid t -> if (t |> get_current_menu |> get_menu_id) = "login"
       then (print_login; transition t) 
       else (print_whole_chat t; transition t)
     | Invalid -> ANSITerminal.(print_string [magenta]
                                  "\n\n You don't have a contact with that name. \n");
       transition st)
  | Send str ->
    (* display most recent chat *)
    (match next_menu str st with
     | Valid t -> if t |> get_current_menu |> get_menu_id = "chat" 
       then ((print_new_message t);transition t )
       else (print_plaza t); transition t
     | Invalid -> failwith "should not happen")
  | Quit -> exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let state = State.init_state in
  print_login;
  transition state 

(* Execute the game engine. *)
let () = main ()
