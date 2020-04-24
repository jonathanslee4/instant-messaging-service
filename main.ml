open Command
open State
open Readingjson
open Jmodule

let rec print_contacts con_list  =
  match con_list with
  |[] -> print_endline ""
  |h::t-> print_endline("\n" ^h^ "\n"); print_contacts t

let rec print_convo t=
  match t with
  |[]->print_endline ""
  |x::xs-> output_convo_line x; (print_convo xs)

let print_login =
  ANSITerminal.(print_string [magenta]
                  "This is the login page.")

let print_plaza st =
  ANSITerminal.(print_string [magenta]
                  "This is the Plaza."); print_contacts (get_current_contacts st)

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
     | Valid t -> ANSITerminal.(print_string [magenta]
                                  "This is the plaza.");transition t
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
       then ((print_new_message st);transition t )
       else (print_plaza t); transition t
     | Invalid -> failwith "should not happen")
  | Quit -> exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let state = State.init_state in
  print_login;
  transition state 