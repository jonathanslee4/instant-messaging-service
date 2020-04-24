open Command
open State
open Readingjson
open Jmodule

let rec print_contacts con_list =
  match con_list with
  |[] -> print_endline ""
  |h::t-> print_endline("\n"^h"\n"); print_contacts t

let print_menu_description st = 
  match (get_current_menu st) with
  | Login -> ANSITerminal.(print_string [magenta]
                            "This is the login page.");
  | Plaza -> ANSITerminal.(print_string [magenta]
                            "This is the Plaza."); print_contacts (get_current_contacts st);
  | Chat ->ANSITerminal.(print_string [magenta]
                            "This is the chat page."); 

    let rec transition st = 
      let menu = st |> State.get_current_menu in 
      let command = (Command.parse (State.get_menu_id menu) (read_line ())) in
      match command with
      | Username str -> 
        (match next_menu str st with
         | Valid t -> transition t
         | Invalid -> ANSITerminal.(print_string [magenta]
                                      "\n\n That username doesn't exist. \n");
           transition st)
      | Engage str ->
        (match next_menu str st with
         | Valid t -> transition t
         | Invalid -> ANSITerminal.(print_string [magenta]
                                      "\n\n You don't have a contact with that name. \n");
           transition st)
      | Send str ->
        (match next_menu str st with
         | Valid t -> 

           transition t
         | Invalid -> failwith "should not happen")
      | Quit -> exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let state = State.init_state in
  transition state 