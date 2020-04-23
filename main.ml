open Command
open State
open Readingjson
open Jmodule

(* let print_login_window =  *)

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






let run_program input = 
  if input <> "y" then exit 0 else 
    let state = State.init_state in
    transition state 


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [magenta]
                  "\n\nWelcome to our instant messaging system.\n");
  print_endline "Would you like to connect with new people? Type 
  'y' to continue! \n";
  print_string  "> ";
  match read_line () with
  | "" -> exit 0
  | input -> run_program input