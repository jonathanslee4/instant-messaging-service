open Command
open State
open Readingjson
open Jmodule

let print_login_window = 

  let rec transition st = 
    let menu = st |> State.get_current_menu in 
    let command = (parse menu (read_line ()) ) in
    match command with 
    | Engage username ->
      match (State.next_menu username st) with 
      | Valid t -> 
      | Invalid ->
      | Send msg ->
        match (State.next_menu msg st) with
        | Valid t ->
        | Invalid t ->
        | Username username -> 
          State.next_menu username st
        | Back
            exit 0



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