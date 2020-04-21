open Yojson
open Yojson.Basic.Util
open Jmodule

type message = {
  sent_by:string;
  text:string;
}

type t = {
  convo:message list
}

let make_message mess ={
  sent_by = mess |> member "sent_by" |> to_string;
  text = mess |> member "text" |> to_string;
}

let rec make_message_list mess_list json_list=
  match json_list with
  |[]-> mess_list
  |h::t-> make_message_list ((make_message h)::mess_list) t

(* description: takes in json script and pipelines script to string attributes,
requires: json is valid json script *)
let from_json json (name1:string) (name2:string)={
  convo=json |> member (Jmodule.id_creator name1 name2) |> to_list |> make_message_list [];}

(* helper function: takes sent_by and text and displays message *)
let output_convo_line message=
  ANSITerminal.(print_string [yellow]
                  (message.sent_by^": "));
   ANSITerminal.(print_string [green]
                  (message.text^"\n"))  

(* ITERATE THROUGH SENT BY AND TEXT TO NOW
PRINT OUT EACH ELEMENT OF LISTS *)
(* get sent_by as list *)
let get_sent_bys t = 
  let sent_list = t.convo in
  List.map (fun s -> s.sent_by) sent_list

(* get text as list *)
let get_texts t =
  let text_list = t.convo in
  List.map (fun txt->txt.text) text_list

(* iterate through both and print out as you iterate *)
(* let rec print_convo (sent_list:string list) (text_list:string list)=
  match sent_list with
  |[]-> output_convo_line "" ""
  |h::t-> 
    (match text_list with
    |[]-> output_convo_line "" ""
    |x::xs-> output_convo_line h x; print_convo t xs); print_convo *)

let rec print_convo t=
  match t with
  |[]->print_endline ""
  |x::xs-> output_convo_line x; (print_convo xs)