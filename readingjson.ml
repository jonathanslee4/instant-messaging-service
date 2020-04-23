open Yojson
open Yojson.Basic.Util
open Jmodule

type message = {
  sent_by:string;
  text:string;
}

type convo = message list

type contact = string list


let message_from_convo_json j ={
  sent_by = j |> member "sent_by" |> to_string;
  text = j |> member "text" |> to_string;
}

let rec make_message_list mess_list json_list=
  match json_list with
  |[]-> mess_list
  |h::t-> make_message_list ((message_from_convo_json h)::mess_list) t

(* (* description: takes in json script and pipelines script to string attributes,
   requires: json is valid json script *)
   let from_json json (name1:string) (name2:string)=
   json |> member (Jmodule.id_creator name1 name2) |> to_list |> make_message_list [] *)

let convo_from_json json = 
  json |> member "text history" |> to_list |> List.map message_from_convo_json

let contacts_from_json json = 
  json |> member "contact list" |> to_list |> List.map to_string 

(* helper function: takes sent_by and text and displays message *)
let output_convo_line message=
  ANSITerminal.(print_string [yellow]
                  (message.sent_by^": "));
  ANSITerminal.(print_string [green]
                  (message.text^"\n"))  

(* (* ITERATE THROUGH SENT BY AND TEXT TO NOW
   PRINT OUT EACH ELEMENT OF LISTS *)
   (* get sent_by as list *)
   let get_sent_bys t = 
   let sent_list = t.convo in
   List.map (fun s -> s.sent_by) sent_list

   (* get text as list *)
   let get_texts t =
   let text_list = t.convo in
   List.map (fun txt->txt.text) text_list *)

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