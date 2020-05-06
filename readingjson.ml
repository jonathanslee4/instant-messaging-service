open Yojson
open Yojson.Basic.Util
open Jmodule

type message = {
  sent_by:string;
  text:string;
}

type convo = message list

type contact = string list

let get_sent_by msg = msg.sent_by

let get_text msg = msg.text

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

let convo_from_json yojsont = 
  yojsont |> member "text history" |> to_list |> List.map message_from_convo_json

let contacts_from_json yojsont = 
  yojsont |> member "contact list" |> to_list |> List.map to_string 

let accepted_friend_pairs_from_json yojsont = 
  yojsont |> member "accepted_friend_pairs" |> to_list |> List.map to_string

let pending_friend_pairs_from_json yojsont = 
  yojsont |> member "pending_friend_pairs" |> to_list |> List.map to_string

(* helper function: takes sent_by and text and displays message*)
let output_convo_line message=
  ANSITerminal.(print_string [yellow]
                  (message.sent_by^": "));
  ANSITerminal.(print_string [green]
                  (message.text^"\n"))   