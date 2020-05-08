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

(** [message_from_convo_json j] is a unit that parses data from [j] to type 
message. *) 
let message_from_convo_json j ={
  sent_by = j |> member "sent_by" |> to_string;
  text = j |> member "text" |> to_string;
}

(** [make_message_list mess_list json_list] is a list of messages.  *) 
let rec make_message_list mess_list json_list=
  match json_list with
  |[]-> mess_list
  |h::t-> make_message_list ((message_from_convo_json h)::mess_list) t


(** [from_json json] is a list of messages between [name1] and [name2]. *)
   let from_json json (name1:string) (name2:string)=
   json |> member (Jmodule.id_creator name1 name2) |> to_list |> 
   make_message_list [] 

let convo_from_json yojsont = 
  yojsont |> member "text history" |> to_list |> 
  List.map message_from_convo_json

(** [contacts_from_json yojsont] is a list of contacts from [yojsont]. *) 
let contacts_from_json yojsont = 
  yojsont |> member "contact list" |> to_list |> List.map to_string 

(** [accepted_friend_pairs_from_json yojsont] is a list of accepted friend pairs
from [yojsont]. *) 
let accepted_friend_pairs_from_json yojsont = 
  yojsont |> member "accepted friend pairs" |> to_list |> List.map to_string
 
let pending_friend_pairs_from_json yojsont = 
  yojsont |> member "pending friend pairs" |> to_list |> List.map to_string

(** @JONNY CHECK ALL THE THINGS BELOW:
[expand slist] is a list of strings that have been separated by "&".*) 
let rec expand slist = 
  match slist with
  | [] -> []
  | hd::tl -> String.split_on_char '&' hd :: expand tl

(** [list_to_pair list] is a tuple containing two elements, requires: [list]
contains only two elements. *) 
let rec list_to_pair list =
  match list with 
  |[] -> failwith "list must have only two elements"
  | hd :: hd2 :: tl -> (hd, hd2)
  | _ -> failwith "list must have only two elements"

(** [list_to triple list] is a tuple containing three elements, requires: [list]
contains only three elements. *) 
let rec list_to_triple list =
  match list with 
  | [] -> failwith "list must have only three elements"
  | hd :: hd2 :: hd3 :: tl -> (hd,hd2,hd3)
  | _ -> failwith "list must have only three elements"

(** [filter_pair s pairlist] is a list of tuples containing two elements,
without tuples containing repeating elements.  *) 
let rec filter_pair s pairlist = 
  match pairlist with
  | [] -> []
  | (a,b) :: tl -> if a = s then b::filter_pair s tl else if b = s 
    then a::filter_pair s tl else filter_pair s tl

(** [filter_triple s pairlist] is a list of tuples containing three elements,
without tuples containing repeating elements.  *) 
let rec filter_triple s pairlist = 
  match pairlist with
  | [] -> []
  | (a,b,c) :: tl -> 
    if c = s then filter_triple s tl else
    if a = s then b::filter_triple s tl else
    if b = s then a::filter_triple s tl else
      filter_triple s tl

let get_accepted_friends username =
  let slist = 
    accepted_friend_pairs_from_json (Yojson.Basic.from_file "afp.json") in
  slist |> expand |> List.map list_to_pair |> filter_pair username

let get_pending_friends username =
  let slist = 
    pending_friend_pairs_from_json (Yojson.Basic.from_file "pfp.json") in
  (* if slist = [] then [] else *)
  slist |> expand |> List.map list_to_triple |> filter_triple username
