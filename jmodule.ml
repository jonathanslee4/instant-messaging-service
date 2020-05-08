open Yojson
open Yojson.Basic.Util
open Printf
open Str

(* [contains]: returns true if list contains given item, false otherwise. *)
let rec contains list item =
  match list with
  |[]-> false
  |h::t-> if h=item then true else contains t item

let id_creator (name1:string) (name2:string)=
  if name1<name2 then name1^"&"^name2
  else name2^"&"^name1

let json_creator (id:string)=
  id^".json"

(* [directory_exists]: determines whether json file name exists in current directory,
   returns: true if in directory, false otherwise *)
let directory_exists (json_name:string)=
  Sys.file_exists json_name

(* [entire_file]: returns entire contents of given file as string. *)
let entire_file (file_name:string)=
  let ch  = open_in file_name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* [save]: save to text to file in directory, file contents is overwritten. *)
let save (file:string) (text:string) =
  let channel = open_out file in
  output_string channel text;
  close_out channel

(* [existing_convo]: when conversation between two users already exists, it takes information from the sender
   and recipient and writes the sender's message in the appropriate format to the correct json file (dependent on
   the two users communicating). *)
let existing_convo (sent_by:string) (text:string) (id:string)=
  let file_contents = entire_file (json_creator id) in
  let bracket_char= String.index file_contents '[' in
  let first = Str.string_before file_contents (bracket_char+1) in
  let third = Str.string_after file_contents (bracket_char+1) in
  let second = "{\"sent_by\":\""^sent_by^"\",\"text\":\""^text^"\"}," in
  save (json_creator id) (first^second^third)

(* [new_convo]: when conversation between two users does not currently exist, takes information from the sender
   and recipient and writes the sender's message in the appropriate format to the a newly created json file, whose name is
   dependent on the two users communicating. *)
let new_convo (sent_by:string) (text:string) (id:string)=
  let string_to_print = "{\"text history\": [{\"sent_by\":\""^sent_by^"\", \"text\":\""^text^"\"}]}" in
  save (json_creator id) (string_to_print)


let editingtext_json (sent_by:string) (text:string) (id:string)=
  if directory_exists (json_creator id)
  then existing_convo sent_by text id
  else new_convo sent_by text id


let afp_add (new_contact:string) =
  let contacts_contents = entire_file "afp.json" in
  let last_square = String.rindex contacts_contents ']' in
  let first = Str.string_before contacts_contents (last_square) in
  let third = Str.string_after contacts_contents (last_square) in
  let second = ",\""^new_contact^"\"" in
  save ("afp.json") (first^second^third);;


let pfp_add (new_contact:string) =
  let contacts_contents = entire_file "pfp.json" in
  let last_square = String.rindex contacts_contents ']' in
  let first = Str.string_before contacts_contents (last_square) in
  let third = Str.string_after contacts_contents (last_square) in
  save ("pfp.json") (first^",\""^new_contact^"\""^third)

let pfp_empty (new_contact:string)=
  let contacts_contents = entire_file "pfp.json" in
  let last_square = String.rindex contacts_contents ']' in
  let first = Str.string_before contacts_contents (last_square) in
  let third = Str.string_after contacts_contents (last_square) in
  save ("pfp.json") (first^"\""^new_contact^"\""^third)

(* [replace]: replaces all instances of input in output string. *)
let replace input output =
  Str.global_replace (Str.regexp_string input) output

let pfp_remove (to_remove:string)=
  let pfp_contents = entire_file "pfp.json" in
  let removed1 = replace to_remove "" pfp_contents in
  let removed2 = replace "\"\"," "" removed1 in
  save ("pfp.json") removed2


(* THE ACCOUNT JSON EDITING IS BELOW *)

let account_json_add username password=
  let file_contents = entire_file ("logindetails.json") in
  let bracket_char= String.index file_contents '[' in
  let first = Str.string_before file_contents (bracket_char+1) in
  let third = Str.string_after file_contents (bracket_char+1) in
  let second = "{\"username\":\""^username^"\",\"password\":\""^password^"\"}," in
  save ("logindetails.json") (first^second^third)


(* TO BE USED IN STATE *)
(* user account verification -> check if u_exists=false && 
                                      let acc_veri t (username:string) pass1 pass2=
                                        if ((pass1=pass2) && not (user_exists username t))
                                        then (account_json_add username pass1)
                                        else failwith "ask to enter passwords again" *)
