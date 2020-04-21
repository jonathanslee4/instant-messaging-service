open Yojson
open Yojson.Basic.Util
open Printf

(* helper function: returns true if list contains item, false otherwise *)
let rec contains list item =
  match list with
  |[]-> false
  |h::t-> if h=item then true else contains t item

(*takes two usernames, determines which one comes first alphabetically,
  concatenates them and returns userID*)
let id_creator (name1:string) (name2:string)=
  if name1<name2 then String.concat "" [name1;name2]
  else  String.concat "" [name2;name1]

(* creates json file name from id *)
let json_creator (id:string)=
  id^".json"

(* determines whether json file name exists in current director,
returns: true if in directory, false otherwise *)
let directory_exists (json_name:string)=
  Sys.file_exists json_name

(* helper function: returns entire contents of given file as string *)
let entire_file (file_name:string)=
  let ch  = open_in file_name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* helper function: returns index of penultimate } *)
let penultimate_index (str:string) =
  (* find index  of first and last },
  find index of last } of string[first:last-1]*)
  let first_last = String.rindex str '}' in
  let first_chop = Str.string_before str first_last in
  String.rindex first_chop '}'

(* helper function: save to file *)
let save (file:string) (text:string) =
  let channel = open_out file in
  output_string channel text;
  close_out channel

(* helper function: to create new file in workspace *)
let create (filename:string) =
  let oc = open_out filename in
  close_out oc

(* what happens when file already exists *)
let existing_convo (sent_by:string) (text:string) (id:string)=
  let file_contents = entire_file (json_creator id) in
  let penult = penultimate_index file_contents in
  let first = Str.string_before file_contents (penult+1) in
  let second = ",{\"sent_by\":\""^sent_by^"\",\"text\":\""^text^"\"}" in
  let third = "]}" in
  save (json_creator id) (first^second^third)

(* what happens when a new file needs to be made *)
let new_convo (sent_by:string) (text:string) (id:string)=
  let string_to_print = "{\""^id^"\": [{\"sent_by\":\""^sent_by^"\", \"text\":\""^text^"\"}]}" in
  save (json_creator id) (string_to_print)

let editing_json (sent_by:string) (text:string) (id:string)=
  if directory_exists (json_creator id)
  then existing_convo sent_by text id
  else new_convo sent_by text id


