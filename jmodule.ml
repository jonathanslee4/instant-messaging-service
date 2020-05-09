open Yojson
open Yojson.Basic.Util
open Printf
open Str

(** [contains list item] is a boolean expression denoting whether [list] contains [item]. *)
let rec contains list item =
  match list with
  |[]-> false
  |h::t-> if h=item then true else contains t item

let id_creator (name1:string) (name2:string)=
  if name1<name2 then name1^"&"^name2
  else name2^"&"^name1

let json_creator (id:string)=
  id^".json"

(** [directory_exists json_name] is a boolean expression denoting whether 
    file of name [json_name] is in the working directory.*)
let directory_exists (json_name:string)=
  Sys.file_exists json_name

(** [entire_file file_name] is a string of the contents of [file_name]. *)
let entire_file (file_name:string)=
  let ch  = open_in file_name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(** [save file next] is a unit that overwrites [file] with [text]. *)
let save (file:string) (text:string) =
  let channel = open_out file in
  output_string channel text;
  close_out channel

(** [existing_convo sent_by text id] is a unit that saves data to [id] if it
    already exists in the current working directory.*)
let existing_convo (sent_by:string) (text:string) (id:string)=
  let file_contents = entire_file (json_creator id) in
  let bracket_char= String.index file_contents '[' in
  let first = Str.string_before file_contents (bracket_char+1) in
  let third = Str.string_after file_contents (bracket_char+1) in
  let second = "{\"sent_by\":\""^sent_by^"\",\"text\":\""^text^"\"}," in
  save (json_creator id) (first^second^third)

(** [new_convo sent_by text id] is a unit that saves data to [id] if it
    does not exist in the current working directory. *)
let new_convo (sent_by:string) (text:string) (id:string)=
  let string_to_print = 
    "{\"text history\": [{\"sent_by\":\""^sent_by^"\", \"text\":\""^text^"\"}]}"
  in
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

let afp_empty (new_contact:string)=
  let contacts_contents = entire_file "afp.json" in
  let last_square = String.rindex contacts_contents ']' in
  let first = Str.string_before contacts_contents (last_square) in
  let third = Str.string_after contacts_contents (last_square) in
  save ("afp.json") (first^"\""^new_contact^"\""^third)

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

(** [replace input output] is a string that sees [input] replace all occurences of [output]
    in a given string. *)
let replace input output =
  Str.global_replace (Str.regexp_string input) output

let pfp_remove (to_remove:string)=
  let pfp_contents = entire_file "pfp.json" in
  let removed1 = replace to_remove "" pfp_contents in
  let removed2 = replace "\"\"," "" removed1 in
  save ("pfp.json") removed2

(* let pfp_contents = entire_file "pfp.json" in
   let removed1 = replace to_remove "" pfp_contents in 
   if String.contains pfp_contents ','
   then save ("pfp.json") (replace "\"\"," "" removed1)
   else save ("pfp.jsop") (replace "\"\"" "" removed1) *)

(* THE ACCOUNT JSON EDITING IS BELOW *)

let account_json_add username password=
  let file_contents = entire_file ("logindetails.json") in
  let bracket_char= String.index file_contents '[' in
  let first = Str.string_before file_contents (bracket_char+1) in
  let third = Str.string_after file_contents (bracket_char+1) in
  let second = "{\"username\":\""^username^"\",\"password\":\""^
               password^"\"}," in
  save ("logindetails.json") (first^second^third)


