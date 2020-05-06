open Yojson
open Yojson.Basic.Util
open Jmodule

type account = {
  username:string;
  password:string;
}

type accounts = account list

let rec contains list item =
  match list with
  |[]-> false
  |h::t-> if h=item then true else contains t item

let account_from_login_json j ={
  username = j |> member "username" |> to_string;
  password = j |> member "password" |> to_string;
}

(** Parses json and returns an account list. *)
let accounts_from_json j = 
  j |> member "users" |> to_list |> List.map account_from_login_json

(* let rec make_accounts acc_list json_list=
   match json_list with
   |[]-> acc_list
   |h::t-> make_accounts ((account_from_login_json h)::acc_list) t *)

(** gets the list usernames from account list *)
let rec usernames_from_accounts acclist = 
  match acclist with
  | [] -> []
  | {username = usr; password = pwd}:: tl -> usr :: usernames_from_accounts tl

(* let rec make_usernames t=
   let u_list = t.accounts in
   List.map (fun a -> a.username) u_list |> List.sort_uniq String.compare *)

let user_exists usr =
  List.mem usr ("logindetails.json" |> Yojson.Basic.from_file |> accounts_from_json |> usernames_from_accounts)

(* user password verification *)
let password_veri pass1 pass2=
  if pass1=pass2 then true
  else false

let rec password_again pass1 pass2=
  if (password_veri pass1 pass2)
  then ()
  else password_again pass1 pass2

(* adding password and username to json *)
let account_json_add username password=
  let file_contents = entire_file ("logindetails.json") in
  let bracket_char= String.index file_contents '[' in
  let first = Str.string_before file_contents (bracket_char+1) in
  let third = Str.string_after file_contents (bracket_char+1) in
  let second = "{\"username\":\""^username^"\",\"password\":\""^password^"\"}," in
  save ("logindetails.json") (first^second^third)

(* user account verification -> check if u_exists=false &&  *)
let acc_veri t (username:string) pass1 pass2=
  if ((password_veri pass1 pass2) && not (u_exists username t))
  then (account_json_add username pass1)
  else failwith "ask to enter passwords again"

(* if passwords are the same, call write to signup.json *)