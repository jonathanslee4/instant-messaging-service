open Yojson
open Yojson.Basic.Util
open Jmodule

type account = {
  username:string;
  password:string;
}

type accounts = account list

(** [accounts_from_login_json]: takes json file and parses data to
    type account. *)
let account_from_login_json j ={
  username = j |> member "username" |> to_string;
  password = j |> member "password" |> to_string;
}

(** [accounts_from_json]: parses json and returns an account list. *)
let accounts_from_json j = 
  j |> member "users" |> to_list |> List.map account_from_login_json

(** [usernames_from_accounts]: gets the list usernames from account list. *)
let rec usernames_from_accounts acclist = 
  match acclist with
  | [] -> []
  | {username = usr; password = pwd}:: tl -> usr :: usernames_from_accounts tl

(** [user_exists]: uses list mapping to determine whether a given username is stored in
    json file containing users' login details. *)
let user_exists usr =
  List.mem usr ("logindetails.json" |> Yojson.Basic.from_file |> accounts_from_json |> usernames_from_accounts)

(** [is_verified_password]: given a username, determines whether password entered matches
    username and password pair in "logindetails.json". If username given doesn't exist in file,
    error message is displayed. *)
let rec is_verified_password usr pwd actlist = 
  match actlist with
  | [] -> failwith "Username not found in accounts database."
  | {username = username; password = password}:: tl -> 
    if username = usr then password = pwd else is_verified_password usr pwd tl


