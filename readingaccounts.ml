(* open Yojson
   open Yojson.Basic.Util
   open Jmodule

   type account = {
   username:string;
   password:string;
   }

   type accounts = account list

   (* [accounts_from_login_json j] is a unit that takes [j] and parses data to
   type account. *)
   let account_from_login_json j ={
   username = j |> member "username" |> to_string;
   password = j |> member "password" |> to_string;
   }

   (* [accounts_from_json j] is a unit that parses [j] and returns an account
   list. *)
   let accounts_from_json j = 
   j |> member "users" |> to_list |> List.map account_from_login_json

   (* [usernames_from_accounts acclist] is a list of usernames from [acclist]. *)
   let rec usernames_from_accounts acclist = 
   match acclist with
   | [] -> []
   | {username = usr; password = pwd}:: tl -> usr :: usernames_from_accounts tl

   (* [user_exists usr] is a boolean that denotes if [usr] exists in 
   "logindetails.json". *)
   let user_exists usr =
   List.mem usr ("logindetails.json" |> Yojson.Basic.from_file |> accounts_from_json |> usernames_from_accounts)

   let rec is_verified_password usr pwd actlist = 
   match actlist with
   | [] -> failwith "Username not found in accounts database."
   | {username = username; password = password}:: tl -> 
    if username = usr then password = pwd else is_verified_password usr pwd tl

*)
