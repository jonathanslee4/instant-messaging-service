type account = {
  username:string;
  password:string;
}

type accounts = account list

val contains :'a list -> 'a -> bool

val accounts_from_json : Yojson.Basic.t -> account list

val usernames_from_accounts : account list -> string list

val user_exists : string -> bool

