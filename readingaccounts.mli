type account = {
  username:string;
  password:string;
}
(** TODO: question is type accounts even necessary *)
type accounts = account list

val contains :'a list -> 'a -> bool

val accounts_from_json : Yojson.Basic.t -> account list

val usernames_from_accounts : account list -> string list

val user_exists : string -> bool

(** [is_verified_password actlist pwd usr] is true there is an account
    registered in [actlist] with a username that matches [usr] and a password,
    that matches [pwd]; is false if there is not.  *)
val is_verified_password : string -> string -> account list -> bool

