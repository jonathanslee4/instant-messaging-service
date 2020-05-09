(* type account = {
   username:string;
   password:string;
   }
   (** TODO: question if accounts is necessary *)
   type accounts = account list

   val accounts_from_json : Yojson.Basic.t -> account list

   val user_exists : string -> bool

   (* [is_verified_password usr pwd actlist]is a boolean that denotes if [usr] and
   [pwd] match pair in [actlist] and if not, displays error message.*)
   val is_verified_password : string -> string -> account list -> bool
*)
