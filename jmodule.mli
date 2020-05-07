(* NEW IDEA *)
val contains : 'a list -> 'a -> bool

val id_creator : string -> string -> string

val json_creator : string -> string

val directory_exists : string -> bool

val entire_file : string -> string

val penultimate_index : string -> char -> int

val existing_convo : string -> string -> string -> unit

val new_convo : string -> string -> string -> unit

val editingtext_json : string -> string -> string -> unit

val afp_add : string -> unit

val pfp_add : string -> unit

val pfp_empty : string -> unit

val account_json_add : string -> string -> unit




(* json reading/writing stuff *)
(* val ty_of_yojson : Yojson.Safe.json -> (ty, string) Result.result
   val ty_of_yojson_exn : Yojson.Safe.json -> ty
   val ty_to_yojson : ty -> Yojson.Safe.json *)


