type message 
type convo = message list


val convo_from_json : Yojson.Basic.t -> convo
val contacts_from_json : Yojson.Basic.t -> string list
val accepted_friend_pairs_from_json : Yojson.Basic.t -> string list
val pending_friend_pairs_from_json : Yojson.Basic.t -> string list

(* 
val from_json : Yojson.Basic.t -> string -> string -> t *)

val output_convo_line : message -> unit

(* val get_sent_by : message -> string *)

(* val get_texts : message -> string *)

(* val print_convo : message list->unit*)