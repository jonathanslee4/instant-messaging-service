type message 
type convo = message list
type contacts = string list


val convo_from_json : Yojson.Basic.t -> convo
val contacts_from_json : Yojson.Basic.t -> contacts
(* 
val from_json : Yojson.Basic.t -> string -> string -> t *)

val output_convo_line : message -> unit

val get_sent_by : message -> string

val get_texts : message -> string

(* val print_convo : message list->unit*)