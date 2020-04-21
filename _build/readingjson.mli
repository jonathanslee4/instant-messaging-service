type t

type message = {
  sent_by:string;
  text:string;
}

val from_json : Yojson.Basic.t -> string -> string -> t

val output_convo_line : message -> unit

val get_sent_bys : t -> string list

val get_texts : t-> string list

val print_convo : message list->unit