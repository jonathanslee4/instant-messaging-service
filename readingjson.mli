type message 
type convo = message list
val get_sent_by : message -> string
val get_text : message -> string
val convo_from_json : Yojson.Basic.t -> convo
val contacts_from_json : Yojson.Basic.t -> string list
val accepted_friend_pairs_from_json : Yojson.Basic.t -> string list
val pending_friend_pairs_from_json : Yojson.Basic.t -> string list
val get_accepted_friends : string -> string list
val get_pending_friends : string -> string list

(* 
val from_json : Yojson.Basic.t -> string -> string -> t *)

