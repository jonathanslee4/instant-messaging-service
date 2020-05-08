
(** The type representing a message from a user. *)
type message 

(** The type representing a conversation between two users. *)
type convo = message list

(** [get_sent_by message] is the username of the person who sent [message].  *)
val get_sent_by : message -> string

(** [get_text message] is the *)
val get_text : message -> string
val convo_from_json : Yojson.Basic.t -> convo
val contacts_from_json : Yojson.Basic.t -> string list
val accepted_friend_pairs_from_json : Yojson.Basic.t -> string list
val pending_friend_pairs_from_json : Yojson.Basic.t -> string list
val get_accepted_friends : string -> string list
val get_pending_friends : string -> string list

(* 
val from_json : Yojson.Basic.t -> string -> string -> t *)

