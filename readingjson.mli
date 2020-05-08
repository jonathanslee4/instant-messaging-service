
(** The type representing a message from a user. *)
type message 

(** The type representing a conversation between two users. *)
type convo = message list

(** [get_sent_by message] is the string of the person who sent [message].  *)
val get_sent_by : message -> string

(** [get_text message] is the string of text sent from a particular user. *)
val get_text : message -> string

(** [convo_from json yojsont] is a unit that takes in json script and 
pipelines script to string attributes, requires: json is valid json script *)
val convo_from_json : Yojson.Basic.t -> convo

(** [pending_friend_pairs_from_json yojsont] is a list of pending friend pairs
from [yojsont] *)
val pending_friend_pairs_from_json : Yojson.Basic.t -> string list

(** [get_accepted_friends username] is a list of accepted friend pairs from
"afp.json". *) 
val get_accepted_friends : string -> string list

(** [get_pending_friends username] is a list of pending friend pairs from
"pfp.json". *) 
val get_pending_friends : string -> string list
