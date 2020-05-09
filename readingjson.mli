(**  
    Handles reading/retrieving information from different types of 
    json files used in the instant messaging system, including jsons 
    representing registered users, accepted friend requests, pending friend
    requests, and conversations. 

*)

(** The type representing a message from a user. *)
type message 

(** The type representing a conversation between two users. *)
type convo = message list

(** The type representing a user's account. *)
type account 

(** [get_sent_by message] is the string of the person who sent [message].  *)
val get_sent_by : message -> string

(** [get_text message] is the string of text sent from a particular user. *)
val get_text : message -> string

(** [get_account_username act] is the username associated with [act]. *)
val get_username : account -> string

(** [get_account_username act] is the password associated with [act]. *)
val get_password : account -> string 

(** [convo_from_json yojsont] is a unit that takes in json script and 
    pipelines script to string attributes, requires: json is valid json 
    script. *)
val convo_from_json : Yojson.Basic.t -> convo

(** [accepted_friend_pairs_from_json yojsont] is a list of accepted friend pairs
    from [yojsont]. *) 
val accepted_friend_pairs_from_json : Yojson.Basic.t -> string list

(** [pending_friend_pairs_from_json yojsont] is a list of pending friend pairs
    from [yojsont] *)
val pending_friend_pairs_from_json : Yojson.Basic.t -> string list

(** [get_accepted_friends username] is a list of the friends of [username]. *) 
val get_accepted_friends : string -> string list

(** [get_pending_friends username] is a list of the individuals who have sent
    a pending friend request to [username]. *) 
val get_pending_friends : string -> string list

(* [accounts_from_json j] is a unit that parses [j] and returns an account
   list. *)
val accounts_from_json : Yojson.Basic.t -> account list

(* [user_exists usr] is a boolean that denotes if [usr] exists in 
   "logindetails.json". *)
val user_exists : string -> bool

(* [is_verified_password usr pwd actlist] is a boolean that denotes if [usr] and
   [pwd] match pair in [actlist] and if not, displays error message.*)
val is_verified_password : string -> string -> account list -> bool
