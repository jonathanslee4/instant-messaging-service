
(*[id_creator]: takes two usernames, determines which one comes first alphabetically,
  concatenates them and returns userID. *)
val id_creator : string -> string -> string

(* [json_creator]: creates json file name from id by concatenating ".json" to given id. *)
val json_creator : string -> string

(* [editingtext_json]: determines whether the sender and recipient have previously communicated. Dependent on whether the two
   users have communicated before, makes use of helper functions existing_convo and new_convo. *)
val editingtext_json : string -> string -> string -> unit

(* [afp_add]: when a new accepted friend pair is created, adds new friend pair to afp.json.  *)
val afp_add : string -> unit

(* [pfp_add]: when a new pending friend pair is created, and file already contains existing friend
   pairs, adds pending friend pair to pfp.json.  *)
val pfp_add : string -> unit

(* [pfp_empty]: when a new pending friend pair is created, and file does not contain existing friend
   pairs, adds pending friend pair to pfp.json.  *)
val pfp_empty : string -> unit

(* [pfp_remove]: takes in given string and removes it from "pfp.json". *)
val pfp_remove : string -> unit

(* [account_json_add]: adding password and username to json. *)
val account_json_add : string -> string -> unit
