open OUnit2
open Jmodule
open Readingjson
open State
open Command
(********************************************************************
   TEST PLAN 
 ********************************************************************)
(*
  -1: The test plan does not explain which parts of the system were automatically tested by OUnit vs. manually tested.
  -1: The test plan does not explain what modules were tested by OUnit and how test cases were developed (black box, glass box, randomized, etc.).
  -1: The test plan does not provide an argument for why the testing approach demonstrates the correctness of the system.

  Our instant messaging system consists of 5 modules. 

   OUnit Tested: Readingjson, Command, State
   Manually Tested: JModule, Main
   (In readingjson we implemented )

   We implemented a glass box approach to testing our code. 
   In particular, we sought statement and condition coverage 
   through our test cases. *)

let identity x = x

(* ************               Readingjson Tests                *************  *)

let yojsont1 = Yojson.Basic.from_file "testconvo.json"
let yojsont2 = Yojson.Basic.from_file "testaccounts.json"
let convo1 =  "testconvo.json" |> Yojson.Basic.from_file  |> convo_from_json
let accounts1 = "logindetails.json" |> Yojson.Basic.from_file  |> accounts_from_json

(** [make_get_sent_by_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_sent_by input]. *)
let make_get_sent_by_test 
    (name : string) 
    (input: message) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_sent_by input ) ~printer: identity) 

(** [make_get_text_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_text input]. *)
let make_get_text_test 
    (name : string) 
    (input: message) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_text input) ~printer: identity) 

(** [make_get_username_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_username input]. *)
let make_get_username_test 
    (name : string) 
    (input: account) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_username input) ~printer: identity)

(** [make_get_password_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_password input]. *)
let make_get_password_test 
    (name : string) 
    (input: account) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_password input) ~printer: identity)

(** [make_is_verified_password_test name user pwd actlist expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_password input]. *)
let make_is_verified_password_test 
    (name : string) 
    (user: string) 
    (pwd: string) 
    (actlist: account list)
    (expected_output : bool ) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (is_verified_password user pwd actlist) ~printer: string_of_bool)


(** TESTS TO GO FOR READINGJSON
    - pending_friend_pairs_from_json
    - get_accepted_friends
    - get_pending_friends 
    - user_exists  *)

let readingjson_tests = [
  make_get_sent_by_test "get sent_by 1st sender" (List.nth convo1 0)
    "user2";
  make_get_sent_by_test "get sent_by 2nd sender" (List.nth convo1 1)
    "user2";
  make_get_sent_by_test "get sent_by last sender" (List.nth convo1 4)
    "user1";
  make_get_text_test "get text 1st sender" (List.nth convo1 0) "how about you";
  make_get_text_test "get text 2nd sender" (List.nth convo1 1) "nothing";
  make_get_text_test "get text last sender" (List.nth convo1 4) "hello there";
  make_get_username_test "get username first account" (List.nth accounts1 0) 
    "testuser3";
  make_get_username_test "get username 2nd account" (List.nth accounts1 1) 
    "testuser2";
  make_get_username_test "get username 3rd account" (List.nth accounts1 2) 
    "testuser1";
  make_get_password_test "get password first account" (List.nth accounts1 0) 
    "testpassword3";
  make_get_password_test "get password 2nd account" (List.nth accounts1 1) 
    "testpassword2";
  make_get_password_test "get password 3rd account" (List.nth accounts1 2) 
    "testpassword1";
  make_is_verified_password_test "valid username-password combo 1" "testuser1"
    "testpassword1" accounts1 true;
  make_is_verified_password_test "valid username-password combo 2" "testuser2"
    "testpassword2" accounts1 true;
  make_is_verified_password_test "valid username-password combo 3" "testuser3"
    "testpassword3" accounts1 true;
  make_is_verified_password_test "invalid username-password combo 1" "testuser1"
    "testpassword2" accounts1 false;
  make_is_verified_password_test "invalid username-password combo 2" "testuser2"
    "testpassword3" accounts1 false;
  make_is_verified_password_test "valid username-password combo 3" "testuser3"
    "falalalal" accounts1 false;

]

(* ************               Command Tests                    *************  *)


(* ************               State Tests                      *************  *)


let suite =
  "test suite for ims"  >::: List.flatten [
    readingjson_tests;
  ]

let _ = run_test_tt_main suite
