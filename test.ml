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

   We implemented a glass box approach to testing our code. 
   In particular, we sought statement and condition coverage 
   through our test cases. *)



(* ************               Readingjson Tests                *************  *)
let identity x = x

let yojsont1 = Yojson.Basic.from_file "testconvo.json"
let convo1 = yojsont1 |> convo_from_json



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

(** [make_accepeted_friend_pairs_from_json name input expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [get_text input]. *)
let make_accepted_friend_pairs_from_json_test 
    (name : string) 
    (input: message) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_text input) ~printer: identity) 

let readingjson_tests = [
  make_get_sent_by_test "get sent_by first sender" (List.nth convo1 0)
    "user2";
  make_get_sent_by_test "get sent_by second sender" (List.nth convo1 1)
    "user2";
  make_get_sent_by_test "get sent_by last sender" (List.nth convo1 4)
    "user1";
  make_get_text_test "get text first sender" (List.nth convo1 0) "how about you";
  make_get_text_test "get text second sender" (List.nth convo1 1) "nothing";
  make_get_text_test "get text first sender" (List.nth convo1 4) "hello therem";


]


(* ************               Command Tests                    *************  *)


(* ************               State Tests                      *************  *)


let suite =
  "test suite for ims"  >::: List.flatten [
    readingjson_tests;
  ]

let _ = run_test_tt_main suite
