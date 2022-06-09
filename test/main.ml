open OUnit2
open Yojson.Basic.Util
open Game
open State
open Data_processing
open Storage
open Player

(*TEST PLAN: The main goal of our testing efforts were to make sure that
  the core functions required for each of the games we developed worked
  effectively. These functions ranged from scoring a string input
  against a chosen word for how similar their characters were (in the
  case of Wordle), to checking that certain conditions be true in the
  game over stages for a given game.

  The testing suite below uses OUnit to test the following main
  features, which were designed through a mix of black box and glass box
  testing. (By this, we mean that certain larger picture features such
  as game termination were testing based on their specification, whereas
  more specific features like scoring input were testing by assessing
  all possible acceptable input combinations): - Wordle inputs are
  scored properly - Player profile can be made and updated properly -
  Player statistics can be generated accurately - Word banks and "grids"
  can be created properly - Certain conditions (such as running out of
  lives) go hand in hand with the game being terminated, also known as
  Game Over.

  Outside of this testing suite, we used manual testing to test the
  following: - That the user interface displayed text in the proper
  colors and orientations - That the instructions were clear and the
  game stages could be progressed through - That printed outputs such as
  summary statistics were actually being output by the program

  Correctness of our system: As a terminal-based arcade, we felt that
  testing our product had to be achieved through both OUnit and manual
  testing, as some features (as outlined above) made more sense to test
  in one method vs the other. We believe that our extensive game play
  testing and OUnit test cases combine to provide a comprehensive test
  on our system, which we now believe is correct. *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_int i] pretty-prints int [i]. *)
let pp_int i = "\"" ^ string_of_int i ^ "\""

(** [pp_bool b] pretty-prints bool [b]. *)
let pp_bool b = "\"" ^ string_of_bool b ^ "\""

(** [score_input_test name user_input correct_word start_index exp]
    constructs an OUnit test named [name] that assets the quality of
    [exp] with [score_inout input]*)
let score_input_test
    (name : string)
    (user_input : string)
    (correct_word : string)
    (exp : string) =
  name >:: fun _ ->
  assert_equal exp
    (Scoring.score_input user_input correct_word)
    ~printer:pp_string

(** [make_player_test name username password expected_output] constructs
    an OUnit test named [name] that assets the quality of
    [expected_output] with [Player.make_player username password]*)
let make_player_test
    (name : string)
    (username : string)
    (password : string)
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output (Player.make_player username password)

(** [update_player_test name last_game_length last_game_guesses user exp]
    constructs an OUnit test named [name] that assets the quality of
    [exp] with
    [Player.update_player last_game_length last_game_guesses user]*)
let update_player_test
    (name : string)
    (last_game_length : int)
    (last_game_guesses : int)
    (user : player)
    (exp : unit) =
  name >:: fun _ ->
  assert_equal exp
    (Player.update_player last_game_length last_game_guesses user)

(** [games_played_test name username player_database expected_output]
    constructs an OUnit test named [name] that assets the quality of
    [expected_output] with
    [Storage.games_played username player_database]*)
let games_played_test
    (name : string)
    (username : string)
    (player_database : (string * player) list)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Storage.games_played username player_database)
    ~printer:pp_int

(** [average_guesses_test name player expected_output] constructs an
    OUnit test named [name] that assets the quality of [expected_output]
    with [Storage.get_average_guesses player]*)
let average_guesses_test
    (name : string)
    (player : player)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Storage.get_average_guesses player)
    ~printer:pp_int

(** [guess_trend_test name guess_list expected_output] constructs an
    OUnit test named [name] that assets the quality of [expected_output]
    with [Storage.guess_trend guess_list]*)
let guess_trend_test
    (name : string)
    (guess_list : int list)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Storage.guess_trend guess_list)
    ~printer:pp_int

(** [check_start_game_test name input expected_output] constructs an
    OUnit test named [name] that assets the quality of [expected_output]
    with [Dataprocessing.check_start_game input]*)
let check_start_game_test
    (name : string)
    (input : char)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (check_start_game input)

(** [generate_word_bank_test name input expected_output] constructs an
    OUnit test named [name] that assets the quality of [expected_output]
    with [Dataprocessing.generate_word_bank n d]*)
let generate_word_bank_test
    (name : string)
    (n : int)
    (d : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (generate_word_bank n d)

(** [choose_random_word_test name dic_list expected_output] constructs
    an OUnit test named [name] that assets the quality of
    [expected_output] with
    [List.mem (Dataprocessing.choose_random_word dic_list) (dict_list)]*)
let choose_random_word_test
    (name : string)
    (dic_list : string list)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (List.mem (choose_random_word dic_list) dic_list)
    ~printer:pp_bool

(** [is_word_test name w d expected_output] constructs an OUnit test
    named [name] that assets the quality of [expected_output] with
    [Dataprocessing.is_word w d]*)
let is_word_test
    (name : string)
    (w : string)
    (d : string list)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (is_word w d) ~printer:pp_bool

(** [make_hidden_words_test name size dic expected_output] constructs an
    OUnit test named [name] that assets the quality of [expected_output]
    with [Dataprocessing.make_hidden_words size dic]*)
let make_hidden_words_test
    (name : string)
    (n : string)
    (d : string list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (List.length (make_hidden_words n d))

let make_game_board_test
    (name : string)
    (words : string list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (List.length (make_game_board words))

let dic =
  Yojson.Basic.from_file "dictionary.json"
  |> to_assoc |> make_dic |> generate_word_bank 5

let rec check_word_length (word_list : string list) (len : int) =
  match word_list with
  | h :: t ->
      if String.length h <> len then false else check_word_length t len
  | [] -> true

let scoring_tests =
  [
    score_input_test "Empty input" "" "bands" "_____";
    score_input_test "Guessed correct word 5 letter" "bands" "bands"
      "BANDS";
    score_input_test "Guessed correct word double-check" "beach" "beach"
      "BEACH";
    score_input_test "Guessed correct word different capitalization"
      "bAnDs" "bands" "BANDS";
    score_input_test "Proper feedback" "hates" "bands" "_A__S";
    score_input_test "Proper feedback 2" "money" "epoxy" "_o_eY";
    score_input_test "Input with no correct letters" "trunk" "beach"
      "_____";
    score_input_test "3 letter correct word" "bed" "bed" "BED";
  ]

let empty_testing_dic = []
let testing_dic = [ "bed"; "came"; "bee"; "loud" ]

let data_processing_tests =
  [
    check_start_game_test
      "This tests the specification that if the user wants to start \
       the game by pressing 's', the function returns true"
      's' true;
    check_start_game_test
      "This tests that if the user doesn't pass in s, the game does \
       not start"
      'b' false;
    generate_word_bank_test "empty dic" 3 empty_testing_dic
      empty_testing_dic;
    generate_word_bank_test
      "generating a non-empty word bank for 3 letter words" 3
      testing_dic [ "bed"; "bee" ];
    generate_word_bank_test
      "generating a non-empty word bank for 4 letter words" 4
      testing_dic [ "came"; "loud" ];
    choose_random_word_test "word is inside dictionary that is provided"
      dic true;
    choose_random_word_test
      "word is inside an alternative dictionary: testing different \
       words "
      [ "a"; "b" ] true;
    is_word_test "word belongs to the dictionary provided" "beach" dic
      true;
    is_word_test "word does not belong to the dictionary provided"
      "beach"
      [ "sharks"; "dictionary"; "elephants" ]
      false
    (*Not sure if can test generate hidden words yet*);
  ]

let word_length = 5
let st1 = init_game_state word_length
let new_guess1 = "epoxy"
let st2 = update_game_state st1 new_guess1 false 0 0
let new_guess2 = st2.word
let st3 = update_game_state st2 new_guess2 true 2 3
let st4 = { st3 with remaining_guesses = 0 }

let state_tests =
  [
    ( "The word in newly-initiated state is of the proper length"
    >:: fun _ -> assert_equal word_length (String.length st1.word) );
    ( "The remaining guesses of newly-initiated state is 6" >:: fun _ ->
      assert_equal 6 st1.remaining_guesses );
    ( "The current guess of newly-initiated state is the empty string"
    >:: fun _ -> assert_equal "" st1.curr_guess );
    ( "The initial setting of the game over field of the state is false"
    >:: fun _ -> assert_equal false st1.game_over );
    ( "The initial setting for the last_game_length field is set to 0"
    >:: fun _ -> assert_equal 0 st1.last_game_length );
    ( "The initial setting for the last_game_guesses field is set to 0"
    >:: fun _ -> assert_equal 0 st1.last_game_guesses );
    ( "Updating a game state does not change the designated word"
    >:: fun _ -> assert_equal st1.word st2.word );
    ( "Updating a game state can change the last game length field"
    >:: fun _ -> assert_equal 2 st3.last_game_length );
    ( "Updating a game state can change the last game guesses field"
    >:: fun _ -> assert_equal 3 st3.last_game_guesses );
    ( "Updating a game state reduces the number of guesses by 1"
    >:: fun _ ->
      assert_equal st2.remaining_guesses (st1.remaining_guesses - 1) );
    ( "Updating a game state changes the current guess to the new guess"
    >:: fun _ -> assert_equal new_guess1 st2.curr_guess );
    ( "An ongoing game where there are remaining guesses left and \
       where the word has not beet guessed is not over"
    >:: fun _ -> assert_equal false (check_game_over st2) );
    ( "A game is over if the designated word has been guessed"
    >:: fun _ -> assert_equal true (check_game_over st3) );
    ( "A game is over if there are no remaining guesses" >:: fun _ ->
      assert_equal true (check_game_over st4) );
    ( "When a game is over, the game_over field should be set to true"
    >:: fun _ -> assert_equal true st4.game_over );
  ]

let player1 = make_player "bananalover34" "monkey"
let player2 = make_player "ocaml" "pragmaticprogrammer"
let player3 = update_player 4 5 (make_player "bananalover34" "monkey")

let player4 =
  update_player 5 6 (make_player "ocaml" "pragmaticprogrammer")

let player5 = make_player "username" "password"
let updated_player5 = update_player 3 4 player5

let player_tests =
  [
    make_player_test "player1 test" "bananalover34" "monkey" player1;
    make_player_test "player2 test" "ocaml" "pragmaticprogrammer"
      player2;
    update_player_test "adding one game of history to player1" 4 5
      player1 player3;
    update_player_test "adding one game of history to player2" 5 6
      player2 player4;
  ]

let no_player_database = init_database

let one_player_database =
  update_database "bananalover34"
    {
      username = "bananalover34";
      password = "monkey";
      game_history = [];
    }
    no_player_database

let two_player_database =
  update_database "ocaml"
    {
      username = "ocaml";
      password = "pragmaticprogrammer";
      game_history = [ (1, 2) ];
    }
    one_player_database

let storage_tests =
  [
    games_played_test "no games played" "bananalover34"
      one_player_database 0;
    games_played_test "one game played" "ocaml" two_player_database 1;
    average_guesses_test "no games played" player1 0;
    average_guesses_test "non-empty game history"
      (updated_player5;
       player5)
      4;
    guess_trend_test "no guesses" [] 0;
    guess_trend_test "one guess" [ 1 ] 1;
    guess_trend_test "two guesses" [ 1; 3 ] 2;
    guess_trend_test "three guesses" [ 2; 4; 6 ] 4;
    guess_trend_test "more than three guesses" [ 3; 4; 6; 7 ] 4;
  ]

let word_search_dic =
  [
    "bed";
    "came";
    "bee";
    "loud";
    "camera";
    "home";
    "repos";
    "coin";
    "game";
  ]

let word_search_test =
  [
    make_hidden_words_test
      "test if make_hidden_words test with small game returns correct \
       number of hidden words"
      "small" word_search_dic 4;
    make_hidden_words_test
      "test if make_hidden_words test with medium game returns correct \
       number of hidden words"
      "medium" word_search_dic 8;
    make_hidden_words_test
      "test if make_hidden_words test with medium game returns correct \
       number of hidden words"
      "large" word_search_dic 12;
    make_game_board_test
      "test if game board is of correct size with small game (ie 10)"
      [ "bed"; "came"; "bee"; "loud" ]
      10;
    make_game_board_test
      "test if game board is of correct size with small game (ie 10)"
      [ "bed"; "came"; "bee"; "loud"; "repos"; "home"; "coin"; "game" ]
      15;
  ]

let suite =
  "test suite for MultiWordle"
  >::: List.flatten
         [
           scoring_tests;
           state_tests;
           data_processing_tests;
           storage_tests;
           player_tests;
           word_search_test;
         ]

let _ = run_test_tt_main suite
