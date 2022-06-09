open Yojson.Basic.Util
open Data_processing

let dictionary_json = "dictionary.json"

type state = {
  dictionary : string list;
  mutable word : string;
  remaining_guesses : int;
  curr_guess : string;
  char_bank : char list;
  game_over : bool;
  last_game_length : int;
  last_game_guesses : int;
}

let init_game_state (num_letters : int) : state =
  {
    dictionary =
      Yojson.Basic.from_file dictionary_json |> to_assoc |> make_dic;
    word =
      Yojson.Basic.from_file dictionary_json
      |> to_assoc |> make_dic
      |> generate_word_bank num_letters
      |> choose_random_word;
    remaining_guesses = 6;
    curr_guess = "";
    char_bank = [];
    game_over = false;
    last_game_length = 0;
    last_game_guesses = 0;
  }

let update_char_bank (char_bank : char list) (guess : string) :
    char list =
  let char_list = List.init (String.length guess) (String.get guess) in
  List.append char_list char_bank |> List.sort_uniq compare

let update_game_state
    (game_state : state)
    (new_guess : string)
    (game_over : bool)
    (length : int)
    (guesses : int) : state =
  {
    dictionary = game_state.dictionary;
    word = game_state.word;
    remaining_guesses = game_state.remaining_guesses - 1;
    curr_guess = new_guess;
    char_bank =
      update_char_bank game_state.char_bank (new_guess : string);
    game_over;
    last_game_length = length;
    last_game_guesses = guesses;
  }

let check_game_over (game_state : state) : bool =
  if game_state.curr_guess = game_state.word then (
    print_endline "Congrats you guessed the word!\n";
    true)
  else if game_state.remaining_guesses = 0 then (
    print_endline
      ("You ran out of guesses. The correct word was " ^ game_state.word);
    true)
  else false

let to_string (game_state : state) : string =
  "Word: " ^ game_state.word ^ " | Remaining Guesses: "
  ^ string_of_int game_state.remaining_guesses
  ^ " | Current Guess: " ^ game_state.curr_guess
