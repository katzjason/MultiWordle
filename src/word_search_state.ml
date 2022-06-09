open Data_processing
open Yojson.Basic.Util

let dictionary_json = "dictionary.json"

type wordsearch_state = {
  dictionary : string list; (*dictionary to draw words from*)
  hidden_words : string list; (*list of words in the word search*)
  found_words : string list;
  (*list of words correctly guessed by the user*)
  start_time : float;
  game_board : char list list;
      (*start tim eof the game, used to time how long they take*)
}

let init_wordsearch_game_state (size : string) : wordsearch_state =
  let dict =
    Yojson.Basic.from_file dictionary_json |> to_assoc |> make_dic
  in
  let hidden_words = make_hidden_words size dict in
  {
    dictionary = dict;
    hidden_words;
    found_words = [];
    start_time = Sys.time ();
    game_board = make_game_board hidden_words;
  }
