type wordsearch_state = {
  dictionary : string list;
  hidden_words : string list;
  found_words : string list;
  start_time : float;
  game_board : char list list;
}
(** The type wordsearch_state is a record that storess all information
    about the word search being played. [dictionary] is the dictionary
    from which random words are drawn from. [hidden_words] is a subset
    of strings in dictionary that are hidden in the word search
    [found_words] are the words that a player has guessed and found...
    initialized to []. [start_time] tracks the time the game started,
    allowing for calculation the duration of the game. [game_bard] is a
    matrix of characters that include the words in [hidden_words] *)

val init_wordsearch_game_state : string -> wordsearch_state

(**val init_wordsearch_game_state takes a string of "small, medium, or
   large" and initalizes a wordsearch state to {dic; hidden_words;
   found_words; start_time; game_board}*)
