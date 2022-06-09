type player = {
  username : string;
  password : string;
  mutable game_history : (int * int) list;
}

val make_player : string -> string -> player
(** [make_player username password] creates a new user of type player
    using [username] as the account username, and [password] as the
    account password. *)

val update_player : int -> int -> player -> unit
(** [update_player last_game_length last_game_guesses user] updates the
    score_history field of type player to account for the most recent
    game that was played. [last_game_length] is an int that represents
    the word length of the last game played, and last_game_guesses is an
    int representing the number of guesses it took to guess the word.
    The int 7 represents the scenario where the word was not guessed. *)
