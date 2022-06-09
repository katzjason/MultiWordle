open Player

type player_database = (string * player) list
(** [player_database] is a list containing each player's username and
    the corresponding player type that matches the corresponding
    username. *)

val init_database : (string * player) list
(** [init_database] instantiates a new database for future players. *)

val update_database :
  string -> player -> player_database -> player_database
(** [update_database username player player_database] updates
    [player_database] to include a new tuple, [(username, player)]. *)

val games_played : string -> player_database -> int
(** [games_played username player_database] calculates the total number
    of games that a specific user has played. *)

val get_average_guesses : player -> int
(** [get_average_guesses player_record] generates the average number of
    guesses it takes a specific user to guess a word in Wordle. *)

val guess_trend : int list -> int
(** [guess_trend guess_list] calculates the average number of guesses
    needed to guess a word on the last three attempts for a given
    player's list of guesses. *)

val get_guess_trend : player -> int
(** [get_guess_trend player] calculates the average number of guess it
    has taken a specific user to guess a word in their last 3 games
    played. *)
