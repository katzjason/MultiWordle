type player = {
  username : string;
  password : string;
  mutable game_history : (int * int) list;
}

let make_player username password =
  { username; password; game_history = [] }

let update_player last_game_length last_game_guesses user : unit =
  user.game_history <-
    (last_game_length, last_game_guesses) :: user.game_history
