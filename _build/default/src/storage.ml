open Player

type player_database = (string * player) list

let init_database = []

let update_database username player database =
  (username, player) :: database

let rec get_usernames (database : player_database) : string list =
  match database with
  | [] -> []
  | [ (h, _) ] -> [ h ]
  | (h, _) :: t -> h :: get_usernames t

let get_game_history (player_record : player) : (int * int) list =
  player_record.game_history

let rec get_guesses (history : (int * int) list) : int list =
  match history with
  | [] -> []
  | [ (_, guess) ] -> [ guess ]
  | (_, guess) :: t -> guess :: get_guesses t

let get_player_record (username : string) (database : player_database) :
    player =
  let usernames_list = get_usernames database in
  if List.mem username usernames_list then List.assoc username database
  else failwith "Player does not exist"

let games_played (username : string) (database : player_database) : int
    =
  let player_record = get_player_record username database in
  List.length player_record.game_history

let rec list_sum lst =
  match lst with
  | [] -> 0
  | [ h ] -> h
  | h :: t -> h + list_sum t

let last_three lst =
  match lst with
  | [] -> []
  | [ h ] -> [ h ]
  | [ h; t ] -> [ h; t ]
  | h :: m :: t :: _ -> [ h; m; t ]

let average_guesses (guesses : int list) : int =
  if List.length guesses <> 0 then
    let sum = list_sum guesses in
    sum / List.length guesses
  else 0

let get_average_guesses (player_record : player) : int =
  get_game_history player_record |> get_guesses |> average_guesses

let guess_trend (guesses : int list) : int =
  guesses |> last_three |> average_guesses

let get_guess_trend (player : player) : int =
  player |> get_game_history |> get_guesses |> last_three
  |> average_guesses
