open Game
open Data_processing
open State
open Scoring
open Storage
open Player
open Word_search_state
open Grid
open Greedy_state

let alphabet =
  [
    'q';
    'w';
    'e';
    'r';
    't';
    'y';
    'u';
    'i';
    'o';
    'p';
    'a';
    's';
    'd';
    'f';
    'g';
    'h';
    'j';
    'k';
    'l';
    'z';
    'x';
    'c';
    'v';
    'b';
    'n';
    'm';
  ]

type move_direction =
  | Left
  | Right
  | Up
  | Down

let rec game_iter_one (game_state : state) : state =
  print_endline
    ("Remaining guesses: " ^ string_of_int game_state.remaining_guesses);
  print_endline "\nYour guess: ";
  print_string "> ";
  let guess = read_line () in
  if String.length guess <> String.length game_state.word then (
    print_endline
      "You did not enter a string of valid length. Please try again.";
    game_iter_one game_state)
  else if is_word guess game_state.dictionary = false then (
    print_endline "You did not enter a valid word. Please try again.";
    game_iter_one game_state)
  else (
    print_colored_feedback (score_input guess game_state.word);
    let new_game_state = update_game_state game_state guess false 0 0 in
    print_word_bank new_game_state alphabet;
    if check_game_over new_game_state = false then
      game_iter_one new_game_state
    else
      let final_game_state =
        update_game_state new_game_state guess true
          (String.length new_game_state.word)
          (6 - new_game_state.remaining_guesses)
      in
      final_game_state)

let rec game_iter_two_state_update (game_state : state) : unit =
  print_string "> ";
  let input_word = read_line () in
  if String.length input_word <> String.length game_state.word then (
    print_endline
      "You did not enter a string of valid length. Please try again. ";
    game_iter_two_state_update game_state)
  else if is_word input_word game_state.dictionary = false then (
    print_endline "You did not enter a valid word. Please try again. ";
    game_iter_two_state_update game_state)
  else game_state.word <- input_word

let rec game_iter_two
    (player_one_game_state : state)
    (player_two_game_state : state)
    (player_one_points : int)
    (player_two_points : int)
    (round_number : int) =
  if round_number = 6 then
    if player_one_points > player_two_points then
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nPlayer 1 won!\n"
    else if player_one_points < player_two_points then
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nPlayer 2 won!\n"
    else
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nThe game ended in a tie\n"
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("ROUND " ^ string_of_int round_number);
    print_endline "\nCurrent score:";
    print_endline
      ("PLAYER ONE: "
      ^ string_of_int player_one_points
      ^ " | PLAYER TWO: "
      ^ string_of_int player_two_points);
    print_endline
      "\nPLAYER ONE, please enter a word for PLAYER TWO to guess";
    game_iter_two_state_update player_two_game_state;
    print_endline "\nPLAYER TWO, your turn to guess";
    let player_one_round_points =
      (game_iter_one player_two_game_state).remaining_guesses
    in

    print_endline
      "\nPLAYER TWO, please enter a word for PLAYER ONE to guess";
    game_iter_two_state_update player_one_game_state;
    print_endline "PLAYER ONE, your turn to guess";
    let player_two_round_points =
      (game_iter_one player_one_game_state).remaining_guesses
    in

    game_iter_two
      (init_game_state (String.length player_one_game_state.word))
      (init_game_state (String.length player_one_game_state.word))
      (player_one_points + player_one_round_points)
      (player_two_points + player_two_round_points)
      (round_number + 1))

let rec word_contains_chars (word : string) (letters : char list) =
  match word with
  | "" -> false
  | _ ->
      let first_letter = word.[0] in
      let rest_word = String.sub word 1 (String.length word - 1) in
      if List.mem first_letter letters then true
      else word_contains_chars rest_word letters

let generate_devious_word (game_state : state) : string =
  let word_bank =
    generate_word_bank
      (String.length game_state.word)
      game_state.dictionary
  in
  let devious_word_bank =
    List.filter
      (fun word ->
        word_contains_chars word game_state.char_bank = false)
      word_bank
  in
  if List.length devious_word_bank = 0 then game_state.word
  else List.hd devious_word_bank

let rec game_iter_absurdle (game_state : state) =
  print_endline "\nYour guess: ";
  print_string "> ";
  let guess = read_line () in
  if String.length guess <> String.length game_state.word then (
    print_endline
      "You did not enter a string of valid length. Please try again.";
    game_iter_absurdle game_state)
  else if is_word guess game_state.dictionary = false then (
    print_endline "You did not enter a valid word. Please try again.";
    game_iter_absurdle game_state)
  else
    let new_game_state = update_game_state game_state guess false 0 0 in
    new_game_state.word <- generate_devious_word new_game_state;
    print_colored_feedback (score_input guess new_game_state.word);
    print_word_bank new_game_state alphabet;
    if new_game_state.curr_guess = new_game_state.word then
      print_endline "Congrats you guessed the word!\n"
    else game_iter_absurdle new_game_state

let rec play_wordle_game
    (num_letters : int)
    (database : player_database) : unit =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nGAME MODE\n";
  print_endline
    "Select one of the game modes below to get started\n\
     One Player | Two Player | Absurdle";
  print_string "> ";
  let input = read_line () in
  match String.lowercase_ascii input with
  | "one player" -> (
      print_endline "Please enter the Username you wish to play under";
      print_string "> ";
      let username = read_line () in
      let existing_player =
        try List.assoc username database
        with _ -> (
          print_endline
            "Username not found. Press T to try again, or C to create \
             a user profile with this username";
          let next_move = read_line () in
          match next_move with
          | "C" ->
              print_endline "Please enter a password for this username";
              print_string "> ";
              let password = read_line () in

              let new_player = make_player username password in
              ignore (update_database username new_player database);
              new_player
          | _ -> make_player "" "")
      in
      if existing_player.username = "" then
        play_wordle_game num_letters database
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nONE PLAYER INSTRUCTIONS\n";
      print_endline
        "Welcome to one player mode! This mode simulates a classic \
         Wordle game where you have six attempts to guess a \
         predetermined word.";

      let end_state = game_iter_one (init_game_state num_letters) in
      update_player end_state.last_game_length
        end_state.last_game_guesses existing_player;
      print_string "Here are your summary statistics.";
      print_endline ("Username: " ^ existing_player.username);
      print_endline
        ("Number of games played: "
        ^ string_of_int (games_played existing_player.username database)
        );
      print_endline
        ("Average number of guesses need per game: "
        ^ string_of_int (existing_player |> get_average_guesses));
      print_endline
        ("Average number of guesses needed for last three games: "
        ^ string_of_int (get_guess_trend existing_player));
      print_endline
        "Would you like to play again? Press Y for yes and N for no";
      print_string "> ";
      let continue = read_line () in
      match continue with
      | "Y" -> play_wordle_game num_letters database
      | _ -> ignore end_state)
  | "two player" ->
      (* ( (*start parenthesis*) print_endline "Please enter the
         username Player 1 wishes to utilize. "; print_string "> "; let
         p1username = read_line () in ( let p1 =

         try List.assoc p1username database with | _ -> ( print_endline
         "User not found. Press T to try again or C to create a new user
         profile"; let next_step = read_line () in match next_step with
         | "C" -> ( print_endline "Please enter a password for this
         profile"; let p1password = read_line () in let new_player =
         make_player p1username p1password in ignore (update_database
         username new_player database); new_player | _ -> make_player ""
         "")

         )

         in if p1.username = "" then play_wordle_game num_letters
         database else *)
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nTWO PLAYER INSTRUCTIONS\n";
      print_endline
        "Welcome to two player mode! Please first decide the player \
         that will be Player One and the player that will be Player \
         Two. This mode consists of five rounds, after which the \
         player with the most points will win. In each round, Player \
         One will choose a word that Player Two must guess, and then \
         Player Two will choose a word that Player One must guess. \
         Each player will have 6 guesses to guess their assigned word, \
         and if they guess the word in x tries, then they will earn (6 \
         - x) points for that round.\n";

      game_iter_two
        (init_game_state num_letters)
        (init_game_state num_letters)
        0 0 1
  | "absurdle" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nABSURDLE INSTRUCTIONS\n";
      print_endline
        "Welcome to absurdle! This mode is a slight variant of a \
         similar game made by qntm. In this mode, you must also guess \
         a target word. However, after every one of your guesses, our \
         program will change the target word so that your guess is as \
         different from the new target word as possible. Frequently, \
         our program will come up with a new target word by scanning \
         the dictionary for a word that does not have any letters that \
         you have guessed so far. However, eventually this becomes \
         impossible, so our program will instead find words that use \
         as little previously-guessed letters as possible. It will be \
         nearly impossible to guess the target word in six tries, so \
         instead you will have unlimited tries to beat the game.";
      game_iter_absurdle (init_game_state num_letters)
  | _ ->
      print_endline "You did not enter a valid command";
      play_wordle_game num_letters database

let play_wordle (database : player_database) () : unit =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nINSTRUCTIONS\n";
  print_endline
    "Welcome to MultiWordle! Your objective is to guess a \
     predetermined word (length is your choice) with only 6 guesses. \
     For every word that you guess, our system will output that exact \
     word, but with each letter colorcoded. A yellow letter means the \
     predetermined word has that letter, but the letter is in the \
     wrong place. A green letter means that the predetermined word has \
     that letter, and the letter is in the right place. An underscore \
     means that that letter is not in the predetermined word. To play, \
     enter a number between 3 and 10 to determine the length of the \
     word you will be guessing.\n";
  print_string "> ";
  let s = read_line () in
  try play_wordle_game (int_of_string s) database
  with _ -> print_endline "You did not enter a valid command"

let map_num_letters (size : string) : string =
  match size with
  | "small" -> string_of_int 4
  | "medium" -> string_of_int 8
  | "large" -> string_of_int 12
  | _ -> failwith "not possible"

let rec print_matrix (game_board : char list list) : unit =
  match game_board with
  | [] -> print_endline ""
  | h :: t ->
      List.iter (Printf.printf "%c ") h;
      print_endline "";
      print_matrix t

let rec check_word (guess : string) (hidden_words : string list) : bool
    =
  match hidden_words with
  | [] -> false
  | h :: t -> if guess = h then true else check_word guess t

let update_state (guess : string) (game_state : wordsearch_state) :
    wordsearch_state =
  if check_word guess game_state.hidden_words then
    let new_game_state =
      { game_state with found_words = guess :: game_state.found_words }
    in
    new_game_state
  else game_state

let rec game_iter_word_search (game_state : wordsearch_state) : unit =
  if
    List.length game_state.hidden_words
    - List.length game_state.found_words
    = 0
  then print_endline "Congrats! You Found all the Words!"
    (* print_endline ("Time Elapsed: "^ string_of_float (Sys.time () -.
       game_state.start_time)^ ""); *)
  else (
    print_matrix game_state.game_board;
    print_endline
      ("You have "
      ^ string_of_int
          (List.length game_state.hidden_words
          - List.length game_state.found_words)
      ^ " words left to find. \n\
        \  Please type a word below when you find one");
    print_endline "> ";
    let guess = read_line () in
    let new_state = update_state guess game_state in
    if check_word guess game_state.hidden_words then (
      print_endline " You guessed the word!";
      game_iter_word_search new_state)
    else print_endline "That's not a word in the dictionary";
    game_iter_word_search new_state)

let play_word_search_game (size : string) : unit =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nGAME MODE\n";
  print_endline
    ("hidden in the above grid are " ^ map_num_letters size
   ^ " letters. Type \n\
     \ in the terminal below until you find all the letters");
  let game_state = init_wordsearch_game_state size in
  game_iter_word_search game_state

let word_search () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nINSTRUCTIONS\n";
  print_endline
    "Welcome to Wordsearch! Your objective is to spot all the words \
     hidden in the grid of letters. When you find a word, type it into \
     the terminal. When you find all the words, you win! Begin your \
     adventure by typing small, medium or large, to determine the size \
     of your word search game.";
  print_string "> ";
  let s = read_line () in
  try play_word_search_game s
  with _ -> print_endline "You did not enter a valid command"

let check_greedy_game_ending (greedy_state : greedy_state) : bool =
  if
    (greedy_state.row_pos - 1 < 0
    || get greedy_state.visited_grid
         (greedy_state.row_pos - 1)
         greedy_state.col_pos)
    && (greedy_state.row_pos + 1 > 8
       || get greedy_state.visited_grid
            (greedy_state.row_pos + 1)
            greedy_state.col_pos)
    && (greedy_state.col_pos - 1 < 0
       || get greedy_state.visited_grid greedy_state.row_pos
            (greedy_state.col_pos - 1))
    && (greedy_state.col_pos + 1 > 8
       || get greedy_state.visited_grid greedy_state.row_pos
            (greedy_state.col_pos + 1))
  then (
    print_endline
      ("You cannot move left, right, up, or down, so the game is over. \
        Your final coin efficiency was: "
      ^ string_of_float
          (float_of_int greedy_state.coins_collected
          /. float_of_int greedy_state.steps_taken)
      ^ "\n\n");
    true)
  else false

let rec play_greedy_iter (greedy_state : greedy_state) =
  if check_greedy_game_ending greedy_state = false then (
    print_game_grid greedy_state;
    print_endline
      ("Current coin count: "
      ^ string_of_int greedy_state.coins_collected);
    print_endline
      ("Current steps taken: " ^ string_of_int greedy_state.steps_taken);
    print_endline
      ("You must take "
      ^ string_of_int greedy_state.remaining_moves
      ^ " steps before collecting next coins.");
    print_endline
      "Please enter \"left\", \"right\", \"up\", or \"down\" to move";
    print_string "> ";
    let next_move = read_line () in
    match String.lowercase_ascii next_move with
    | "left" -> play_greedy_iter (update_greedy_state greedy_state Left)
    | "right" ->
        play_greedy_iter (update_greedy_state greedy_state Right)
    | "up" -> play_greedy_iter (update_greedy_state greedy_state Up)
    | "down" -> play_greedy_iter (update_greedy_state greedy_state Down)
    | _ ->
        print_endline "Invalid command entered. Please try again.";
        play_greedy_iter greedy_state)

let play_greedy () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nINSTRUCTIONS\n";
  print_endline
    "Welcome to Greedy! Your objective is to continually move through \
     a 9 by 9 grid while picking up coins. Each cell in the grid \
     contains a certain number of coins. At the beginning of the game, \
     you will find yourself in the middle of the grid, at which point \
     you may take a step to the left, right, up, or down. If you move \
     into a cell containing x amount of coins, you will pick up x \
     coins, but you will also need to move x steps in the next round. \
     You may only pick up coins when ending your steps in a cell. So \
     in the previous examples, you may only pick up the coins on the \
     cell you end up on after taking x steps. You may not revisit the \
     cells that you pass through when completing those x steps. The \
     game ends when you cannot possibly move to the left, right, up, \
     or down without moving off the grid or revisiting a visited cell. \
     Your final score is your coin efficiency, that is the number of \
     coins you collected divided by the number of steps you took \
     during the game.\n";
  play_greedy_iter (init_greedy_state ())

let main () : unit =
  let database = init_database in

  ANSITerminal.print_string [ ANSITerminal.red ] "\nWELCOME TO ARCADE\n";
  print_endline "To begin, please enter a username";
  print_string "> ";

  let username = read_line () in

  print_endline "\nPlease enter a password for this user profile";
  print_string "> ";

  let password = read_line () in
  let new_player = Player.make_player username password in

  let new_database = update_database username new_player database in
  print_endline
    "\n\
     Please choose from the following games modes: | MultiWordle | \
     Word Search | Greedy";
  print_string "> ";

  let s = read_line () in
  match String.lowercase_ascii s with
  | "multiwordle" -> play_wordle new_database ()
  | "word search" -> word_search ()
  | "greedy" -> play_greedy ()
  | _ -> print_endline "You did not enter a valid command"

let () = main ()
