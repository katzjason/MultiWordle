let check_start_game (user_input : char) : bool =
  if user_input = 's' then true else false

let rec make_dic (dic : (string * Yojson.Basic.t) list) : string list =
  match dic with
  | [] -> []
  | (name, _) :: t -> name :: make_dic t

let rec generate_word_bank (num_letters : int) (dic : string list) :
    string list =
  match dic with
  | [] -> []
  | h :: t ->
      if String.length h = num_letters then
        h :: generate_word_bank num_letters t
      else generate_word_bank num_letters t

let choose_random_word (dic : string list) : string =
  let _ = Random.self_init () in
  List.nth dic (Random.int (List.length dic))

let is_word (word : string) (dic : string list) : bool =
  List.mem word dic

let rec get_filtered_words (dic : string list) : string =
  (*ensure word returned is less than length 8s*)
  let proposed_word = choose_random_word dic in
  if String.length proposed_word < 5 then proposed_word
  else get_filtered_words dic

let rec generate_hidden_words
    (size : int)
    (dic : string list)
    (word_list : string list) : string list =
  if List.length word_list = size then word_list
  else
    let word_list = get_filtered_words dic :: word_list in
    generate_hidden_words size dic word_list

let make_hidden_words (size : string) (dic : string list) : string list
    (* make_hidden_words [s d] selects 4 words out of dictionary d
       randomly of if s = 'small', 8 words out of dic d randomly if s =
       'medium', or 12 words out of dictionary d randomly if s =
       'large'. Otherwise, throws an exception (formally not
       possible) *) =
  match size with
  | "small" -> generate_hidden_words 4 dic []
  | "medium" -> generate_hidden_words 8 dic []
  | "large" -> generate_hidden_words 12 dic []
  | _ -> failwith "not possible"

let reverse_index (i : int) : char = Char.chr i

let rec add_char_array_helper
    (board_size : int)
    (return_array : char list) : char list =
  if List.length return_array = board_size then return_array
  else
    let new_array =
      (let _ = Random.self_init () in
       reverse_index (97 + Random.int 25))
      :: return_array
    in
    add_char_array_helper board_size new_array

let add_char_array (board_size : int) : char list =
  add_char_array_helper board_size []

let rec fill_in_board (board_size : int) (return_array : char list list)
    : char list list =
  if List.length return_array = board_size then return_array
  else
    let new_array = add_char_array board_size :: return_array in
    fill_in_board board_size new_array

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let add_char_front (char_list : char list) : char list =
  Char.chr (97 + Random.int 25) :: char_list

let add_char_back (char_list : char list) : char list =
  List.append char_list [ Char.chr (97 + Random.int 25) ]

let rec fill_in_char_helper
    (original_list : char list)
    (desired_length : int)
    (start_num : int)
    (orig_length : int) : char list =
  if List.length original_list < orig_length + start_num then
    fill_in_char_helper
      (add_char_front original_list)
      desired_length start_num orig_length
  else if List.length original_list < desired_length then
    fill_in_char_helper
      (add_char_back original_list)
      desired_length start_num orig_length
  else original_list

let fill_in_char_list
    (original_list : char list)
    (desired_length : int)
    (start_num : int) : char list =
  fill_in_char_helper original_list desired_length start_num
    (List.length original_list)

let rec create_new_matrix
    (old_char_array : char list)
    (new_char_array : char list)
    (char_matrix : char list list) =
  match char_matrix with
  | h :: t ->
      if h = old_char_array then
        new_char_array
        :: create_new_matrix old_char_array new_char_array t
      else h :: create_new_matrix old_char_array new_char_array t
  | [] -> []

let modify_horizontal_array
    (char_array : char list)
    (word : string)
    (col_num : int)
    (char_matrix : char list list) : char list list =
  let char_list = string_to_char_list word in
  let new_char_list =
    fill_in_char_list char_list (List.length char_matrix) col_num
  in
  create_new_matrix char_array new_char_list char_matrix

let hide_word_horizontal (word : string) (char_matrix : char list list)
    : char list list =
  let word_len = String.length word in
  let mat_len = List.length char_matrix in
  let col_seed = Random.int mat_len - word_len in
  let row_seed = Random.int word_len in
  modify_horizontal_array
    (List.nth char_matrix row_seed)
    word col_seed char_matrix

(* let hide_word_vertical (word : string) (char_matrix : char list list)
   : char list list = failwith "todo"

   let hide_word_diagonal (word : string) (char_matrix : char list list)
   : char list list = failwith "todo" *)

let rec hide_words_in_board
    (hidden_words : string list)
    (char_matrix : char list list) : char list list =
  match hidden_words with
  | [] -> char_matrix
  | h :: t -> (
      let word_pattern = Random.int 2 in
      match word_pattern with
      | 0 ->
          let new_char_matrix = hide_word_horizontal h char_matrix in
          hide_words_in_board t new_char_matrix
      | 1 ->
          let new_char_matrix = hide_word_horizontal h char_matrix in
          hide_words_in_board t new_char_matrix
      | 2 ->
          let new_char_matrix = hide_word_horizontal h char_matrix in
          hide_words_in_board t new_char_matrix
      | _ -> failwith "failure")

let make_game_board (hidden_words : string list) : char list list =
  match List.length hidden_words with
  | 4 -> fill_in_board 10 [] |> hide_words_in_board hidden_words
  | 5 -> fill_in_board 10 [] |> hide_words_in_board hidden_words
  | 6 -> fill_in_board 10 [] |> hide_words_in_board hidden_words
  | 7 -> fill_in_board 10 [] |> hide_words_in_board hidden_words
  | 8 -> fill_in_board 15 [] |> hide_words_in_board hidden_words
  | 9 -> fill_in_board 15 [] |> hide_words_in_board hidden_words
  | 10 -> fill_in_board 15 [] |> hide_words_in_board hidden_words
  | 11 -> fill_in_board 15 [] |> hide_words_in_board hidden_words
  | 12 -> fill_in_board 20 [] |> hide_words_in_board hidden_words
  | 13 -> fill_in_board 20 [] |> hide_words_in_board hidden_words
  | 14 -> fill_in_board 20 [] |> hide_words_in_board hidden_words
  | 15 -> fill_in_board 20 [] |> hide_words_in_board hidden_words
  | _ -> failwith "too many words :0"
