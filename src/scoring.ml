open String
open State

let rec string_to_string_list str =
  match str with
  | "" -> []
  | some_input ->
      if length some_input = 1 then [ some_input ]
      else
        sub some_input 0 1
        :: string_to_string_list
             (sub some_input 1 (length some_input - 1))

let rec score_helper
    (count : int)
    (str_list : string list)
    (correct_word_str_list : string list) : string =
  if count = List.length str_list then ""
  else
    let str_first_letter = List.nth str_list count in
    let correct_str_first_letter =
      List.nth correct_word_str_list count
    in
    if str_first_letter = correct_str_first_letter then
      uppercase_ascii str_first_letter
      ^ score_helper (count + 1) str_list correct_word_str_list
    else if List.mem str_first_letter correct_word_str_list then
      str_first_letter
      ^ score_helper (count + 1) str_list correct_word_str_list
    else "_" ^ score_helper (count + 1) str_list correct_word_str_list

let print_indent (most_recent_letter : char) =
  if most_recent_letter = 'p' then print_string "\n "
  else if most_recent_letter = 'l' then print_string "\n  "
  else print_string " "

let rec print_word_bank (game_state : state) (alphabet : char list) :
    unit =
  match alphabet with
  | [] -> print_endline ""
  | h :: t ->
      if List.mem h game_state.char_bank then (
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          (String.make 1 h |> String.uppercase_ascii);
        print_indent h;
        print_word_bank game_state t)
      else (
        ANSITerminal.print_string [ ANSITerminal.red ]
          (String.make 1 h |> String.uppercase_ascii);
        print_indent h;
        print_word_bank game_state t)

let score_input (user_input : string) (correct_word : string) : string =
  match user_input with
  | "" ->
      let len = length correct_word in
      make len '_'
  | input ->
      score_helper 0
        (input |> lowercase_ascii |> string_to_string_list)
        (correct_word |> string_to_string_list)

let rec print_colored_feedback (str : string) =
  if String.length str = 0 then print_endline ""
  else if String.get str 0 = '_' then (
    print_string "_";
    print_colored_feedback (String.sub str 1 (String.length str - 1)))
  else if String.get str 0 >= 'a' && String.get str 0 <= 'z' then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      (String.uppercase_ascii (String.make 1 (String.get str 0)));
    print_colored_feedback (String.sub str 1 (String.length str - 1)))
  else if String.get str 0 >= 'A' && String.get str 0 <= 'Z' then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      (String.make 1 (String.get str 0));
    print_colored_feedback (String.sub str 1 (String.length str - 1)))
  else
    failwith
      ("The character "
      ^ String.make 1 (String.get str 0)
      ^ " is not a valid feedback character.")