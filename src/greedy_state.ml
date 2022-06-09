open Grid

type move_direction =
  | Left
  | Right
  | Up
  | Down

type greedy_state = {
  grid : int Grid.t;
  visited_grid : bool t;
  row_pos : int;
  col_pos : int;
  remaining_moves : int;
  coins_collected : int;
  steps_taken : int;
}

let init_greedy_state () =
  {
    grid = update_grid (generate_randomly_filled_int_grid 9 9 1 9) 4 4 1;
    visited_grid = update_grid (generate_all_false_grid 9 9) 4 4 true;
    row_pos = 4;
    col_pos = 4;
    remaining_moves = 1;
    coins_collected = 0;
    steps_taken = 0;
  }

let update_greedy_state greedy_state move_direction =
  let new_row_pos = ref greedy_state.row_pos in
  let new_col_pos = ref greedy_state.col_pos in
  (match move_direction with
  | Left -> new_col_pos := !new_col_pos - 1
  | Right -> new_col_pos := !new_col_pos + 1
  | Up -> new_row_pos := !new_row_pos - 1
  | Down -> new_row_pos := !new_row_pos + 1);

  if
    !new_row_pos < 0 || !new_row_pos > 8 || !new_col_pos < 0
    || !new_col_pos > 8
    || get greedy_state.visited_grid !new_row_pos !new_col_pos
  then (
    print_endline
      "You cannot move in this direction because you are either \
       visitng a nonexistent cell or a cell that has already been \
       visited. Please try again.";
    greedy_state)
  else
    let new_remaining_moves = ref (greedy_state.remaining_moves - 1) in

    let new_coins_collected = ref greedy_state.coins_collected in
    if !new_remaining_moves = 0 then (
      new_coins_collected :=
        !new_coins_collected
        + get greedy_state.grid !new_row_pos !new_col_pos;
      new_remaining_moves :=
        get greedy_state.grid !new_row_pos !new_col_pos);
    let new_steps_taken = greedy_state.steps_taken + 1 in
    let new_visited_grid =
      update_grid greedy_state.visited_grid !new_row_pos !new_col_pos
        true
    in
    {
      greedy_state with
      visited_grid = new_visited_grid;
      row_pos = !new_row_pos;
      col_pos = !new_col_pos;
      remaining_moves = !new_remaining_moves;
      coins_collected = !new_coins_collected;
      steps_taken = new_steps_taken;
    }

let print_game_grid greedy_state =
  for r = 0 to 8 do
    for c = 0 to 8 do
      if r = greedy_state.row_pos && c = greedy_state.col_pos then
        ANSITerminal.print_string [ ANSITerminal.green ]
          (string_of_int (get greedy_state.grid r c) ^ "   ")
      else if get greedy_state.visited_grid r c then
        ANSITerminal.print_string [ ANSITerminal.blue ]
          (string_of_int (get greedy_state.grid r c) ^ "   ")
      else
        print_string (string_of_int (get greedy_state.grid r c) ^ "   ")
    done;
    print_string "\n\n\n"
  done