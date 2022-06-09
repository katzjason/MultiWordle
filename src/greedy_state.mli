type move_direction =
  | Left
  | Right
  | Up
  | Down

type greedy_state = {
  grid : int Grid.t;
  visited_grid : bool Grid.t;
  row_pos : int;
  col_pos : int;
  remaining_moves : int;
  coins_collected : int;
  steps_taken : int;
}

val init_greedy_state : unit -> greedy_state
(** [init_greedy_state ()] is a greedy_state initialized with a 9 by 9
    gameplay grid filled with random integers from 1 to 9 inclusive, a
    visited grid filled with false values except for center value true,
    starting coordinates at the middle of the gameplay grid, the
    remaining moves equal to 1, and the number of coins collected and
    steps taken both set to zero. *)

val update_greedy_state : greedy_state -> move_direction -> greedy_state
(** [update_greedy_state g m] is [g], but with all necessary changes
    that result from moving to an adjacent cell in the direction
    described by [m]. *)

val print_game_grid : greedy_state -> unit
(** [print_game_grid g] prints out the gameplay grid [g] so that any
    visited elements are blue and any unvisited elements are white. *)
