type 'a t = 'a list list

let empty = []

let generate_random_int (lower_bound : int) (upper_bound : int) : int =
  let _ = Random.self_init () in
  lower_bound + Random.int (upper_bound - lower_bound + 1)

let rec generate_randomly_filled_int_list
    (length : int)
    (lower_bound : int)
    (upper_bound : int) : int list =
  if length = 0 then []
  else
    generate_random_int lower_bound upper_bound
    :: generate_randomly_filled_int_list (length - 1) lower_bound
         upper_bound

let rec generate_randomly_filled_int_grid
    (rows : int)
    (columns : int)
    (lower_bound : int)
    (upper_bound : int) : int t =
  if rows = 0 then []
  else
    generate_randomly_filled_int_list columns lower_bound upper_bound
    :: generate_randomly_filled_int_grid (rows - 1) columns lower_bound
         upper_bound

let get (grid : 'a t) (row : int) (column : int) =
  List.nth (List.nth grid row) column

let rec update_list (list : 'a list) (index : int) (new_elem : 'a) :
    'a list =
  match list with
  | [] -> []
  | _ :: t when index = 0 -> new_elem :: t
  | h :: t -> h :: update_list t (index - 1) new_elem

let update_grid (grid : 'a t) (row : int) (column : int) (new_elem : 'a)
    =
  update_list grid row (update_list (List.nth grid row) column new_elem)

let rec generate_all_false_row (columns : int) =
  if columns = 0 then []
  else false :: generate_all_false_row (columns - 1)

let rec generate_all_false_grid (rows : int) (columns : int) =
  if rows = 0 then []
  else
    generate_all_false_row columns
    :: generate_all_false_grid (rows - 1) columns
