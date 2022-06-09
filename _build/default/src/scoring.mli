open State

val score_input : string -> string -> string
(** [score_input user_input correct_word] generates an output string
    that indicates the correctness of the input string. The output
    string will be generated based on three cases. 1). The output string
    will feature an uppercase character if the input string has the same
    character and in the same index of the string as the string
    correct_word. 2). The output string will feature a lowercase
    character if the input string contains a letter in the correct_word,
    but it is in the wrong index. 3). The output string will feature a
    '_' if the given letter in the input string is not in correct_word.
    Examples: score_input "lover" "bands" will output "_ _ _ _ _",
    score_input "lover" "ocean" will output "o _ e _ _", score input_
    "lover" "cover" will output "_ O V E R". *)

val print_colored_feedback : string -> unit
(** [print_colored_feedback s] prints a color-coded representation of
    [s]. An individual char in [s] is printed as green if it is
    uppercase, as yellow if it is lowercase, and as an underscore if it
    is an underscore. *)

val print_word_bank : state -> char list -> unit
(** [print_word_bank s a] prints a color-coded representation of the
    alphabet given by [b]. An individual letter in [b] is printed as
    yellow if it has already been guessed. It is printed red otherwise. *)
