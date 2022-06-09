val check_start_game : char -> bool
(** [check_start_game c] is true if the user has input the letter 's'
    and thus wants to start playing. It is false for any other input. *)

val make_dic : (string * Yojson.Basic.t) list -> string list
(** [make_dic d] is a list of all string words inside the association
    list represented by [d]. *)

val generate_word_bank : int -> string list -> string list
(** [generate_word_bank n d] is a string list of all strings inside [d]
    that are of length [n]. *)

val choose_random_word : string list -> string
(** [choose_random_word d] is a random string inside of the string list
    represented by [d]. *)

val is_word : string -> string list -> bool
(** [is_word w d] is true if [w] is a member of [d]. It is false
    otherwise. *)

val make_hidden_words : string -> string list -> string list
(** [make_hidden_words size dic] takes a string of value "small",
    "medium", "large" and returns a string list of 4, 8 , 12
    respectively. *)

val make_game_board : string list -> char list list
(** [make game board h] is a word search board that hides inside the
    word search all words contained by [h]. The size of the board is
    determined by the length of [h]. returns: char list list, the word
    search board. *)
