include module type of RawPuzzleType

val from_chn : in_channel -> raw_file
(** [from_chn chn] reads a set of puzzles from a channel and returns them along
    with their global rules.

    @param [chn] channel to read from
    @return the global rules and the list of puzzles *)

val get_dimension : t -> int * int
(** [get_dimension raw] returns the width and height of the puzzle.

    @param [raw] the raw puzzle
    @return [(width, height)] *)
