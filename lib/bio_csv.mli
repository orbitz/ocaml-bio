type quoting = Quote_all | Quote_minimal | Quote_none
type state = { data : string; pos : int; stream : string Stream.t; }
type dialect = {
  delim : char;
  rowdelim : char;
  quoting : quoting;
  quotechar : char;
  ignorews : bool;
}
val default_dialect : dialect
val chunk_size : int
val ensure_data : state -> (state -> 'a) -> (state -> 'a) -> 'a
val incr : state -> state
val char_at : state -> char
val empty_stream : 'a -> 'b Stream.t
val add_char : Buffer.t -> char -> Buffer.t
val rev_some : 'a list -> 'a list option
val identity : 'a -> 'a
val read_value : dialect -> Buffer.t -> bool -> state -> string * state
val read_columns : dialect -> string list -> state -> string list * state
val read_rows : dialect -> state -> string list Stream.t
val read_stream : dialect -> string Stream.t -> string list Stream.t
val read_file : dialect -> string -> string list Stream.t
