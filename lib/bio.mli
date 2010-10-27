module Fasta :
  sig
    type fasta = Bio_fasta.fasta = Header of string | Sequence of string
    val read : string Stream.t -> fasta Stream.t
    val read_file : ?chunk_size:int -> string -> fasta Stream.t
    val read_strict : string Stream.t -> fasta Stream.t
    val read_file_strict : ?chunk_size:int -> string -> fasta Stream.t
  end
module Genbank :
  sig val gene_re : Str.regexp val read : string -> (int * int) list end
module Csv :
  sig
    module String_ext :
      sig
        external length : string -> int = "%string_length"
        external get : string -> int -> char = "%string_safe_get"
        external set : string -> int -> char -> unit = "%string_safe_set"
        external create : int -> string = "caml_create_string"
        val make : int -> char -> string
        val copy : string -> string
        val sub : string -> int -> int -> string
        val fill : string -> int -> int -> char -> unit
        val blit : string -> int -> string -> int -> int -> unit
        val concat : string -> string list -> string
        val iter : (char -> unit) -> string -> unit
        val escaped : string -> string
        val index : string -> char -> int
        val rindex : string -> char -> int
        val index_from : string -> int -> char -> int
        val rindex_from : string -> int -> char -> int
        val contains : string -> char -> bool
        val contains_from : string -> int -> char -> bool
        val rcontains_from : string -> int -> char -> bool
        val uppercase : string -> string
        val lowercase : string -> string
        val capitalize : string -> string
        val uncapitalize : string -> string
        type t = string
        val compare : t -> t -> int
        external unsafe_get : string -> int -> char = "%string_unsafe_get"
        external unsafe_set : string -> int -> char -> unit
          = "%string_unsafe_set"
        external unsafe_blit : string -> int -> string -> int -> int -> unit
          = "caml_blit_string" "noalloc"
        external unsafe_fill : string -> int -> int -> char -> unit
          = "caml_fill_string" "noalloc"
        val compare_subs_ :
          string -> string -> int -> int -> int -> int -> bool
        val compare_subs : string -> string -> int -> int -> bool
        val startswith : string -> string -> bool
        val find_from : string -> string -> int -> int
        val find : string -> string -> int
        val get_string_between : string -> string -> string -> string
        val split : ?sidx:int -> string -> string -> string list
        val whitespace : char -> bool
        val ltrim : ?f:(char -> bool) -> string -> string
        val rtrim : ?f:(char -> bool) -> string -> string
        val trim : ?f:(char -> bool) -> string -> string
      end
    type quoting = Bio_csv.quoting = Quote_all | Quote_minimal | Quote_none
    type state =
      Bio_csv.state = {
      data : string;
      pos : int;
      stream : string Stream.t;
    }
    type dialect =
      Bio_csv.dialect = {
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
  end
module String_ext :
  sig
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    external set : string -> int -> char -> unit = "%string_safe_set"
    external create : int -> string = "caml_create_string"
    val make : int -> char -> string
    val copy : string -> string
    val sub : string -> int -> int -> string
    val fill : string -> int -> int -> char -> unit
    val blit : string -> int -> string -> int -> int -> unit
    val concat : string -> string list -> string
    val iter : (char -> unit) -> string -> unit
    val escaped : string -> string
    val index : string -> char -> int
    val rindex : string -> char -> int
    val index_from : string -> int -> char -> int
    val rindex_from : string -> int -> char -> int
    val contains : string -> char -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val uppercase : string -> string
    val lowercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    type t = string
    val compare : t -> t -> int
    external unsafe_get : string -> int -> char = "%string_unsafe_get"
    external unsafe_set : string -> int -> char -> unit
      = "%string_unsafe_set"
    external unsafe_blit : string -> int -> string -> int -> int -> unit
      = "caml_blit_string" "noalloc"
    external unsafe_fill : string -> int -> int -> char -> unit
      = "caml_fill_string" "noalloc"
    val compare_subs_ : string -> string -> int -> int -> int -> int -> bool
    val compare_subs : string -> string -> int -> int -> bool
    val startswith : string -> string -> bool
    val find_from : string -> string -> int -> int
    val find : string -> string -> int
    val get_string_between : string -> string -> string -> string
    val split : ?sidx:int -> string -> string -> string list
    val whitespace : char -> bool
    val ltrim : ?f:(char -> bool) -> string -> string
    val rtrim : ?f:(char -> bool) -> string -> string
    val trim : ?f:(char -> bool) -> string -> string
  end
module Newick :
  sig
    type identifier = string
    type distance = float
    type newick_leaf = identifier * distance
    type 'a tree =
      'a Bio_newick.tree =
        Leaf of 'a
      | Tree of ('a tree list * distance)
      | Newick of 'a tree list
    type newick_tree = newick_leaf tree
    val parse : char Stream.t -> newick_leaf tree
  end
