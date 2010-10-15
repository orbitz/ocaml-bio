type fasta = Header of string | Sequence of string
val read : string Stream.t -> fasta Stream.t
val read_file : ?chunk_size:int -> string -> fasta Stream.t
val read_strict : string Stream.t -> fasta Stream.t
val read_file_strict : ?chunk_size:int -> string -> fasta Stream.t
