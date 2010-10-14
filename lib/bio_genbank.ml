open Ort

let gene_re = Str.regexp ".*CDS +\\([0-9]+\\)\\.\\.\\([0-9]+\\)$"

(*
 * This is an overly simplified function to load genbank files.  
 * this only loads a list of genes listed in the genbank file
 *)
let read fname =
  List.rev (Seq.fold
	      ~f:(fun a l ->
		if Str.string_match gene_re l 0 then
		  let s = int_of_string (Str.matched_group 1 l) - 1 in
		  let e = int_of_string (Str.matched_group 2 l) in
		  if (e - s) mod 3 = 0 then
		    (s, e)::a
		  else
		    a
		else
		  a)
	      ~init:[]
	      (Lazy_io.read_file_lines ~close:true (open_in fname)))

	 
