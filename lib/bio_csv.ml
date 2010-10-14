(*pp camlp4o *)
(*
 * Reads CSV files
 *)

open Ort

module String_ext = Bio_string_ext

type quoting = Quote_all | Quote_minimal | Quote_none

type state = { data : string
	     ; pos : int
	     ; stream : string Stream.t
	     }

type dialect = { delim : char
	       ; rowdelim : char
	       ; quoting : quoting
	       ; quotechar : char
	       ; ignorews : bool
	       }


let default_dialect = { delim = ','
		      ; rowdelim = '\n'
		      ; quoting = Quote_minimal
		      ; quotechar = '"'
		      ; ignorews = true
		      }


(* default chunk size when reading a csv in *)
let chunk_size = 4192


let ensure_data s y n=
  if s.pos >= String_ext.length s.data then
    match Seq.next s.stream with
	Some data ->
	  y {s with data = data; pos = 0}
      | None -> n s
  else
    y s

let incr s = {s with pos = s.pos + 1}

let char_at s = s.data.[s.pos]

let empty_stream _ = [< >]

let add_char b c = Buffer.add_char b c; b

let rev_some l = Some (List.rev l)

let identity x = x

let rec read_value dialect buf quoted s =
  let next s = 
    ensure_data 
      (incr s) 
      (read_value dialect (add_char buf (char_at s)) quoted) 
      (fun s -> 
	 if quoted then 
	   raise (Failure "Stream ended in middle of quoted column")
	 else
	   (Buffer.contents buf, s))
  in
  if quoted && char_at s = dialect.quotechar then
    let b = Buffer.contents buf in
    ensure_data
      (incr s)
      (fun s -> (b, s))
      (fun s -> (b, s))
  else if not quoted && (char_at s = dialect.delim || char_at s = dialect.rowdelim) then
    (Buffer.contents buf, s)
  else if char_at s = '\\' then
    ensure_data 
      (incr s) 
      next
      (fun _ -> raise (Failure "Stream ended while escaping a char"))
  else
    next s
    
let rec read_columns dialect columns s =
  let (column, s) = 
    if char_at s = dialect.quotechar then
      ensure_data 
	(incr s) 
	(read_value dialect (Buffer.create 100) true) 
	(fun _ -> raise (Failure "Stream ended in middle of quoted column"))
	
    else
      read_value dialect (Buffer.create 100) false s
  in
  let columns = column :: columns in
  ensure_data
    s
    (fun s ->
       if char_at s = dialect.rowdelim then
	 (List.rev columns, s)
       else
	 ensure_data (incr s) (read_columns dialect columns) (fun s -> (List.rev columns, s)))
    (fun s -> (List.rev columns, s))

let rec read_rows dialect s =
  let (columns, s) =
    if char_at s = dialect.rowdelim then
      ([""], s)
    else
      read_columns dialect [] s
  in
  [< 'columns; ensure_data (incr s) (read_rows dialect) empty_stream >]

let read_stream dialect sin =
  ensure_data {data = ""; pos = 0; stream = sin} (read_rows dialect) empty_stream
	
let read_file dialect fname =
  read_stream dialect (Lazy_io.read_file_chunks ~close:true chunk_size (open_in fname))
