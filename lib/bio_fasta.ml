(*pp camlp4o *)
open Ort
open Ort.Function

module String_ext = Bio_string_ext

(* A fasta sequence has a header and a sequence *)
type fasta = Header of string | Sequence of string

let rec read_fasta_sequence f curdata sin =
  if curdata = "" then
    match Seq.next sin with
      | Some d -> read_fasta_sequence f d sin
      | None -> [< >]
  else if String_ext.contains curdata '>' && String_ext.index curdata '>' > 0 then
    let idx = String_ext.index curdata '>' in
    let s = String_ext.sub curdata 0 (idx - 1) in
    let r = String_ext.sub curdata idx (String_ext.length curdata - idx) in
    [< 'Sequence s; f r sin >]
  else if String_ext.contains curdata '>' then
    [< f curdata sin >]
  else if String_ext.contains curdata '\n' then
    read_fasta_sequence f (String_ext.concat "" (String_ext.split curdata "\n")) sin
  else
    [< 'Sequence curdata; read_fasta_sequence f "" sin >]

let rec extract_header curdata sin =
  if String_ext.contains curdata '\n' then
    let idx = String_ext.index curdata '\n' in
    (String_ext.sub curdata 1 idx, 
     String_ext.sub curdata (idx + 1) (String_ext.length curdata - (idx + 1)))
  else
    match Seq.next sin with
      | Some d -> extract_header (curdata ^ d) sin
      | None -> (curdata, "")


(*
 * Expects to read either an empty line or a fasta header
 *)
let rec read_fasta_header curdata sin =
  let tcurdata = String_ext.trim curdata in
  if tcurdata <> "" && tcurdata.[0] = '>' then
    let (header, rest) = extract_header curdata sin in
    [< 'Header header; read_fasta_sequence read_fasta_header rest sin >]
  else
    match Seq.next sin with
      | Some d -> read_fasta_header (curdata ^ d) sin
      | None -> raise (Failure ("Unknown data: " ^ curdata))
    
    
(*
 * Reads from a stream.  It returns a lazy sequnece of 
 * Header or Sequence elements.  Throws an exception on bad input
 * The sequence will be returned in chunks until the stream is empty
 * or another Header is found
 *)
let read sin =
  read_fasta_header "" sin

let read_file ?(chunk_size = 1000) fname =
  read (Lazy_io.read_file_chunks ~close:true chunk_size (open_in fname))

let read_strict sin =
  let rec strict_sequence accum sin =
    match Seq.next sin with
      | Some (Header header) when Buffer.length accum = 0 ->
	[< 'Header header; strict_sequence (Buffer.create 400) sin >]
      | Some (Header header) ->
	let seq = Buffer.contents accum in
	[< 'Sequence seq; 'Header header; strict_sequence (Buffer.create 400) sin >]
      | Some (Sequence seq) -> begin
	Buffer.add_string accum seq;
	strict_sequence accum sin
      end
      | None when Buffer.length accum = 0 ->
	[< >]
      | None ->
	let seq = Buffer.contents accum in
	[< 'Sequence seq >]
  in
  read_fasta_header "" sin |> strict_sequence (Buffer.create 400)

let read_file_strict ?(chunk_size = 1000) fname =
  read_strict (Lazy_io.read_file_chunks ~close:true chunk_size (open_in fname))

let rec to_seq sin =
  match Seq.next sin with
      Some (Sequence d) ->
	[< Seq.of_string d; to_seq sin >]
    | Some (Header _) ->
	to_seq sin
    | None ->
	[< >]

