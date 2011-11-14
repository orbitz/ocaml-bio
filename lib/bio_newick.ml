(*pp camlp4o *)
(*
 * Parses Newick style trees
 *)

open Core_extended
open Core_extended.Std

open Ort
open Ort.Function

(*
 * Grammar:
 * newick     := '(' tree ')' ';'
 * tree       := entry ',' entry | entry
 * entry      : '(' tree ')' ':' distance | identifier ':' distance
 * distance   := floating number
 * identifier := alphanumeric
 *)


type identifier = string
(* 
 * This should really be a float, but I don't care about
 * the actual value right now
 *)
type distance = float

type newick_leaf = (identifier * distance)

type 'a tree = 
  | Leaf of 'a
  | Tree of ('a tree list * distance)
  | Newick of 'a tree list

type newick_tree = newick_leaf tree

let string_of_list l =
  let b = Buffer.create (List.length l) in
  l |> List.iter ~f:(Buffer.add_char b);
  Buffer.contents b

let is_digit = List.mem ~set:['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let rec newick = parser
  | [< _ = whitespace; ''('; _ = whitespace; t = tree; _ = whitespace; '')'; _ = whitespace; '';'; _ = whitespace >] -> Newick t
and tree = parser
  | [< e = entry; _ = whitespace; es = entry_aux >] -> e::es
and entry = parser
  | [< ''('; _ = whitespace; t = tree; _ = whitespace; '')'; '':'; d = distance; _ = whitespace >] -> Tree (t, d)
  | [< id = identifier; '':'; d = distance >] -> Leaf (id, d)
and entry_aux = parser
  | [< '','; _ = whitespace; e = entry; es = entry_aux >] -> e::es
  | [< >] -> []
and identifier s = s |> Seq.take_while ~f:((<>) ':') |> Seq.to_list |> string_of_list
and distance s = 
  let floating_point c = is_digit c || c = '-' || c = '.' || c = 'e' in
  s |> Seq.take_while ~f:floating_point |> Seq.to_list |> string_of_list |> float_of_string
and whitespace = 
  let is_whitespace = List.mem ~set:[' '; '\t'; '\n'] in
  Seq.drop_while ~f:is_whitespace

let parse s = newick s
