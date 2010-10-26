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

type newick_leaf = (identifier * distance option)

type 'a tree = 
  | Leaf of 'a
  | Tree of ('a tree list * distance option)
  | Newick of 'a tree list

type newick_tree = newick_leaf tree

exception ParseError of string

let is_whitespace = flip List.mem [' '; '\t'; '\n']

let read_all_whitespace = Seq.drop_while ~f:is_whitespace



let rec read_tree s =
  let rec rt t s =
    let (e, ns) = read_entry s in
    let ns' = read_all_whitespace ns in
    match Seq.next ns' with
      | Some '(' ->
	let (nt, ns) = read_tree s in
	
      | Some ',' ->
	rt (e::t) ns'
      | Some x ->
	(t, [< 'x; ns' >])
  in
  rt [] s

let newick s =
  match Seq.next s with
    | Some '(' ->
      let (t, ns) = read_tree s in
      finish_newick t ns
    | Some x ->
      raise (ParseError (String.make 1 x))
    | None ->
      Newick []
      
let parse s =
  let (t, s) = read_tree (Tree []) s in
  

