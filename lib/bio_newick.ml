(*
 * Parses Newick style trees
 *)

type name = string
(* 
 * This should really be a float, but I don't care about
 * the actual value right now
 *)
type distance = string

type newick_leaf = (name * distance)

type 'a tree = 
  | Leaf 'a
  | Tree ('a tree list)

type newick_tree = newick_leaf tree



