type identifier = string
type distance = float
type newick_leaf = identifier * distance
type 'a tree =
    Leaf of 'a
  | Tree of ('a tree list * distance)
  | Newick of 'a tree list
type newick_tree = newick_leaf tree
val parse : char Stream.t -> newick_leaf tree
