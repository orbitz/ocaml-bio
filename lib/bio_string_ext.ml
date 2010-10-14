include String
let rec compare_subs_ large s hl nl hidx nidx =
  if hidx < hl && nidx < nl && large.[hidx] = s.[nidx] then
    compare_subs_ large s hl nl (hidx + 1) (nidx + 1)
  else if nidx >= nl then
    true
  else
    false
      
let compare_subs large s hidx nidx =
  compare_subs_ large s (length large) (length s) hidx nidx
    
let startswith large s =
  compare_subs large s 0 0
    
    
(* Find a string *)
let rec find_from haystack needle sidx = 
  let idx = index_from haystack sidx needle.[0]
  in
  if compare_subs haystack needle idx 0 then
    idx
  else
    find_from haystack needle (idx + 1)
      
let find haystack needle =
  find_from haystack needle 0
    
let get_string_between haystack startstr endstr =
  let sidx = (find haystack startstr) + (length startstr)
  in
  (* Just incase startstr and endstr are the same, like '|' or '\t' *)
  sub haystack sidx ((find_from haystack endstr (sidx + 1)) - sidx)


(* Not sure if it is a good idea to expose sidx *)
let rec split ?(sidx = 0) str spl  =
  match try Some (find_from str spl sidx) with Not_found -> None with
      Some idx ->
	String.sub str sidx (idx - sidx) :: split str spl ~sidx:(idx + String.length spl)
    | None -> [String.sub str sidx (String.length str - sidx)]



let whitespace c = List.mem c [' '; '\t'; '\n']

let ltrim ?(f  = whitespace) s =
  let rec ltrim' i =
    if s <> "" then
      if f s.[i] then
	ltrim' (i + 1)
      else
	String.sub s i (String.length s - i)
    else
      s
  in
  ltrim' 0

let rtrim ?(f = whitespace) s = 
  let rec rtrim' i =
    if s <> "" then
      if f s.[i] then
	rtrim' (i - 1)
      else
	String.sub s 0 (i + 1)
    else
      s
  in
  rtrim' (String.length s - 1)


let trim ?(f = whitespace) s =
  ltrim ~f:f (rtrim ~f:f s)

