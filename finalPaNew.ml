open Scanf
open Array

let d = read_line()
let g = read_line()
    
let ht = Hashtbl.create 100

let min3 a b c = min a (min b c)

let rec levDist i j =
  try Hashtbl.find ht (i,j) 
  with Not_found ->
    if i=0 || j = 0 then
      let valor1 = (max i j) in
      (Hashtbl.add ht (i,j) valor1 ; valor1)
    else 
    if d.[i-1] = g.[j-1] then
      let valor2 = (levDist (i-1) (j-1)) in
      (Hashtbl.add ht (i,j) valor2;valor2)
    else let valor3 = min3 ((levDist i (j-1)) + 1) ((levDist (i-1) (j-1)) + 1) ((levDist (i-1) j) + 1) in
      (Hashtbl.add ht (i,j) valor3; valor3)

let () = Printf.printf "%d\n" (levDist (String.length d)  (String.length g))