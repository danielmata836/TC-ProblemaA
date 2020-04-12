open Scanf
open Array

let d = read_line();;
let g = read_line();;
let length_d = String.length d;;
let length_g = String.length g;;
let i = 0;;
let j = 0;;
let ht = Hashtbl.create 100;;
(*let tab=make_matrix (length_d+1) (length_g+1) 0;;*) (*matriz para os números identificadores de cada campo*)
(*coloca os números de 0 a lenth na linha e coluna 0

(*matriz para os números identificadores de cada campo*)
let aux =ref 0 in 
  for i=1 to length_d do 
    for j=1 to length_g do (*NÂO ESTÁ A FUNCIONAR*)
      aux:=!aux+1;
      tab.(i).(j) <- !aux 
    done
  done
*)
let makeMatrix a b =
  Array.init a (fun i -> 
  Array.init b (fun j -> 
  if i = 0 then j else
  if j = 0 then i else
  i * b + (j-1)  +1))

let tab = makeMatrix (length_d+1) (length_g+1);;
(* Imprimir a tabela -- teste*)
(*let print ch = ch |> Array.iter (Array.iter print_int) in
print tab; *)

let rec levDist i j =
  let w = tab.(i).(j) in
  try Hashtbl.find ht w 
  with Not_found ->
  if i=0 || j = 0 then let valor1= (max i j) in (Hashtbl.add ht w valor1;valor1) else (* w guarda o identificador da célula que agrega o i e j num só número *)
  if d.[i] = g.[j] then let valor2= (levDist (i-1) (j-1)) in (Hashtbl.add ht w valor2;valor2) (* adiciona na hashtable, o valor2 com a key w *)
  else let valor3 = min ((levDist i (j-1))+1) (min((levDist(i-1) (j-1))+1) ((levDist i (j))+1)) 
  in (Hashtbl.add ht w valor3; valor3)
in

let c = levDist (length_d) (length_g) in
Printf.printf "%d\n" c;;
let print ch = ch |> Array.iter (Array.iter print_int) in
print tab;