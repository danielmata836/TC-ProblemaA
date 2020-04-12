open Scanf
open Array

let d = read_line();;
let g = read_line();;
let length_d = String.length d;;
let length_g = String.length g;;
let m = make_matrix length_d length_g 0;;
let j = ref 1;;
let i = ref 1;;

for d = 0 to length_d-1 do
    m.(d).(0)<-d
done;
for g = 0 to length_g-1 do
    m.(0).(g)<-g 
done;

(* TODO: 
-A função só pode devolver um tipo de dados, neste caso unit
-erro na linha 29
*)


let rec table s t =
    (*if (!i, !j)=(length_d, length_g) then m.(!i).(!j) else*)
    (*if !j = length_d && !i <> length_g then j:=!j+1 else 
    if s.(!i) = s.(!j) then m.(!i).(!j) <- m.(!i-1).(!j-1) else
    if min !i !j = 0 then m.(!i).(!j) <- max !i !j else m.(!i).(!j) <- min (table m.(!i).(!j-1)+1) (min(table m.(!i-1).(!j-1)+1) (table m.(!i).(!j)+1))
  
in
table m.(i+1).(j) (*inicialização da recursão*)

let()=
    table m;;