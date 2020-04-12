open Scanf
open Array

let d = read_line();;
let g = read_line();;
let length_d = String.length d;;
let length_g = String.length g;;
let m = make_matrix length_d length_g 0;;

for d = 0 to length_d-1 do
    m.(d).(0)<-d
done;
for g = 0 to length_g-1 do
    m.(0).(g)<-g 
done;

let (i,j)= ref (0,0);
let rec table m = 
    (*if (i,j)=(length_d, length_g) then m.(i).(j) else(*devolve o tamanho final*)
    if m.(i).(0) = m.(0).(j) then 0 else (*se as letras nas posições i da string forem iguais*)
    (*início do algoritmo wiki*)
    if min i j = 0 then max i j else 
    let dist=ref 0 in (*declarar variável mutável*)
    dist:=min (table m.(i).(j-1)) (table m.(i-1).(j-1)+1) (*atribuir-lhe o valor que min devolver (segunda algoritmo da wiki*)
    dist:=dist+1 (*somar 1, uma vez que não consegui fazê-lo na linha a cima*)
    (min (table m.(i-1).(j)) dist)+1 (*como queremos o mínimo das 3 funções, temos de o fazer por 2 vezes. Min só aceita 2 parâmetros de cada vez*)
    (*incrementa as posições na tabela !!!Temos de corrigir de forma a correr todas as posições!!!*)
    if (!i = length_d && !j <> length_g) then (j:=!j+1) else (i:=!i+1)
in 
table m.(1).(1) (*inicialização da recursão*)
    
let findDist m = (*devolve a última posição *)
    if m.(length_d).(length_g) > 0 then m.(length_d).(length_g)
    else 0;;

let()=
    table m;;
    findDist m;;