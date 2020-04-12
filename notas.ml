(*let m = make_matrix length_d length_g 0;;*)
(*for d = 0 to length_d-1 do 
    m.(d).(0)<-d
done;
for g = 0 to length_g-1 do
    m.(0).(g)<-g 
done;*) (* Código pertencente a uma versão anterior do programa *)

(* TODO: -> DONE 
  -adicionar parâmetro a função Hashtabl para adicionar o valor na posição (i, j)
*)


(*CICLOS FOR ANINHADOS NÃO FUNCIONAM EM OCAML*)
(*Trying to print a matrix in ocaml*)
(*não funciona*)  
let print_tab tab length_d length_g=
  for i=1 to length_d do
    for j=1 to length_g do
      print_int tab.(i).(j)
    done
  done
(*correto*)
  let print ch = ch |> Array.iter (Array.iter print_int)
(*-----------------------------------------------------------*)
 
let i = ref 0 in
  let j = ref 0 in 
  let aux =ref 0 in 
let rec index tab=
  if !i < length_d then (aux:=!aux+1; i:=!i+1; tab.(!i).(!j) <- !aux) else 
  if !j < length_g then (i:=0;aux:=!aux+1; index tab.(i).(!j+1)
  in 
  index tab.(0).(0)



(*Soma 1 a cada elemento de um array*)
let _ =
  let a = [| 1 ; 2 ; 5 ; 7 ; 11 |];;
  let aux = ref 0 in
  Array.iter (fun x -> a.(!aux) <- x+1;aux:=!aux+1) a ;;

(*Soma o elemento antrior ao seguinte ----- NÂO FUNCIONA
OBJETIVO: resolver isto*)
let _ =
  let a = [| 0 ; 0; 0|] ;;
  let aux = ref 1 in
  Array.iter (fun x -> a.(!aux) <- x+;aux:=!aux+1) a ;;


  (*outro exemplo*)
  let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in
  let fold_left_sum = Array.fold_left
  (   fun x y ->
          Printf.printf "%4d %4d\n" x y ;
          x + y
          )
      0 a
  in
  Printf.printf "\nFold_left sum  : %d\n" fold_left_sum

  (*  2a tentativa  *)
  let a = [| 0 ; 0; 0|] ;;
  let aux = ref 0 in
  Array.fold_left (fun x y -> a.(!aux) <-x+y+1; aux:=!aux+1) 0 a;;
(*Array.init*)
let tab = Array.make_matrix 4 3 0 in
tab.(0)<- 0;;


let f i = i+1 in
tab.(i) <- Array.init 3 f;;
(*tab.(0) <- Arraytab.init 3 f;;*)

(*Solução para inicializar uma matriz com os índices numerados de 1 a length*length*)
let make_matrix a b =
    Array.init a (fun i -> 
    Array.init b (fun j -> i * b + j  +1))
;;

(*Notas finais:
  -em videochamada, o professor disse que não é necessário criar uma tabela 
  para indentificar as céluas. Por isso, a tabela tab com as fun aninhadas não é necessária.
  Basta usar (i,j) que funciona na mesma na tabela de Hash.
*)7
(* Função que faz duas tarefas mas devolve apenas um inteiro *)
let tab = Array.make_matrix 3 4 0;;
let teste i j = tab.(i).(j) <- 4 ; tab.(i).(j);;

(* Função recursiva particular*)
let rec fill s t =
  for i=0 to 3 do
    let v = t.(i) in
    let a,b =  ((v-1) / 7), ((v-1) mod 7) in
    s.(a).(b) <- true
  done