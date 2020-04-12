open String
(* Implementação da WIKIPEDIA *)
(*À la C*)
let findMin x y z =
    if x<=y && x<=z then x else 
    if y<=x && y<=z then y else z

let findMin x y z = 
min x (min y z)

let rec findDist a b =
    let lenght_a=Array.length a in
    let length_b=Array.length b in 
    let m = Array.make lenght_a+1 (Array.make length_b+1 1)
  
    
    for j=0 to length_a+1 do
        m.(0).(j) <- j
    done;
    for i=0 to length_b do
        m.(i).(0) <- i
    done;

    for i=0 to lenght_a
----------------------------------------------
let meno = Hashtbl.create 100;;

let rec findDist a b = match (i,j) with
        try Hashtbl.find memo (i,j)
         with Not_found -> 
         | (i,0) -> i
      | (0,j) -> j
      | (i,j) ->
       

         if s.[i-1] = t.[j-1] then dist (i-1) (j-1)
         else let d1, d2, d3 = dist (i-1) j, dist i (j-1), dist (i-1) (j-1) in
         1 + min d1 (min d2 d3)
   in
   findDist (String.length s) (String.length t)
 
let test s t =
  Printf.printf " %s -> %s = %d\n" s t (levenshtein s t)
 
let () =
  test "kitten" "sitting";
  test "rosettacode" "raisethysword";