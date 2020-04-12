let d = read_line()
let g = read_line()

let print ch = ch |> Array.iter (Array.iter print_int);; 
let min3 a b c = min a (min b c)
 
let tab = Array.make_matrix ((String.length d)+1) ((String.length g)+1) 0;;

let rec levDist tab i j = 
    if i = 0 || j = 0 then tab.(i).(j) <- max i j else 
    if d.[i-1] = g.[j-1] then tab.(i).(j) <- tab.(i-1).(j-1) else 
    begin
    let val1 = tab.(i).(j-1) + 1 in
    let val2 = tab.(i-1).(j-1) + 1 in
    let val3 = tab.(i-1).(j) + 1 in
    let min = min3 val1 val2 val3 in
    if min = val1 then levDist tab i (j-1) else
    if min = val2 then levDist tab (i-1) (j-1) else
    levDist tab (i-1) j 
    end
in   
levDist tab (String.length d) (String.length g);;

print tab;;
print_string "\n";;
let () = Printf.printf "%d\n" tab.(String.length d).(String.length g) 
