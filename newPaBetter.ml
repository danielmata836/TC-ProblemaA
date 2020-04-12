let d = read_line()
let g = read_line()

let print ch = ch |> Array.iter (Array.iter print_int);; 
let min3 a b c = min a (min b c)
 
let tab =Array.make_matrix ((String.length d)+1) ((String.length g)+1) 0;;

let rec levDist i j = 
    for a = 0 to i do
        tab.(a).(0) <- a
    done;
    for b = 0 to j do
        tab.(0).(b) <- b
    done;
    for b=1 to j do
        for a=1 to i do
            if d.[a-1] = g.[b-1] then tab.(a).(b) <- tab.(a-1).(b-1)
            else tab.(a).(b) <- (min3 tab.(a).(j-b) tab.(a-1).(b-1) (tab.(a-1).(b))) + 1 
        done;
    done;
;;
levDist (String.length d) (String.length g);;

print tab;;
print_string "\n";;
let () = Printf.printf "%d\n" tab.(String.length d).(String.length g) 
