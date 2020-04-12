(*Após termos submetido uma primeira versão onde implementámos Memoização através de uma tabela de Hash, 
que surpreedentemente deu "Time Limit Exceded", tentámos fazer uma versão recursiva deste programa. Mas 
não conseguímos, por isso decidímos enviar também esta.

Referências para as duas soluções submetidas:
  https://rosettacode.org/wiki/Levenshtein_distance#OCaml
*)

let d = read_line()
let g = read_line()

let min3 a b c = min a (min b c)
 
let tab = Array.make_matrix ((String.length d)+1) ((String.length g)+1) 0;;

let rec levDist i j = 
    for b=0 to j do
        for a=0 to i do
          if a = 0 || b = 0 then tab.(a).(b) <- max a b else
          if d.[a-1] = g.[b-1] then tab.(a).(b) <- tab.(a-1).(b-1)
          else tab.(a).(b) <- (min3 tab.(a).(b-1) tab.(a-1).(b-1) (tab.(a-1).(b))) + 1 
      done;
    done;
;;
levDist (String.length d) (String.length g);;

let () = Printf.printf "%d\n" tab.(String.length d).(String.length g) 
