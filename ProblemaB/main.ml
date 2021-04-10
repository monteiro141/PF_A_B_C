type color = W | B (* W: White, B: Black *)
type image = L of color (* leaf of one color *)
           | N of image * image * image * image  (* node with four children *)
           

(*
0 0 0 0 1 0 1 0
0 0 0 0 0 0 0 1
0 0 0 0 0 0 1 1
0 0 0 0 0 1 1 1
0 0 0 0 1 1 1 1
0 0 0 0 1 1 1 1
0 0 0 1 1 1 1 1
0 0 1 1 1 1 1 1
*)
(* Criar matriz N por N e atribuir valor random a cada posição
(Array.make_matrix size size 0)
*)

(**Acabar isto*)
let createMatrix n =
    let m = Array.make_matrix n n 0 in
        for j=0 to n-1 do
            for l=0 to n-1 do
                let x = Scanf.scanf "%d " x in
                    m.(j).(l) <- x
            done;
        done;
    m
;;

(*Mostrar cada posição da matriz *)
let showMatrix m n=
for j=0 to n-1 do
    for l=0 to n-1 do
        Printf.printf "%d " m.(j).(l)
    done;
    Printf.printf "\n" 
done
;;


(*Main*)
let size = 8  in
    let m = (createMatrix size) in
    showMatrix m size
;;



(*m.(x).(y) <- Aceder à pos (x,y) da matriz *)