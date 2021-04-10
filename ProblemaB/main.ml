type color = W | B (* W: White, B: Black *)
type image = L of color (* leaf of one color *)
           | N of image * image * image * image  (* node with four children *)
           

(* Criar matriz N por N e atribuir valor random a cada posição*)
let createMatrix n m =
    for j=0 to n-1 do
        for l=0 to n-1 do
            m.(j).(l) <- (Random.int 100)
        done
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

showMatrix (createMatrix 5 (Array.make_matrix 5 5 0)) 5;;

(*m.(x).(y) <- Aceder à pos (x,y) da matriz *)