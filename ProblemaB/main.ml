type color = W | B (* W: White, B: Black *)
type image = L of color (* leaf of one color *)
           | N of image * image * image * image  (* node with four children *)
           

(*
**Exemplo de Entrada**

P1
8 8
0 0 0 0 1 0 1 0
0 0 0 0 0 0 0 1
0 0 0 0 0 0 1 1
0 0 0 0 0 1 1 1
0 0 0 0 1 1 1 1
0 0 0 0 1 1 1 1
0 0 0 1 1 1 1 1
0 0 1 1 1 1 1 1
4


**Exemplo de Saída**

1
22
0 0 0 1
0 0 0 1
0 0 1 1 
0 1 1 1 


*)
(* Criar matriz N por N e atribuir valor random a cada posição
(Array.make_matrix size size 0)
*)
let createMatrix n =
    let m = Array.make_matrix n n 0 in
        for j=0 to n-1 do
            for l=0 to n-1 do
                if (l = (n-1)) 
                then(
                    let y = Scanf.scanf "%d\n" (fun x:int -> x) in
                    m.(j).(l) <- y
                    )
                else(
                    let y = Scanf.scanf "%d " (fun x:int -> x) in
                    m.(j).(l) <- y 
                    )                    
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

let createMatrixByQuadrant matrixOriginal tamanho inicialJ inicialL =
    let m = Array.make_matrix tamanho tamanho 0 in
        for j=0 to tamanho-1 do
            for l=0 to tamanho-1 do
                m.(j).(l) <- matrixOriginal.(j+inicialJ).(l+inicialL)
            done
        done;
    m
;;

(*Função para dividir as matrizes*)
let splitMatrix matrixOriginal tamanhoOriginal quadrante =
    match quadrante with
    |1 -> showMatrix (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) 0 0) (tamanhoOriginal/2)
    |2 -> showMatrix (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) 0 (tamanhoOriginal/2)) (tamanhoOriginal/2)
    |3 -> showMatrix (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) (tamanhoOriginal/2) 0) (tamanhoOriginal/2)
    |4 -> showMatrix (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) (tamanhoOriginal/2) (tamanhoOriginal/2)) (tamanhoOriginal/2)
    |_ -> Printf.printf "No quadrant.\n"
;;
(*
Função para comparar se a matriz é folha ou node
m é a matriz
*)
let checkForZerosAndOnes m =
    let x = m.(0).(0) in (* Primeiro elemento *)
        Array.for_all (fun a -> Array.for_all (fun a -> a = x) a) m
;;

(*Undone
let checkLeaf matrix size =
    if (checkForZerosAndOnes m) then
        1 (*Conta 1 folha*)
    else 

;;*)



(*Main*)
let size = 8  in
    let m = (createMatrix size) in
    Printf.printf "\nMatriz scanned:\n";
    showMatrix m size;
    Printf.printf "\nPrimeiro Quadrante:\n";
    splitMatrix m 8 1;
    Printf.printf "Segundo Quadrante:\n";
    splitMatrix m 8 2;
    Printf.printf "Terceiro Quadrante:\n";
    splitMatrix m 8 3;
    Printf.printf "Quarto Quadrante:\n";
    splitMatrix m 8 4
    
;;



(*m.(x).(y) <- Aceder à pos (x,y) da matriz *)