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
(*
Criar matriz N por N e atribuir valor random a cada posição
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

(*
Mostrar cada posição da matriz 
*)
let showMatrix m n=
for j=0 to n-1 do
    for l=0 to n-1 do
        Printf.printf "%d " m.(j).(l)
    done;
    Printf.printf "\n" 
done
;;

(*
Função para criar a matriz após ser dividida por quadrante
-matrixOriginal-> Matriz que vai ser dividida para outra consoante o quadrante que lhe é passando
-tamanhoOriginal-> Tamanho da matriz nova
-inicialJ e inicialL-> Posições em relação à matriz original usados para definir os offsets dos valores
*)
let createMatrixByQuadrant matrixOriginal tamanho inicialJ inicialL =
    let m = Array.make_matrix tamanho tamanho 0 in
        for j=0 to tamanho-1 do
            for l=0 to tamanho-1 do
                m.(j).(l) <- matrixOriginal.(j+inicialJ).(l+inicialL)
            done
        done;
    m
;;

(*
Função para dividir as matrizes
-matrixOriginal-> Matriz que vai ser dividida para outra consoante o quadrante que lhe é passando
-tamanhoOriginal-> Tamanho da matriz antes de ser dividido, usado para definir o quadrante NW SW NE SE
-quadrante-> NW(1), NE(2), SW(3) e SE(4)
*)
let splitMatrix matrixOriginal tamanhoOriginal quadrante =
    match quadrante with
    |1 -> (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) 0 0)
    |2 -> (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) 0 (tamanhoOriginal/2))
    |3 -> (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) (tamanhoOriginal/2) 0) 
    |4 -> (createMatrixByQuadrant matrixOriginal (tamanhoOriginal/2) (tamanhoOriginal/2) (tamanhoOriginal/2)) 
    |_ -> (createMatrixByQuadrant matrixOriginal (tamanhoOriginal) 0 0) 
;;
(*
Função para comparar se a matriz é folha ou node
m é a matriz
*)
let checkForZerosAndOnes m =
    let x = m.(0).(0) in (* Primeiro elemento *)
        Array.for_all (fun a -> Array.for_all (fun a -> a = x) a) m
;;

(*
Função para contar a quantidade de leafs
*)
let rec checkLeaf matrix size =
    if (checkForZerosAndOnes matrix) then
        1 (*Conta 1 folha*)
    else 
        let nw = checkLeaf (splitMatrix matrix size 1 ) (size/2) and
        ne = checkLeaf (splitMatrix matrix size 2 ) (size/2) and
        sw = checkLeaf (splitMatrix matrix size 3 ) (size/2) and
        se = checkLeaf (splitMatrix matrix size 4 ) (size/2) in
        (nw + ne + sw + se)
;;

(*
Main
*)
let size = 8  in
    let m = (createMatrix size) in
    Printf.printf "\nMatriz scanned:\n";
    showMatrix m size;
    let leafs = checkLeaf m size in
    Printf.printf "Leafs: %d\n" leafs
    
;;

(*m.(x).(y) <- Aceder à pos (x,y) da matriz *)