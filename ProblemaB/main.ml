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
(** Criar matriz N por N e atribuir valor random a cada posição
@param n Tamanho da matriz, passado como argumento no stdin
@return Devolve matriz criada a partir dos valores passados em stdin *)
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

(** Mostrar cada posição da matriz 
@param m Matriz para mostrar no stdout
@param n Tamanho da matriz
*)
let showMatrix m n=
for j=0 to n-1 do
    for l=0 to n-1 do
        Printf.printf "%d " m.(j).(l)
    done;
    Printf.printf "\n" 
done
;;


(*-----------Parte A----------------*)
(**Criar a matriz após ser dividida por quadrante
@param matrixOriginal Matriz que vai ser dividida para outra consoante o quadrante que lhe é passando
@param tamanhoOriginal Tamanho da matriz nova
@param inicialJ Posições em relação à matriz original usados para definir os offsets dos valores
@param inicialL Posições em relação à matriz original usados para definir os offsets dos valores
@return Matriz final repartida pelas posições passadas em argumento *)
let createMatrixByQuadrant matrixOriginal tamanho inicialJ inicialL =
    let m = Array.make_matrix tamanho tamanho 0 in
        for j=0 to tamanho-1 do
            for l=0 to tamanho-1 do
                m.(j).(l) <- matrixOriginal.(j+inicialJ).(l+inicialL)
            done
        done;
    m
;;

(** Dividir as matrizes
@param matrixOriginal Matriz que vai ser dividida para outra consoante o quadrante que lhe é passando
@param tamanhoOriginal Tamanho da matriz antes de ser dividido, usado para definir o quadrante NW SW NE SE
@param quadrante NW(1), NE(2), SW(3) e SE(4) 
@return Cria uma matriz consoante o quadrante passado em argumento*)
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

(** Função para contar a quantidade de leafs
@param matrix Matriz para comparar se é leaf ou node recursivamente
@param size Tamanho atual da matrix
@return Somatorio de todas as leafs
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
(*-----------Parte B----------------*)
let compareOnesAndZeros m size =
    let ones = ref 0 and zeros = ref 0 in
    for j=0 to size-1 do
        for l=0 to size-1 do
            if m.(j).(l) = 0 then
                zeros := !zeros +1
            else
                ones := !ones +1
        done
    done;
    if !ones >= !zeros then !ones else !zeros
;;

(** Transformar a matriz original para uma thumbnail
@param matrixOriginal matriz original
@param sizeOriginal Tamanho da matriz original
@param sizeThumbnail Tamanho da matriz thumbnail
@param numberOfDivisions Log2(Tamanho da matriz thumbnail)
@param currentDivision Divisão atual da matriz, começa a 0 e vai até ao numberOfDivisions  
@return Devolve uma matriz thumbnail final*)
(*
let matrixToThumbnail matrixOriginal sizeOriginal sizeThumbnail numberOfDivisions currentDivision offsetJ offsetL=
    let matrixThumbnail = Array.make_matrix sizeThumbnail sizeThumbnail 0 in
        let rec splitToThumbnail matrixOriginal numberOfDivisions currentDivision
;;
*)

(*-----------------Main-----------------*)
let size = 8  in
    let m = (createMatrix size) in
    Printf.printf "\nMatriz scanned:\n";
    showMatrix m size;
    let leafs = checkLeaf m size in
    Printf.printf "Leafs: %d\n" leafs;
    
;;