type color = W | B (* W: White, B: Black *)
type image = Leaf of color * int (* leaf of one color *)
           | Node of image * image * image * image  (* node with four children  NW NE SW SE nesta ordem*) 
let mytree = Leaf (W,0);;
      

(*Aux functions*)
let oneOrZero a =
  if a = W then
    -1
  else
    1
;;

let wOrB a =
  if a = 1 then
    B
  else
    W
;;

let treeNodetoLeaf a =
  match a with
  | Leaf(a,b) -> oneOrZero a
  | Node(a,b,c,d) -> 0
;;

let rec changeNodeToLeaf tree =
  match tree with
  | Leaf (a,b) -> b
  | Node(a,b,c,d) -> changeNodeToLeaf a + changeNodeToLeaf b + changeNodeToLeaf c + changeNodeToLeaf d 
;;

let log x = 
  Stdlib.log x
;;

let log2 x = 
  log x /. log 2.
;;

let compareFourVariables a b c d = 
  if (a <= b && a <=c && a <=d) then a else if (b <= a && b <=c && b <=d) then b else if (c <= a && c <=b && c <=d) then c else d
;;   

(*------------------------------------------------------------------------------------------------------------*)
(*
-------------------------Step 1: Read input-------------------------
*)
(** 
@summary Criar matriz N por N e atribuir valor random a cada posição
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

(**
@summary Criar a matriz após ser dividida por quadrante
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

let showMatrix m n=
for j=0 to n-1 do
  for l=0 to n-1 do
      if l < n-1 then
      Printf.printf "%d " m.(j).(l)
      else
      Printf.printf "%d" m.(j).(l)
  done;
  Printf.printf "\n" 
done
;;

(*
-------------------------Step 2: Input matrix to tree-------------------------
*)
let rec matrixToTree matrixOriginal size tree = 
  if size = 1 then(
    Leaf(wOrB matrixOriginal.(0).(0),(if matrixOriginal.(0).(0) = 1 then 1 else ~-1))
  )
  else(
    let leafNW = matrixToTree (splitMatrix matrixOriginal size 1) (size/2) tree and
        leafNE = matrixToTree (splitMatrix matrixOriginal size 2) (size/2) tree and
        leafSW = matrixToTree (splitMatrix matrixOriginal size 3) (size/2) tree and
        leafSE = matrixToTree (splitMatrix matrixOriginal size 4) (size/2) tree in
        if treeNodetoLeaf leafNW  = 1 && treeNodetoLeaf leafNE = 1 && treeNodetoLeaf leafSW = 1 && treeNodetoLeaf leafSE = 1 then
          match leafNW with
          | Leaf(a,b) -> Leaf(a,4*b)
          |_ -> leafNW
        else if treeNodetoLeaf leafNW  = ~-1 && treeNodetoLeaf leafNE = ~-1 && treeNodetoLeaf leafSW = ~-1 && treeNodetoLeaf leafSE = ~-1 then
          match leafNW with
          | Leaf(a,b) -> Leaf(a,4*b)
          |_ -> leafNW
        else
        Node(leafNW,leafNE,leafSW,leafSE)
  )
;;
(*
-------------------------Step 3: Higher leaf-------------------------
*)
let rec checkHigherLeaf tree divisoes =
  match tree with
  | Leaf(a,b) -> divisoes
  | Node (a,b,c,d) -> compareFourVariables (checkHigherLeaf a (divisoes+1)) (checkHigherLeaf b (divisoes+1)) (checkHigherLeaf c (divisoes+1)) (checkHigherLeaf d (divisoes+1))
;;
(*
-------------------------Step 4: Count leafs-------------------------
*)
let rec countLeafs tree =
  match tree with
  | Leaf(a,b) -> 1
  | Node(a,b,c,d) -> (countLeafs a) + (countLeafs b) + (countLeafs c) + (countLeafs d)
;;
(*
-------------------------Step 5: Thumbnail-------------------------
*)
let matrixToThumbnail tree sizeThumbnail sizeOriginal numberOfDivisions=
let matrixThumbnail = Array.make_matrix sizeThumbnail sizeThumbnail 2 in
  let rec printmytree tree n offsetJ offsetL =
    if n < numberOfDivisions then(
      match tree with 
      | Leaf (a,b) -> 
        printmytree (Leaf (a,b)) (n+1) (offsetJ+offsetJ) (offsetL+offsetL);
        printmytree (Leaf (a,b)) (n+1) (offsetJ+offsetJ) (offsetL+offsetL+1);
        printmytree (Leaf (a,b)) (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL);
        printmytree (Leaf (a,b)) (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL+1)
      | Node (a, b, c, d) ->  
        printmytree a (n+1) (offsetJ+offsetJ) (offsetL+offsetL);
        printmytree b (n+1) (offsetJ+offsetJ) (offsetL+offsetL+1);
        printmytree c (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL);
        printmytree d (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL+1)
      )                              
    else(
      let currentLeaf = changeNodeToLeaf tree in
        if currentLeaf >= 0 then matrixThumbnail.(offsetJ).(offsetL) <- 1 else matrixThumbnail.(offsetJ).(offsetL) <- 0
      )
  in
  printmytree tree 0 0 0;

matrixThumbnail
;;




(*------------------------------------Main------------------------------------*)

let x = read_line ();;
let size = Scanf.scanf "%d " (fun x:int -> x) and size2 = Scanf.scanf "%d\n" (fun x:int -> x) in
  let m = (createMatrix ((size+size2)/2)) in
    let mytree = matrixToTree m size mytree in 
      let sizeForThumbnail = Scanf.scanf "%d\n" (fun x:int -> x) and 
        level = checkHigherLeaf mytree 0 and
        leafs = countLeafs mytree
      in
        Printf.printf "%d\n" level;
        Printf.printf "%d\n" leafs;
        showMatrix (matrixToThumbnail mytree sizeForThumbnail size (int_of_float (log2 (float_of_int sizeForThumbnail)))) sizeForThumbnail
;;
