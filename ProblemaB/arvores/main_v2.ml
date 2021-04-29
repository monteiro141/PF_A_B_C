(*Struct da arvore onde pode ser um node com 4 ramos ou uma folha W | B com um int profundidade*)
type color = W | B (* W: White, B: Black *)
type image = Leaf of color * int (* leaf of one color and int depth*)
           | Node of image * image * image * image  (* node with four children  NW NE SW SE nesta ordem*) 
let mytree = Leaf (W,0);;
      

(*Aux functions*)
(*Função auxiliar para ajudar a calcular o resultado final na matriz thumbnail. Se, por exemplo, a matriz for 2x2 então cada 0 equivale a -1 e cada 1 equivale a 1.
Caso a soma deles seja maior ou igual a zero quer dizer que há mais 1's que 0's*)
let oneOrZero a =
  if a = W then
    -1
  else
    1
;;
(*Função auxiliar para mudar o numero da matriz para uma folha W ou B*)
let wOrB a =
  if a = 1 then
    B
  else
    W
;;
(*Função auxiliar usada na matrix to tree para saber se as 4 folhas a ser comparadas são iguais para formar uma folha "maior", caso contrario mantem se o node*)
let treeNodetoLeaf a =
  match a with
  | Leaf(a,b) -> oneOrZero a
  | Node(a,b,c,d) -> 0
;;

(*Função auxiliar usada para calcular se a matriz thumbnail vai ter 1 ou 0 na posição onde é chamada*)
let rec changeNodeToLeaf tree =
  match tree with
  | Leaf (a,b) -> b
  | Node(a,b,c,d) -> changeNodeToLeaf a + changeNodeToLeaf b + changeNodeToLeaf c + changeNodeToLeaf d 
;;

(*Função auxiliar logx e log2 usadas para calcular o numero de cortes na matriz thumbnail*)
let log x = 
  Stdlib.log x
;;

let log2 x = 
  log x /. log 2.
;;


(*Função auxiliar usada na folha mais alta, o valor menor de a, b, c e d vai ser retornado*)
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
(**Mostrar a matriz
@param m Matriz a ser mostrada no output
@param n Tamanho da matriz*)
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
(** Função recebe uma matriz criada a partir do input e transforma numa árvore
@param matrixOriginal Matriz criada a partir do input
@param size Tamanho da matriz atual, vai ser dividida em metade até chegar a [1,1], esse valor vai ser implementado numa arvore onde, recursivamente, vai ser transformado em 
folhas maiores ou nodes
@param tree Arvore original, começa em apenas Leaf(w,1) e é transformada numa outra arvore recurvisamente
@return Arvore final*)
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
(** Função que vai apanhar a folha mais alta, recursivamente. Esta função tem 2 condições, seja folha: devolve folha; ou seja Node: compara qual das 4 folhas/nodes do mesmo node é mais alto
@param tree Arvore a ser analisada
@param divisoes Numero de divisoes atual, incrementa quando chega a um node de forma a ter uma recursiva terminal
@return Numero de divisoes mais baixo, ou seja, folha mais alta
*)
let rec checkHigherLeaf tree divisoes =
  match tree with
  | Leaf(a,b) -> divisoes
  | Node (a,b,c,d) -> compareFourVariables (checkHigherLeaf a (divisoes+1)) (checkHigherLeaf b (divisoes+1)) (checkHigherLeaf c (divisoes+1)) (checkHigherLeaf d (divisoes+1))
;;
(*
-------------------------Step 4: Count leafs-------------------------
*)
(** Função para contar folhas de uma arvore, caso chegue a uma folha retorna 1, caso contrario vai dividindo a arvore e soma todas as recursivas
@param tree Arvore a ser analisada
@return Numero de folhas
*)
let rec countLeafs tree =
  match tree with
  | Leaf(a,b) -> 1
  | Node(a,b,c,d) -> (countLeafs a) + (countLeafs b) + (countLeafs c) + (countLeafs d)
;;
(*
-------------------------Step 5: Thumbnail-------------------------
*)
(**Função para transformar a árvore numa matriz thumbnail, vai dividindo em 4 quadrantes NW NE SW SE, quando chega ao numero maximo de cortes a função começa a comparar as folhas ou nodes
da mesma. À medida que vai dividindo, os offsetJ e offsetL vai incrementando consoante o quadrante que lhe, ao completar os cortes esses offsetJ e offsetL são as coordenadas na matriz thumbnail
para o resultado da transformação da arvore para matriz
@param tree Arvore a ser analisada
@param sizeThumbnail Tamanho da matriz thumbnail, usado para criar a matriz final
@param numberOfDivisions Valor para controlar quando se deve continuar a cortar ou começar a pegar nos valores da arvore e passar para matriz thumbnail
@return Matriz thumbnail
*)
let matrixToThumbnail tree sizeThumbnail numberOfDivisions =
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
        showMatrix (matrixToThumbnail mytree sizeForThumbnail (int_of_float (log2 (float_of_int sizeForThumbnail)))) sizeForThumbnail
;;

(**
INPUT:
P1
2 2
0 0
1 0
1

Ouput:
1
2
0

Simulação:
ler os valores -> 2 2 para size e size2
0 0 1 0 para uma matriz 2x2
transformar a matriz em árvore:
  arvore = matrixToTree matriz tamanho(2) arvore
            ->  tamanho = 1? Não
                então matrixToTree nw, matrixToTree ne, matrixToTree sw, matrixToTree se
                  matrixToTree nw:
                    tamanho = 1? Sim
                    então nw(a,b) -> a é W ? Sim então Leaf (W,-1)
                  matrixToTree ne:
                    tamanho = 1? Sim
                    então ne(a,b) -> a é W ? Sim então Leaf (W,-1)
                  matrixToTree sw:
                    tamanho = 1? Sim
                    então sw(a,b) -> a é W ? Não então Leaf (B,1)
                  matrixToTree se:
                    tamanho = 1? Sim
                    então se(a,b) -> a é W ? Sim então Leaf (W,-1)
                nw = sw = se = ne = W?Não então
                nw = sw = se = ne = B?Não então
                node(nw,ne,sw,se)
  
contar folha mais alta:
  folha = checkHigherLeaf tree divisoes(0) 
        -> tree é folha? devolve divisões
        -> tree é node? devolve minimo dos checkHigherLeaf nw checkHigherLeaf ne checkHigherLeaf sw checkHigherLeaf se

        -> Node(Leaf(W,-1),Leaf(W,-1),Leaf(B,1),Leaf(W,-1)) é leaf? não então
        minimo de checkHigherLeaf Leaf(W,-1) 0+1  checkHigherLeaf Leaf(W,-1) 0+1 checkHigherLeaf Leaf(B,1) 0+1 checkHigherLeaf Leaf(W,-1) 0+1
          checkHigherLeaf Leaf(W,-1) 1:
            Leaf(W,-1) é leaf? Sim então devolve 1
          checkHigherLeaf Leaf(W,-1) 1:
            Leaf(W,-1) é leaf? Sim então devolve 1
          checkHigherLeaf Leaf(B,1) 1:
            Leaf(B,1) é leaf? Sim então devolve 1
          checkHigherLeaf Leaf(W,-1) 1:
            Leaf(W,-1) é leaf? Sim então devolve 1  
        minimo de 1 1 1 1 ? 1
  folha = 1

contar folhas:
  folhas = countLeafs tree
        -> tree é folha? devolve 1
        -> tree é node? devolve soma dos countLeafs nw countLeafs ne countLeafs sw countLeafs se

        -> Node(Leaf(W,-1),Leaf(W,-1),Leaf(B,1),Leaf(W,-1)) é leaf? não então
        soma de countLeafs Leaf(W,-1)  countLeafs Leaf(W,-1) countLeafs Leaf(B,1) countLeafs Leaf(W,-1)
          countLeafs Leaf(W,-1):
            Leaf(W,-1) é leaf? Sim então devolve 1
          countLeafs Leaf(W,-1):
            Leaf(W,-1) é leaf? Sim então devolve 1
          countLeafs Leaf(B,1):
            Leaf(B,1) é leaf? Sim então devolve 1
          countLeafs Leaf(W,-1):
            Leaf(W,-1) é leaf? Sim então devolve 1  
        soma de 1 1 1 1 ? 4
  folhas = 4

matriz final thumbnail:
thumbnail = matrixToThumbnail arvore 1 log2(1)
          -> matrix = Array.make_matrix 1 1 2:
              printmytree arvore n(0) offsetJ(0) offsetL(0)
                1 < 1? Não
                então
                  matrix.(offsetJ).(offsetL) <- 1 se changeNodeToLeaf arvore >= 0 senão 0
thumbnail = 0


*)              