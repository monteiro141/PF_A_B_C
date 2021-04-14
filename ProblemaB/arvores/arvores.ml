type color = W | B (* W: White, B: Black *)
type image = Leaf of color (* leaf of one color *)
           | Node of image * image * image * image  (* node with four children  NW NE SW SE nesta ordem*) 

let mytree = Node(
                    Node(Leaf(W),Leaf(W),Leaf(W),Leaf(W)),                      Node(Leaf(W),Leaf(B),Leaf(W),Leaf(B)),
                    
                    Node(Leaf(W),Leaf(W),Leaf(W),Leaf(B)),                      Node(Leaf(B),Leaf(B),Leaf(B),Leaf(B))
                  )
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

let oneOrZero a =
  if a = W then
    0
  else
    1
;;

let compareAllFourLeafs a b c d = 
  if ((oneOrZero a )+(oneOrZero b)) + (oneOrZero c) + (oneOrZero d) >= 2 then
    B
  else
    W
;;

let rec changeNodeToLeaf tree =
  match tree with
  | Leaf (a) -> a
  | Node (a,b,c,d) -> compareAllFourLeafs (changeNodeToLeaf a) (changeNodeToLeaf b) (changeNodeToLeaf c) (changeNodeToLeaf d)
;;

(**
| Leaf (a)          ->      if a = W then  matrixThumbnail.(offsetJ).(offsetL) <- 0  else  matrixThumbnail.(offsetJ).(offsetL) <- 1*)

let log x = 
  Stdlib.log x
;;

let log2 x = 
  log x /. log 2.
;;

let matrixToThumbnail tree sizeThumbnail =
let matrixThumbnail = Array.make_matrix sizeThumbnail sizeThumbnail 2 in
  let rec printmytree tree n offsetJ offsetL =
    if n < (int_of_float (log2 (float_of_int sizeThumbnail))) then(
      match tree with 
      | Leaf (a) -> printmytree (Leaf a) (n+1) (offsetJ+offsetJ) (offsetL+offsetL);
                    printmytree (Leaf a) (n+1) (offsetJ+offsetJ) (offsetL+offsetL+1);
                    printmytree (Leaf a) (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL);
                    printmytree (Leaf a) (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL+1)
      | Node (a, b, c, d) ->      printmytree a (n+1) (offsetJ+offsetJ) (offsetL+offsetL);
                                  printmytree b (n+1) (offsetJ+offsetJ) (offsetL+offsetL+1);
                                  printmytree c (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL);
                                  printmytree d (n+1) (offsetJ+offsetJ+1) (offsetL+offsetL+1)
      )                              
    else(
      let currentLeaf = changeNodeToLeaf tree in
        (*Printf.printf "Entrou final offsetJ: %d, offsetL %d\n " offsetJ offsetL;*)
        match currentLeaf with
        | W -> matrixThumbnail.(offsetJ).(offsetL) <- 0
        | B -> matrixThumbnail.(offsetJ).(offsetL) <- 1
      )
  in
  printmytree tree 0 0 0;

matrixThumbnail
;;




Printf.printf "\nMatriz final\n";;
showMatrix (matrixToThumbnail mytree 4) 4