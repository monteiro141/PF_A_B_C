(**
@summary Findvalue encontra a moeda mais "alta" no array de moedas disponiveis que seja menor ou igual ao valor atual 
@param coins Array das moedas usadas
@param amountCoins Quantidade de moedas que se pode usar
@param value Valor recebido usado para encontrar a moeda mais próxima para o algoritmo guloso 
@param i indice do array de moedas
@return moeda que se pode usar para subtrair ao valor atual
*)
let rec findvalue coins amountCoins value i=
  match value with
  | _ when value >= coins.(i) ->  coins.(i)
  | _ -> findvalue coins amountCoins value (i-1)
;;

(**
@summary Algoritmo guloso pega sempre na moeda mais alta(que ao subtrair não dê numero negativo) de forma a subtrair pouco a pouco até chegar ao Zero
@param coins Array das moedas usadas
@param amountCoins Quantidade de moedas que se pode usar
@param value Valor recebido usado para encontrar a moeda mais próxima para o algoritmo guloso 
@param n Numero de moedas atual
@return devolve N -> numero de moedas usadas para chegar ao zero
*)
let rec greedy coins amountCoins value n=
  match value with
  | 0 -> n
  | _ -> (greedy coins amountCoins (value-(findvalue coins amountCoins value (amountCoins-1))) (n+1))
;;



(**
@summary Algoritmo dinamico, o suposto deste algoritmo é usar arrays de forma a que o ultimo valor no array seja a quantidade de moedas usada para chegar ao zero
@param coins Array das moedas usadas
@param amountCoins Quantidade de moedas que se pode usar
@param value Valor recebido usado para encontrar a moeda mais próxima para o algoritmo dinamico 
*)
let dynProg coins amountCoins value =
  let arrayDyn = Array.make (value+1) max_int in
  arrayDyn.(0) <- 0;
    for i=1 to value do
      for j=0 to amountCoins-1 do
        if coins.(j) <= i then
          (
          if arrayDyn.(i-coins.(j)) != max_int && arrayDyn.(i) > arrayDyn.(i-coins.(j)) +1 
            then 
            (
            arrayDyn.(i) <- arrayDyn.(i-coins.(j)) +1
            )
          )
      done
    done;
    if arrayDyn.(value) = max_int then
      -1
    else
      arrayDyn.(value)
;;



(**
@summary Este é o main do programa, primeiro usa-se o count para ler o numero de moedas e em seguida lê-se essas moedas para um array Coins. Com este numero e array consegue-se usar para
determinar quantas moedas minimas são necessarias para fazer o algoritmo dinamico/guloso, no numero em que o algoritmo dinamico é mais eficiente que o guloso então 
mostra-se no ecrã esse numero, caso se chegue a um numero duas vezes maior que a moeda máxima sem falhar, então mostra-se YES no ecrã
*)
let count = read_int() in
  let coins = Array.make count 501 in
    for i=0 to count-1 do
      let coin = read_int() in
        coins.(i) <- coin
    done;
    let compare = ref (-1) in
    for i=1 to (2*coins.(count-1)) do
      let a = (greedy coins count i 0) and b = (dynProg coins count i) in
      if a != b && !compare == -1 then
        compare := i
    done;
    if !compare == -1 then
      Printf.printf "YES\n"
    else
      Printf.printf "%d\n" !compare
;;


(**
INPUT:
3
1
3
4

Valor: 6

Greedy [1,3,4] 3 6 0 : 
  Greedy  [1,3,4] 3 ( 6 - findvalue [1,3,4] 3 6 0 = 4 ) 0+1 :
    Greedy  [1,3,4] 3 ( 2 - findvalue [1,3,4] 3 2 0 = 1 ) 1+1 :
      Greedy  [1,3,4] 3 ( 1 - findvalue [1,3,4] 3 2 0 = 1 ) 2+1 
      Como o valor = 0 então retorna o N = 3

      
dynProg [1,3,4] 3 6:
  arrayDyn.(0) = 0
  i:1
    j:0
      coins.(0) 1 <= 1 ?
      arrayDyn.(1-coins.(0)) 0 != max_int e arrayDyn.(1) max_int > arrayDyn.(1-coins.(0)) 0 +1 ?
        arrayDyn.(1) <- arrayDyn.(1-coins.(0)(0)) +1
    j:1
      coins.(1) 3 <= 1 ?
    j:2
      coins.(2) 4 <= 1 ?
  i:2
    j:0
      coins.(0) 1 <= 2 ?
        arrayDyn.(2-coins.(0)) 1 != max_int e arrayDyn.(2) max_int > arrayDyn.(2-coins.(0)) 1 +1 ?
          arrayDyn.(2) <- arrayDyn.(2-coins.(0)(1)) +1
    j:1
      coins.(1) 3 <= 2 ?
    j:2
      coins.(2) 4 <= 2 ?
  i:3
    j:0
      coins.(0) 1 <= 3 ?
        arrayDyn.(3-coins.(0)) 2 != max_int e arrayDyn.(3) max_int > arrayDyn.(3-coins.(0)) 2 +1 ?
         arrayDyn.(3) <- arrayDyn.(3-coins.(0)(2)) +1
    j:1
      coins.(1) 3 <= 3 ?
        arrayDyn.(3-coins.(1)) 0 != max_int e arrayDyn.(3) 3 > arrayDyn.(3-coins.(1)) 0 +1 ?
          arrayDyn.(3) <- arrayDyn.(3-coins.(1)(0)) +1
    j:2
      coins.(2) 4 <= 3 ?
  i:4
    j:0
      coins.(0) 1 <= 4 ?
        arrayDyn.(4-coins.(0)) 1 != max_int e arrayDyn.(4) max_int > arrayDyn.(4-coins.(0)) 1 +1 ?
          arrayDyn.(4) <- arrayDyn.(4-coins.(0)(1)) +1
    j:1
      coins.(1) 3 <= 4 ?
        arrayDyn.(4-coins.(1)) 1 != max_int e arrayDyn.(4) 2 > arrayDyn.(4-coins.(1)) 1 +1 ?
    j:2
      coins.(2) 4 <= 4 ?
        arrayDyn.(4-coins.(2)) 0 != max_int e arrayDyn.(4) 2 > arrayDyn.(4-coins.(2)) 0 +1 ?
          arrayDyn.(4) <- arrayDyn.(4-coins.(2)(0)) +1
  i:5
    j:0
      coins.(0) 1 <= 5 ?
        arrayDyn.(5-coins.(0)) 1 != max_int e arrayDyn.(5) max_int > arrayDyn.(5-coins.(0)) 1 +1 ?
          arrayDyn.(5) <- arrayDyn.(5-coins.(0)(1)) +1
    j:1
      coins.(1) 3 <= 5 ?
        arrayDyn.(5-coins.(1)) 2 != max_int e arrayDyn.(5) 2 > arrayDyn.(5-coins.(1)) 2 +1 ?
    j:2
      coins.(2) 4 <= 5 ?
        arrayDyn.(5-coins.(2)) 1 != max_int e arrayDyn.(5) 2 > arrayDyn.(5-coins.(2)) 1 +1 ?
  i:6
    j:0
      coins.(0) 1 <= 6 ?
        arrayDyn.(6-coins.(0)) 2 != max_int e arrayDyn.(6) max_int > arrayDyn.(6-coins.(0)) 2 +1 ?
          arrayDyn.(6) <- arrayDyn.(6-coins.(0)(2)) +1
    j:1
      coins.(1) 3 <= 6 ?
        arrayDyn.(6-coins.(1)) 1 != max_int e arrayDyn.(6) 3 > arrayDyn.(6-coins.(1)) 1 +1 ?
          arrayDyn.(6) <- arrayDyn.(6-coins.(1)(1)) +1
    j:2
      coins.(2) 4 <= 6 ?
      arrayDyn.(6-coins.(2)) 2 != max_int e arrayDyn.(6) 2 > arrayDyn.(6-coins.(2)) 2 +1 ?

  arrayDyn.(value) = max_int ?
    retorna -1
*)