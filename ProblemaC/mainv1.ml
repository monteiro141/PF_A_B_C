let rec findvalue coins amountCoins value i=
  match value with
  | _ when value >= coins.(i) ->  coins.(i)
  | _ -> findvalue coins amountCoins value (i-1)
;;
(*Greedy*)
let rec greedy coins amountCoins value n=
  match value with
  | 0 -> n
  | _ -> (greedy coins amountCoins (value-(findvalue coins amountCoins value (amountCoins-1))) (n+1))
;;

(*Dynamic*)
let dynProg coins amountCoins value =
  let arrayDyn = Array.make (value+1) max_int in
  arrayDyn.(0) <- 0;
    for i=1 to value do
      for j=0 to amountCoins-1 do
        if coins.(j) <= i then
          if arrayDyn.(i-coins.(j)) != max_int && arrayDyn.(i) > arrayDyn.(i-coins.(j)) +1 then
            arrayDyn.(i) <- arrayDyn.(i-coins.(j)) +1
      done
    done;
    if arrayDyn.(value) = max_int then
      -1
    else
      arrayDyn.(value)
;;
(*Main*)
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