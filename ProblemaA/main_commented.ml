(**
@summary Função fortytwo conta o numero de passos minimos para o resultado 42 ou badluck! Caso a função retorne counting então é um mau caminho.
@param i Numero a comparar para chegar ao 42
@param counting Numero igual ao i original, usado para controlar.
@return int A ou B ou C, qual for o menor dos 3 senão retorna C
*)
let rec fortytwo i counting=
    if(i >= 42) then(
      if(i = 42) then (0) else 
      (
        let a = if ((i mod 2) = 0) then (1 + (fortytwo (i/2) counting)) else (counting)
        and
        b = if (((i mod 3 = 0) || (i mod 4 = 0)) && (i mod 10) <> 0 && ((i/10) mod 10) <> 0) then ( 1 + (fortytwo (i - ((i mod 10) * ((i/10) mod 10))) counting)) else (counting)
        and
        c = if ((i mod 5 = 0)) then ( 1 + (fortytwo(i-42) counting)) else (counting)
        in
        if(a <= b && a <= c) then (a)else if (b <= a && b <= c) then (b)else (c)
      )  
    )else(
      counting
    )

let x = read_int();;
let y = fortytwo x x;;
if(y <> x) then (
  print_int y;
  print_endline ""
)
else
  print_string "BAD LUCK\n"