let tam  = int_of_string Sys.argv.(1) (* tamanho da imagem*)
let prop = int_of_string Sys.argv.(2)  (* de 0 a 100, indicação da proporção de branco *)


let () = Random.self_init () 


let gera_bit p =
  let v = Random.int 101 in
  if v>p then 1 else 0


let () = Printf.printf "%s\n%d %d\n" "P1" tam tam


let () = 
  for i=0 to tam - 1 do
    for j=0 to tam - 1 do
       Printf.printf (if j<tam-1 then "%d " else "%d") (gera_bit prop)
    done; print_newline ()
  done