let rec fortytwo i counting=
    if(i >= 42) then(
      if(i = 42) then (
        print_string "Igual a 42";
        0
      )else (
        let a = 
        if ((i mod 2) = 0 && (fortytwo (i/2) counting) < counting) then (
          print_string "\nRegra 1 if - ";
          print_int i;
          1 + (fortytwo (i/2) counting)
        ) else (
          print_string "\nRegra 1 else - ";
          print_int i;
          counting
        )and
        b =
          if (((i mod 3 = 0) || (i mod 4 = 0)) && (i mod 10) <> 0 && ((i/10) mod 10) <> 0 && (fortytwo (i - ((i mod 10) * ((i/10) mod 10))) counting) < counting) then (
            print_string "\nRegra 2 if - ";
            print_int i;
          1 + (fortytwo (i - ((i mod 10) * ((i/10) mod 10))) counting)
        )else (
          print_string "\nRegra 1 else - ";
          print_int i;
          counting
        )and
          c =
          if ((i mod 5 = 0) && (fortytwo(i-42) counting) < counting) then (
            print_string "\nRegra 3 if - ";
            print_int i;
          1 + (fortytwo(i-42) counting)
        ) else (
          print_string "\nRegra 3 else - ";
          print_int i;
          counting
        )in
        if(a < b && a < c) then (
          print_string "\nretorna a - ";
          print_int a;
          a
        )else if (b < a && b < c) then (
          print_string "\nretorna b - ";
          print_int b;
          b
        )else (
          print_string "\nretorna c - ";
          print_int c;
          c
        )
      )  
    )else(
      print_string "Ultimo else\n";
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


