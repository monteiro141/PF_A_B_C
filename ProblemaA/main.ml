let rec fortytwo i counting=
    if(i >= 42) then(
      if(i = 42) then (
        0
      )else if ((i mod 2) = 0 && (fortytwo (i/2) counting) < counting && i > 42) then (
        1 + (fortytwo (i/2) counting)
      ) else if (((i mod 3 = 0) || (i mod 4 = 0)) && (i mod 10) <> 0 && ((i/10) mod 10) <> 0 && (fortytwo (i - ((i mod 10) * ((i/10) mod 10))) counting) < counting && i > 42) then (
        1 + (fortytwo (i - ((i mod 10) * ((i/10) mod 10))) counting)
      ) else if ((i mod 5 = 0) && (fortytwo(i-42) counting) < counting && i > 42) then (
        1 + (fortytwo(i-42) counting)
      ) else (
        counting
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


