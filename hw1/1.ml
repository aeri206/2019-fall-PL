let rec sigma ((a, b, f): int * int * (int -> int)): int =
   if a > b then 0
   else sigma (a+1, b, f) + f a
