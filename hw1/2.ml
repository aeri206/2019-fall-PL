let rec iter (n, f) i =
  if n == 0 then (fun x -> x) i
  else if n == 1 then f i
  else iter (n-1, f) (f i)