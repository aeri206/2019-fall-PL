let rec sum_of_list  (l : int list) : int =
  match l with
  |[] -> 0
  |hd::tl -> hd + sum_of_list tl

let _ = (print_int (sum_of_list [1;2;3;4;5])); print_newline()

let test (f: 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline()))
    else ((print_string ("wrong answer")); print_newline())

let _ =
  let test_sum = test sum_of_list in
  (test_sum [1;2;3;4;5] 15);
  (test_sum [1;2;3;4;5;6;7;8;9;10] 55)