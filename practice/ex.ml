(* no.1 : 변수 정의하기 *)
let i = 1
let s = "hello world"
let b = true
(* let _ = print_endline "hello world" (* unit *) *)

let i : int = 1
let s : string = "hello world"
let b : bool = true

(* 타입 알아서 확인해줌. 모듈, 레퍼런스 같은 경우 제외. 가독성을 위해서는 권장 *)

(* no2. 함수 정의하기 *)
let incr x = x+1
let y = incr 5


let incr_3 : int -> int = fun x -> x + 1
let incr_4 : float -> float = fun x -> x +. 1.0

(* let.. in ..*)

let echo : unit -> unit = fun ( ) ->
  let i = read_int() in
  let str = string_of_int i in
  print_endline("Your input : " ^ str)

let rec fact n = (* rec : recursive*)
  if n <= 0 then 1
  else n * fact (n-1)

let x = fact 10
let _ = print_endline (string_of_int x)

(*pair *)
let p : (int * string) = (1, "a")
let i = fst p
let s = snd p
let (ii, ss) = p

(* tuple *)
let t : (int * string * float) = (1, "a", 1.5)
let (i, s, f) = t
let (_, _, f) = t

(* list *)
let x : int list = 1 :: 2 :: 3 :: []
let xx : int list = [1;2;3]

let head_elem : int List.hd [1;2;3]
let tail_list : int list = List.tl [1;2;3]
let elem : int = List.nth [1;2;3] 1
if (List.mem 1 [1;2;3]) then ... else ... 

(* currying 
 두 함수의 타입은 다르다 !
*)
let sum = (x, y) = x+y (* (int * int ) -> int *)
let sum x y = x+ y (* int -> int -> int : int를 받고 int -> int 함수를 내놓음*)

(* inductive type *)
(* Leaf, Node는 constructor*)

type tree = Leaf of int
          | Node of int * tree * tree
let t1 : tree = Leaf 1
let t2 : tree = Node (1, Leaf 2, Leaf 3)

(* match - with 
케이스를 나누어서 계산 *)

let rec sum_of_tree : tree -> int = fun tree -> 
    match tree with
    |Leaf i -> int
    |Node (i, ltree, rtree) ->
        i + (sum_of_tree ltree) + (sum_of_tree rtree)
        (* 중첩시켜 쓸 경우 괄호 잘 써주기 *)

let rec list_len : 'a list -> int = fun l ->
    match l with
    |head :: tail -> 1 + lis_len tail
    |[] -> 0
(* list는 다형 타입 : polynomial type *)

(* try-with. raise 구문*)
let do_div : int -> int -> unit = fun x y ->
  try print_endline (string_of_int (x/y)) with
  Division_by_zero -> print_endline("Div by 0")

let f x =
  if x < 0 then
    raise(Failure "invalid input")
  else ...

(* module : 관련있는 코드들의 모음 
Signature : 모듈에서 드러내고 싶은 것만
Functor : 모듈을 받아서 모듈을 내놓는 것
*)

(* Reference *)

let int_ref = ref 0
let value_of_ref : int = !int_ref (* 값을 가져올때 ! 연산자 *)
let _ = int_ref := !int_ref + 1 (* i++, 새 값을 저장할때 := *)


(* sum.ml *)

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