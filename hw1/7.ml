type expr = NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list

let rec eval : expr -> int = fun ex ->
    match ex with
    | NUM e1 -> e1
    | PLUS (e1, e2) -> (eval e1) + (eval e2)
    | MINUS (e1, e2) -> (eval e1) - (eval e2)
    | MULT (e1, e2) -> (eval e1) * (eval e2)
    | DIVIDE (e1, e2) -> (eval e1) / (eval e2)
    | MAX li -> begin
        match li with 
        | [] -> 0
        | [exp] -> eval exp
        | _ -> let list_cal = List.map eval li in
                List.hd (List.sort (fun x y -> y-x) list_cal)
        end
    
(*
ᆯ 정의하세요. 이때, MAX [NUM 1, NUM 3, NUM 2] = 3, 즉
MAX는 정수식 리 스트에서 가장 큰 정수를 찾아내는 정수식입니다. 
빈 리스트의 경우는 0을 의 미하는 정수식 입니다. 
*)

(* List.map (eval) li;;
let*)