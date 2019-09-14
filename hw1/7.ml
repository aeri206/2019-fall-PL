(* Dept of CSE, 2016-12805, Cho Aeri HW 1-7 *)

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
