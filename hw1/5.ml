(* Dept of CSE, 2016-12805, Cho Aeri HW 1-5 *)

type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2

 let rec crazy2val : crazy2 -> int = fun cr ->
    match cr with
    | NIL -> 0
    | ZERO cr -> 2 * crazy2val cr
    | ONE cr -> 2 * crazy2val cr + 1
    | MONE cr -> 2 * crazy2val cr - 1