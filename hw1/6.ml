(* Dept of CSE, 2016-12805, Cho Aeri HW 1-6 *)

type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2
            
let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (c1 , c2) ->
    match c1 with
    | NIL -> c2
    | ZERO (r1) ->
        (match c2 with
            | NIL -> ZERO (r1) 
            | ZERO (r2) -> ZERO (crazy2add(r1, r2))
            | ONE (r2) -> ONE (crazy2add(r1, r2))
            | MONE (r2) -> MONE (crazy2add(r1, r2)))
    | ONE (r1) -> 
        (match c2 with
            | NIL -> ONE (r1)
            | ZERO (r2) -> ONE (crazy2add(r1, r2))
            | ONE (r2) -> ZERO (crazy2add (crazy2add (r1, r2), ONE(NIL)))
            | MONE (r2) -> ZERO (crazy2add (r1, r2))
        )
    | MONE (r1) ->
        (match c2 with
            | NIL -> MONE (r1)
            | ZERO (r2) -> MONE (crazy2add (r1, r2))
            | ONE (r2) -> ZERO (crazy2add (r1, r2))
            | MONE (r2) -> ZERO (crazy2add(MONE(NIL), crazy2add(r1, r2)))
        )

    
	
   