type formula = TRUE
            | FALSE
            | NOT of formula
            | ANDALSO of formula * formula
            | ORELSE of formula * formula
            | IMPLY of formula * formula
            | LESS of expr * expr

and  expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr

let rec exp : expr -> int = fun expression ->
        match expression with
        | NUM i -> i
        | PLUS (i1, i2) -> (exp i1) + (exp i2)
        | MINUS (i1, i2) -> (exp i1) - (exp i2)

let rec cal : formula -> bool = fun form -> 
        match form with 
        | TRUE -> true
        | FALSE -> false
        | NOT f -> not (cal f)
        | ANDALSO (f1, f2) -> (cal f1) && (cal f2)
        | ORELSE (f1, f2) -> (cal f1) || (cal f2)
        | IMPLY (f1, f2) -> ((cal f1) && (cal f2)) || not (cal f1)
        | LESS (e1, e2) -> (exp e1) < (exp e2)