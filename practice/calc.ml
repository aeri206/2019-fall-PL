module type CalcSig = 
sig
	type exp = NUM of float
			 | PLUS of exp * exp
			 | MINUS of exp * exp
			 | MUL of exp * exp
			 | DIV of exp * exp
			 | SQRT of exp
			 | STORE of exp
			 | RESTORE
			 | SEQ of exp list

	val eval : exp -> float
end

module Calc : CalcSig =
struct
	type exp = NUM of float
			 | PLUS of exp * exp
			 | MINUS of exp * exp
			 | MUL of exp * exp
			 | DIV of exp * exp
			 | SQRT of exp
			 | STORE of exp
			 | RESTORE
			 | SEQ of exp list

	let s = ref 0.0

	let rec eval e =
		match e with
		  NUM f -> f
		| PLUS (e1, e2) -> eval e1 +. eval e2
		| MINUS (e1, e2) -> eval e1 -. eval e2
		| MUL (e1, e2) -> eval e1 *. eval e2
		| DIV (e1, e2) -> eval e1 /. eval e2
		| SQRT e -> sqrt (eval e)
		| STORE e -> 
			let v = eval e in
				s := v;
				v
		| RESTORE -> !s
		| SEQ [] -> 0.0
		| SEQ [x] -> eval x
		| SEQ (x::l) -> (eval x; eval (SEQ l))
end