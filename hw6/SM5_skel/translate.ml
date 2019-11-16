(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)
(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)
(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct
  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
	| K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
	| K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
	| K.LETV (x, e1, e2) ->
	  trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
	  trans e2 @ [Sm5.UNBIND; Sm5.POP]
	| K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
	| K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
	| K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
	| K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
	| K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
	| K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
	| K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
	| K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
	| K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
	| K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
	| K.NOT e -> trans e @ [Sm5.NOT]
	| K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
	| K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
	| K.IF (e_if, e_t, e_f) ->  trans e_if @ [Sm5.JTR (trans e_t, trans e_f)]
	| K.WHILE (e_if, e_do) -> 
	let f = "@function" in
	let arg = "#arguments" in
	let w_body = trans e_if @ [Sm5.JTR (trans e_do @ [Sm5.POP] @ [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans K.UNIT @ [Sm5.MALLOC; Sm5.CALL], trans K.UNIT)] in (* check if *)
	[Sm5.PUSH (Sm5.Fn (arg, [Sm5.BIND f] @ w_body)); Sm5.BIND f] @  (* LETF *)
	[Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans K.UNIT @ [Sm5.MALLOC; Sm5.CALL] @ (* CALL *)
	[Sm5.UNBIND; Sm5.POP]
	| K.LETF (id, arg, e1, e2) -> [Sm5.PUSH (Sm5.Fn (arg, [Sm5.BIND id] @ trans e1)); Sm5.BIND id] @ trans e2 @ [Sm5.UNBIND; Sm5.POP]
	| K.CALLV (id, arg) -> [Sm5.PUSH (Sm5.Id id); Sm5.PUSH (Sm5.Id id)] @ trans arg @ [Sm5.MALLOC; Sm5.CALL]
	| K.CALLR (id, ref) -> [Sm5.PUSH (Sm5.Id id); Sm5.PUSH (Sm5.Id id); Sm5.PUSH (Sm5.Id ref); Sm5.LOAD; Sm5.PUSH (Sm5.Id ref); Sm5.CALL]
	| K.WRITE e -> 
	let mem = "@newMem" in
	trans e @
		[Sm5.MALLOC; Sm5.BIND mem; Sm5.PUSH (Sm5.Id mem); Sm5.STORE] @
		[Sm5.PUSH (Sm5.Id mem); Sm5.LOAD;] @
		[Sm5.PUT; Sm5.PUSH (Sm5.Id mem); Sm5.LOAD; Sm5.UNBIND; Sm5.POP]
	| K.FOR (id, e1, e2, e_do) -> 
	let i_start = "#start_idx" in
	let i_end = "#end_idx" in
	let cond = K.NOT (K.LESS (K.VAR i_end, K.VAR i_start)) in
	let f_do = K.SEQ (K.ASSIGN (id, K.VAR i_start), K.SEQ (e_do, K.ASSIGN (i_start, K.ADD (K.VAR i_start, K.NUM 1)))) in
		trans (K.LETV (i_start, e1, K.LETV (i_end, e2, K.WHILE (cond, f_do))))
	| _ -> failwith "Unimplemented"
	end