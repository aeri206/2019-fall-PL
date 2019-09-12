module type IntStackSig = 
sig
	type intstack
	exception IntStackEmpty

	val emptyStack :intstack
	val push : intstack * int -> intstack
	val pop : intstack -> int * intstack
end

module IntStack : IntStackSig =
struct
	type intstack = int list
	exception IntStackEmpty
	let emptyStack = []
	let push (stk, itm) = itm::stk
	let pop stk =
		match stk with
		  (itm::stack) -> (itm, stack)
		| [] -> raise IntStackEmpty
end