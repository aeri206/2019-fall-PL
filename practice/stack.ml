module type StackSig = 
sig
	type 'a stack
	exception StackEmpty

	val emptyStack : 'a stack
	val push : 'a stack * 'a -> 'a stack
	val pop : 'a stack -> 'a * 'a stack
end

module Stack : StackSig =
struct
	type 'a stack = 'a list
	exception StackEmpty
	let emptyStack = []
	let push (stk, itm) = itm::stk
	let pop stk =
		match stk with
		  (itm::stack) -> (itm, stack)
		| [] -> raise StackEmpty
end