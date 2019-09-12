(*
 	hello.ml: Traditional Hello World Example (especially for C programmers)

	% ocamlc hello.ml
	% ./a.out
	Hello World!
	%
*)

let main () =
	let greetings = "Hello World!" in
	print_string greetings;
	print_newline ()

let _ = main ()

(*
	or as simple as

	let _ = print_endline "Hello World!"
*)