(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

(* 

let rec print_type = fun x -> 
match x with 
    | TInt -> print_endline("int type")
    | TBool -> print_endline("bool type")
    | TString -> print_endline("string type")
    | TPair (t1, t2) -> print_endline("pair type")
    | TLoc t' -> print_endline("loc type")
    | TFun (t1, t2) -> print_endline("fun type")
    | TVar _ -> print_endline("var type")
   
   *)
type tyEqn = 
  | Eqn of typ * typ
  | SysEqn of tyEqn * tyEqn

type substitution = typ -> typ

let rec substitute_eq: substitution -> tyEqn -> tyEqn = fun ss eq -> (
  match eq with
  | Eqn (t, t') -> (
    let t1 = ss t in
    let t2 = ss t' in
    Eqn (t1, t2))
  | SysEqn (teq, teq') -> (
    SysEqn (substitute_eq ss teq, substitute_eq ss teq'))
)


let make_substitution: var -> typ -> substitution = fun x t -> 
   let rec subs t' =
    match t' with
    | TVar x' -> if x = x' 
    then t else t'
    | TInt | TBool | TString -> t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
  in subs


let rec lookup: var -> typ -> bool = fun v t -> (
  (* let _ = print_endline v in *)
  (* let _ = print_type t in *)
  match t with
  | TLoc t -> lookup v t
  | TPair (f, s) -> (lookup v f) || (lookup v s)
  | TFun (a, b) -> (lookup v a) || (lookup v b)
  | TVar v' -> (v = v')
  | TInt | TBool | TString -> false
)
let rec unify: typ -> typ -> substitution = fun t1 t2 -> (
  match (t1, t2) with 
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> (fun x -> x)
  | (TLoc t, TLoc t') -> unify t t'
  | (TPair (f, s), TPair (f', s')) -> (
    let ss = unify f f' in (* new substitition *)
    let ss' = unify (ss s) (ss s') in
    (fun x -> ss (ss' x))
  )
  | (TFun (a,b), TFun (a', b')) -> (
    let ss = unify a a' in
    let ss' = unify (ss b) (ss b') in
    (fun x -> ss (ss' x))
  )
  | (TVar v, t) | (t, TVar v)  -> (
    if lookup v t then raise (M.TypeError "Type Error occured!") 
    (* Var -> type is var does not occur before, if occured before : raise Error! *)
    else make_substitution v t
    )
  |_ -> raise (M.TypeError "Type Error occured")
)

let rec unify_all: tyEqn -> substitution -> substitution = fun eq ss -> (
  match eq with 
  | Eqn (t, t') ->  (* unify -all (t, t'), ss *)
  let u = unify t t' in
  (fun x -> u (ss x)) 
  | SysEqn (teq, teq') -> ( (* unify-all (u and u), ss *)
    let t = unify_all teq ss in (* new substitution *)
    let new_teq = substitute_eq t teq' (* new equation *) in
    unify_all new_teq t 
  )
)
 
let lookup_tenv typeEnv id =
  if List.mem_assoc id typeEnv
  then List.assoc id typeEnv
  else raise (M.RunError ("unbound id: " ^ id))

(* find id in typeEnv : id -> type. return id's type *)

let bind_tenv typeEnv id t =
  (id, t)::typeEnv
  (* add new binding id -> type in typeEnv *)

let var_equiv : typ list ref = ref [] (* int, bool, string, loc for M.EQ : should check! *)
let var_edit : typ list ref = ref [] (* int, bool, string for M.WRITE : should check! *)
(* 
let print_exp : M.exp -> _ = fun e -> (
match e with
    | M.CONST c -> print_endline("const")
    | M.VAR v -> print_endline("var")
    | M.FN (x, e)-> print_endline("fun")
    | M.APP (e1, e2) -> print_endline("app")
    | M.LET (d, e) -> print_endline("let")
    | M.IF (e1, e2, e3) -> print_endline("if")
     | M.BOP (b, e1, e2) -> print_endline("bop")
     | M.READ -> print_endline("read")
     | M.WRITE e -> print_endline("write")
     | M.MALLOC e -> print_endline("malloc")
     | M.ASSIGN (e1, e2) -> print_endline("assign")
     | M.BANG e -> print_endline("bang")
     | M.SEQ (e1, e2) -> print_endline("seq")
     | M.PAIR (e1, e2) -> print_endline("pair")
     | M.FST e -> print_endline("fst")
     | M.SND e -> print_endline("snd")
)

*)

let rec build_eq (typeEnv, exp, t) =  (* (TypeEnv, M.exp, typ) -> tyEqn *)
(* let _ = print_exp exp in *)
  match exp with
  | M.CONST c-> 
    (
      match c with
      | M.S s -> Eqn (t, TString)
      | M.N n -> Eqn (t, TInt)
      | M.B b -> Eqn (t, TBool)
    )
  | M.VAR x -> 
    let t' = lookup_tenv typeEnv x in Eqn (t, t')
  | M.FN (x, e) -> 
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    let u = Eqn (t, TFun (a1, a2)) in
    SysEqn (u, build_eq ((bind_tenv typeEnv x a1), e, a2))
  | M.APP (e1, e2) -> 
    let a = TVar (new_var ()) in
    let u = build_eq (typeEnv, e1, TFun(a, t)) in
    let u' = build_eq (typeEnv, e2, a) in
    SysEqn (u, u')
  | M.IF (e1, e2, e3) ->
    let u = build_eq (typeEnv, e1, TBool) in
    let u' = build_eq (typeEnv, e2, t) in
    let u'' = build_eq (typeEnv, e3, t) in
    SysEqn (u, SysEqn (u', u''))
  | M.BOP (op, e1, e2) ->
    (
      match op with 
       | M.ADD | M.SUB -> (
         let u = build_eq (typeEnv, e1, t) in
         let u' = build_eq (typeEnv, e2, t) in
         SysEqn (Eqn (t, TInt), SysEqn (u, u'))
       )
       | M.AND | M.OR -> (
         let u' = build_eq (typeEnv, e1, t) in
         let u'' = build_eq (typeEnv, e2, t) in
         SysEqn (Eqn (t, TBool), SysEqn (u', u''))
       )
       | M.EQ -> (
         let a = TVar (new_var ()) in
         let _ = var_equiv := a :: !var_equiv in
         (* check later : one of int, bool, str, loc -> iterate array*)
         let u = build_eq (typeEnv, e1, a) in
         let u' = build_eq (typeEnv, e2, a) in
         SysEqn(Eqn (t, TBool), SysEqn(u, u'))
       )
    )
  | M.READ -> Eqn (t, TInt)
  | M.WRITE e -> (
    let a = TVar (new_var()) in
    let _ = var_edit := a :: !var_edit in
    (* check later : one of int, bool, str *)
    let u = build_eq (typeEnv, e, a) in
    SysEqn(Eqn (t, a), u)
  )
  | M.PAIR (e1, e2) -> (
    let a = TVar (new_var ()) in
    let a' = TVar (new_var ()) in
    let u' = build_eq (typeEnv, e1, a) in (* pair.fst = a *)
    let u'' = build_eq (typeEnv, e2, a') in (* pair.snd = a' *)
    SysEqn (Eqn (t, TPair (a, a')), SysEqn (u', u'')) (* a -> a' *)
  )
  | M.FST e -> (
    let a = TVar (new_var ()) in (* a : snd is not important *)
    build_eq (typeEnv, e, TPair (t, a))
  )
  | M.SND e -> (
    let a = TVar (new_var ()) in (* a : fst is not important *)
    build_eq (typeEnv, e, TPair (a, t)) 
  )
  | M.SEQ (e1, e2) -> (
    let a = TVar (new_var ()) in
    let u = build_eq (typeEnv, e1, a) in (* result of first eq is not important *)
    let u' = build_eq (typeEnv, e2, t) in
    SysEqn (u, u')
  )
  | M.LET (d, e2) -> (
    match d with
    | M.REC (f, x, e1)-> (
      let a = TVar (new_var ()) in
      let a' = TVar (new_var ()) in
      let a'' = TVar (new_var ()) in
      let fn = TFun (a, a') in
      let u = build_eq ((bind_tenv typeEnv f fn), e2, a'') in
      let u' = build_eq ((bind_tenv typeEnv f fn),M.FN (x, e1), fn) in
      SysEqn (Eqn (t, a''), SysEqn (u, u'))
    )
    | M.VAL (x, e1) -> (
      let a = TVar (new_var ()) in
      let a' = TVar (new_var ()) in
      let u = build_eq (typeEnv, e1, a) in
      let u' = build_eq ((bind_tenv typeEnv x a), e2, a') in
      SysEqn (Eqn (t, a'), SysEqn (u, u'))
    )
  )
  | M.ASSIGN (e1, e2) ->
    let a = TVar (new_var ()) in
    let u = build_eq (typeEnv, e1, (TLoc a)) in
    let u' = build_eq (typeEnv, e2, a) in
    SysEqn (Eqn (t, a), SysEqn (u, u'))
  | M.BANG e -> 
    let a = TVar (new_var ()) in
    let u = build_eq (typeEnv, e, (TLoc a)) in
    SysEqn (Eqn (t, a), u)
  | M.MALLOC e -> (
    let a = TVar (new_var ()) in
    let u = build_eq (typeEnv, e, a) in
    SysEqn (Eqn (t, (TLoc a)), u)
  )

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let a = TVar (new_var ()) in
  let eq = build_eq ([], exp, a) in (* V : 연립방정식을 세우는 알고리즘 165p *)
  let u = unify_all eq (fun x -> x) in (* fun x -> x : 자기자신 TypeVar를 뱉는다 : 도움 안됨 = empty *)
  let rec check_edit : typ list -> _ = fun tVar -> 
  (match tVar with
  | [] -> ()
  | hd :: tl -> ( (* iterate type list and check each element is one of int, bool, str *)
    match u hd with
    | TInt | TBool | TString -> check_edit tl
    | _ -> raise (M.TypeError "Type Error occured!")
  )
  ) in
  let _ = check_edit !var_edit in
  let rec check_equiv : typ list -> _ = fun tVar -> (
    match tVar with (* iterate type list and check each element is one of int, bool, str, loc for equality *)
    | [] -> ()
    | hd :: tl -> (
      match u hd with
      | TInt | TBool | TString -> check_equiv tl
      | TLoc l -> check_equiv tl
      | _ -> raise (M.TypeError "Type Error occured")
    )
  )
  in let _ = check_equiv !var_equiv in
  let rec transMtypes : typ -> M.types = fun t -> (* finally return type as used in M *)
    match t with 
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (t1, t2) -> M.TyPair (transMtypes t1, transMtypes t2)
    | TLoc t' -> M.TyLoc (transMtypes t')
    | TFun (t1, t2) -> M.TyArrow (transMtypes t1, transMtypes t2)
    | TVar _ -> raise (M.TypeError "Type Error occured") (* cannot decide final type *)
  in transMtypes (u (u a))
  (* u : substitution, a : typeVar *)