(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 Cho AERI 2016-12805 Dept of CSE
 *)

open M

type var = string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TVarEQ of var (* for M.EQ : Int, Bool, String, Loc *)
  | TVarWR of var (* for M.WRITE : Int, Bool, String *)
  

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

(* merge frv from two type *)
let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

(* frv(tau) / ftv(typEnv) *)
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v | TVarEQ v | TVarWR v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TVarEQ x'-> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
    | TVarWR x' -> 
    if (x = x') then (
      match t with
          | TVar x -> TVarWR x
          | TVarEQ x -> TVarWR x (* except loc *)
          | _ -> t
          ) 
          else t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> 
  SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let bind_tenv typeEnv id t = (* (M.id -> typ_scheme -> typ_env) -> typ_env *)
(id, t)::typeEnv

let rec lookup: var -> typ -> bool = fun v t -> (
  match t with
  | TLoc t -> lookup v t
  | TPair (f, s) -> (lookup v f) || (lookup v s)
  | TFun (a, b) -> (lookup v a) || (lookup v b)
  | TVar v' -> (v = v')
  | TInt | TBool | TString | TVarEQ _ | TVarWR _ -> false
)

let rec unify : typ -> typ -> subst = fun t1 t2 -> (
  if t1 = t2 then empty_subst
  else
  match (t1, t2) with 
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst
  | (TLoc t, TLoc t') -> unify t t'
  | (TPair (f, s), TPair (f', s')) -> (
    let ss = unify f f' in (* new substitition *)
    let ss' = unify (ss s) (ss s') in
    ss' @@ ss
  )
  | (TFun (a,b), TFun (a', b')) -> (
    let ss = unify a a' in
    let ss' = (unify (ss b) (ss b')) in
    ss' @@ ss
  )
  | (TVar v, t) | (t, TVar v)  -> (
    if lookup v t then raise (M.TypeError "Type Error occured!") 
    else make_subst v t
    )
  | (TVarEQ v, t) | (t, TVarEQ v) -> (
    if lookup v t then raise (M.TypeError "Type Error occured!") 
    else
      (
        match t with
        | TVarWR _ | TVarEQ _ | TInt | TBool | TString | TLoc _ -> make_subst v t
        | _ -> raise (M.TypeError "Type Error occured!")
      )
  )
  | (TVarWR v, t) | (t, TVarWR v) -> (
    if lookup v t then raise (M.TypeError "Type Error occured!") 
    else
      (
        match t with
        | TVarEQ _ | TInt | TBool | TString | TVarWR _  -> make_subst v t
        | _ -> raise (M.TypeError "Type Error occured!")
      )
  )
  |_ -> raise (M.TypeError "Type Error occured")
)
(* typ_scheme *)
let lookup_tenv typeEnv id = (* return typ_scheme *)
(* let _ = print_endline ("look-up" ^ id) in *)
  if List.mem_assoc id typeEnv
  then List.assoc id typeEnv
  else raise (M.TypeError ("unbound id: " ^ id))

let rec solve_w = fun (typeEnv, exp) ->  (* typ_env -> M.exp -> (subst * typ)  *)
(
  match exp with
  | M.CONST c-> 
    (
      match c with
      | M.S s -> 
      (empty_subst, TString)
      | M.N n -> 
      (empty_subst, TInt)
      | M.B b -> 
      (empty_subst, TBool)
    )
  | M.VAR x ->(
    let t = lookup_tenv typeEnv x in
    let t' = subst_scheme empty_subst t in (
      match t' with
      | SimpleTyp tau -> (empty_subst, tau)
      | GenTyp (_, tau) -> (empty_subst, tau)
    )
  )
  | M.LET (d, e2) -> (
    (* let _ = print_endline("let") in *)
    match d with
    | M.VAL (x, e1) -> 
    let rec expansive e = (
      match e with
      | M.CONST _ | M.VAR _ | M.FN _ | M.READ -> false
      | M.MALLOC _ | M.APP _ | M.ASSIGN _ | M.BANG _ -> true
      | M.LET (M.VAL (_, e1), e2) | M.BOP (_, e1, e2) | M.LET (M.REC (_, _, e1), e2)
      | M.PAIR (e1, e2) | M.SEQ (e1, e2) -> expansive e1 || expansive e2
      | M.IF (e1, e2, e3) -> expansive e1 || expansive e2 || expansive e3
      | M.FST e | M.SND e | M.WRITE e -> expansive e
    ) in
    if not (expansive(e1))
      then (
        (* not alloc memory -> generalize *)
      let (s1, tau1) = solve_w (typeEnv, e1) in
      let typeEnv' = subst_env s1 typeEnv in
      let scm = generalize typeEnv' tau1 in
      let typeEnv'' = bind_tenv typeEnv' x scm in
      let (s2, tau2) = solve_w (typeEnv'', e2) in
      (s2 @@ s1, tau2)
      )
      else (
        (* may alloc memory -> can not generalize *)
      let (s1, tau1) = solve_w (typeEnv, e1) in
      let typeEnv' = subst_env s1 typeEnv in
      let scm = SimpleTyp tau1 in
      let typeEnv'' = bind_tenv typeEnv' x scm in
      let (s2, tau2) = solve_w (typeEnv'', e2) in
      (s2 @@ s1, tau2)
      )
    | M.REC (f, x, e1) -> (
      let beta = TVar (new_var()) in
    let (s1, t1) = solve_w (bind_tenv typeEnv f (SimpleTyp beta), (M.FN (x, e1))) in
    let s2 = unify (s1 beta) t1 in
    let (s3, t3) = (s2 @@ s1, s2 t1) in
    let s3' = subst_env s3 typeEnv in
    let t3' = generalize s3' t3 in
    let (s4, t4) = solve_w (bind_tenv s3' f t3', e2) in
    (s4 @@ s3, t4)
    )
  )
  | M.FN (x, e) ->
  (* let _ = print_endline("fn") in *)
    let beta = TVar (new_var()) in
    let (s1, tau1) = solve_w ((bind_tenv typeEnv x (SimpleTyp beta)), e) in (* TODO : typeEnv+x:beta binding *)
    (s1, TFun (s1 beta, tau1))
  | M.APP (e1, e2) ->
  (* let _ = print_endline("app") in *)
    let (s1, tau1) = solve_w (typeEnv, e1) in
    let (s2, tau2) = solve_w (subst_env s1 typeEnv, e2) in (* TODO *)
    let beta = TVar (new_var()) in
    let s3 = unify (s2 tau1) (TFun (tau2, beta)) in
    (s3 @@ s2 @@ s1, s3 beta)
  | M.IF (e1, e2, e3) ->
  (* let _ = print_endline("if") in *)
    let (s1, tau1) = solve_w (typeEnv, e1) in
    let s1' = unify (s1 tau1) TBool in
    let (s2, tau2) = solve_w (subst_env (s1' @@ s1) typeEnv, e2) in
    let (s3, tau3) = solve_w (subst_env (s2 @@ s1' @@ s1) typeEnv, e3) in
    let s3' = unify (s2 tau2) (s3 tau3) in
    (s3' @@ s3 @@ s2 @@ s1' @@ s1, (s3 tau3))
  | M.BOP (op, e1, e2) -> (
    (* let _ = print_endline("bop") in *)
    let (s1, tau1) = solve_w (typeEnv, e1) in
    match op with 
    | M.ADD | M.SUB -> (
      let (s2, tau2) = solve_w (subst_env s1 typeEnv, e2) in
      (* let _ = print_endline("bop-add/sum") in *)
        let s = unify tau1 TInt in
        let s' = unify tau2 TInt in
        (s' @@ s @@ s2 @@ s1, TInt)
    )
    | M.AND | M.OR -> (
      let (s2, tau2) = solve_w (subst_env s1 typeEnv, e2) in
      (* let _ = print_endline("bop-and/or") in *)
        let s  = unify tau1 TBool in
        let s' = unify tau2 TBool in
        (s' @@ s @@ s2 @@ s1, TBool)
    )
    | M.EQ -> (
      (* let _ = print_endline("eq") in *)
      let beta = TVarEQ (new_var()) in
      let s1' = unify tau1 beta in
      let (s2, tau2) = solve_w ((subst_env (s1' @@ s1) typeEnv), e2) in
      let s2' = unify tau2 (s1' tau1) in
      (s2' @@ s2 @@ s1' @@ s1, TBool)
    )
  )
  | M.READ -> (empty_subst, TInt)
  | M.WRITE e ->
  (*let _ = print_endline("write") in *)
    let (s1, tau1) = solve_w (typeEnv, e) in
    let beta = TVarWR (new_var()) in
    let s2 = unify tau1 beta in
    (s2 @@ s1, tau1)
  | M.PAIR (e1, e2) -> 
  (* let _ = print_endline("pair") in *)
    let (s1, tau1) = solve_w (typeEnv, e1) in
    let (s2, tau2) = solve_w ((subst_env s1 typeEnv), e2) in
    (s2 @@ s1, TPair (s2 tau1, tau2))
  | M.FST e -> 
  (* let _ = print_endline("fst") in *)
    let beta1 = TVar (new_var()) in
    let beta2 = TVar (new_var()) in
    let (s1, tau1) = solve_w (typeEnv, e) in
    let s2 = unify tau1 (TPair (beta1, beta2)) in
    (s2 @@ s1, s2 beta1)
  | M.SND e -> 
  (* let _ = print_endline("snd") in *)
    let beta1 = TVar (new_var()) in
    let beta2 = TVar (new_var()) in
    let (s1, tau1) = solve_w (typeEnv, e) in
    let s2 = unify tau1 (TPair (beta1, beta2)) in
    (s2 @@ s1, s2 beta2)
  | M.SEQ (e1, e2) ->
  (* let _ = print_endline("seq") in *)
    let (s1, tau1) = solve_w (typeEnv, e1) in
    let (s2, tau2) = solve_w ((subst_env s1 typeEnv), e2) in
    (s2 @@ s1, tau2)
  | M.MALLOC e -> 
  (* let _ = print_endline("malloc") in *)
    let (s1, tau1) = solve_w (typeEnv, e) in
    (s1, TLoc tau1)
  | M.ASSIGN (e1, e2) -> 
  (* let _ = print_endline("assign") in *)
    let (s1, tau1) = solve_w (typeEnv, e1) in 
    let (s2, tau2) = solve_w (subst_env s1 typeEnv, e2) in
    let s3 = unify (s2 tau1) (TLoc tau2) in
    (s3 @@ s2 @@ s1, s3 tau2)
  | M.BANG e -> 
  (* let _ = print_endline("bang") in *)
    let (s1, tau1) = solve_w (typeEnv, e) in
    let beta = TVar (new_var()) in
    let s2 = unify tau1 (TLoc beta) in
    (s2 @@ s1, s2 beta)
  )

let check : M.exp -> M.typ = fun exp ->
  let (s, tau) = solve_w ([], exp) in
  let rec transMtypes : typ -> M.typ = fun t -> (* finally return type as used in M *)
    match t with 
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (t1, t2) -> M.TyPair (transMtypes t1, transMtypes t2)
    | TLoc t' -> M.TyLoc (transMtypes t')
    | _ -> raise (M.TypeError "Type Error occured") (* cannot decide final type *)
  in transMtypes tau
    