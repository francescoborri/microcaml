(* types *)

type scoping = Static | Dynamic
type call = ByName | ByValue
type options = { scoping : scoping; call : call }
type ident = string
type 't env = ident -> 't

type expr =
  (* primitive types *)
  | Int of int
  | True
  | False
  | String of string
  | Unit
  (* arithmetic operations *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  (* integer and boolan operations *)
  | Equal of expr * expr
  | LessThan of expr * expr
  | GreaterThan of expr * expr
  (* boolean operations *)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  (* string operations *)
  | Concat of expr * expr
  (* flow control and declarations *)
  | Den of ident
  | IfThenElse of expr * expr * expr
  | Let of ident * expr * expr
  | LetRec of ident * ident * expr * expr
  | Fun of ident * expr
  | Call of expr * expr
  | Sequence of expr * expr
  | Print of expr

let rec string_of_expr = function
  | Int num -> string_of_int num
  | True -> "true"
  | False -> "false"
  | String str -> str
  | Unit -> "()"
  | Add (x, y) -> "(" ^ string_of_expr x ^ " + " ^ string_of_expr y ^ ")"
  | Sub (x, y) -> "(" ^ string_of_expr x ^ " - " ^ string_of_expr y ^ ")"
  | Mul (x, y) -> "(" ^ string_of_expr x ^ " * " ^ string_of_expr y ^ ")"
  | Div (x, y) -> "(" ^ string_of_expr x ^ " / " ^ string_of_expr y ^ ")"
  | Mod (x, y) -> "(" ^ string_of_expr x ^ " mod " ^ string_of_expr y ^ ")"
  | Equal (x, y) -> "(" ^ string_of_expr x ^ " = " ^ string_of_expr y ^ ")"
  | LessThan (x, y) -> "(" ^ string_of_expr x ^ " < " ^ string_of_expr y ^ ")"
  | GreaterThan (x, y) ->
      "(" ^ string_of_expr x ^ " > " ^ string_of_expr y ^ ")"
  | And (x, y) -> "(" ^ string_of_expr x ^ " and " ^ string_of_expr y ^ ")"
  | Or (x, y) -> "(" ^ string_of_expr x ^ " or " ^ string_of_expr y ^ ")"
  | Not x -> "not " ^ string_of_expr x
  | Concat (x, y) -> "(" ^ string_of_expr x ^ " ^ " ^ string_of_expr y ^ ")"
  | Den ident -> ident
  | IfThenElse (x, y, z) ->
      "if " ^ string_of_expr x ^ " then " ^ string_of_expr y ^ " else "
      ^ string_of_expr z
  | Let (ident, x, y) ->
      "let " ^ ident ^ " = " ^ string_of_expr x ^ " in " ^ string_of_expr y
  | LetRec (func, arg, x, y) ->
      "let rec " ^ func ^ " = " ^ arg ^ " -> " ^ string_of_expr x ^ " in "
      ^ string_of_expr y
  | Fun (arg, x) -> arg ^ " -> (" ^ string_of_expr x ^ ")"
  | Call (x, y) -> "(" ^ string_of_expr x ^ " " ^ string_of_expr y ^ ")"
  | Sequence (x, y) -> "(" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ")"
  | Print x -> "print " ^ string_of_expr x

type evalue =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
  | Lazy of expr * evalue env
  | Closure of ident * expr * evalue env
  | RecClosure of ident * ident * expr * evalue env

let string_of_evalue = function
  | Int num -> string_of_int num
  | Bool b -> string_of_bool b
  | String str -> str
  | Unit -> "()"
  | Lazy (expr, _) -> "lazy (" ^ string_of_expr expr ^ ")"
  | Closure (arg, body, _) -> arg ^ " -> (" ^ string_of_expr body ^ ")"
  | RecClosure (func, arg, body, _) ->
      "rec " ^ func ^ " = " ^ arg ^ " -> (" ^ string_of_expr body ^ ")"

type evtype =
  | TInt
  | TBool
  | TString
  | TUnit
  | TClosure of evtype
  | TRecClosure of ident * evtype
  | TWildcard

let rec string_of_evtype = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TClosure evtype -> "_ -> (" ^ string_of_evtype evtype ^ ")"
  | TRecClosure (func, evtype) ->
      "rec " ^ func ^ " _ -> (" ^ string_of_evtype evtype ^ ")"
  | TWildcard -> "?"

(* enviroment *)

let bind (env : 'a env) (ide : ident) value ide' =
  if ide = ide' then value else env ide'

(* type-checking and errors *)

exception TypeError of string
exception RuntimeError of string

let type_error expected found =
  let rec string_of_evtypes = function
    | [] -> ""
    | [ evtype ] -> string_of_evtype evtype
    | evtype :: evtypes' ->
        string_of_evtype evtype ^ ", " ^ string_of_evtypes evtypes'
  in
  raise
    (TypeError
       ("Expected type " ^ string_of_evtypes expected ^ ", but found "
      ^ string_of_evtype found))

let runtime_error msg = raise (RuntimeError msg)

let rec evtypeof evalue =
  match evalue with
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Unit -> TUnit
  | Lazy (expr, env) -> infer expr (fun ident -> evtypeof (env ident))
  | Closure (_, expr, env) ->
      TClosure (infer expr (fun ident -> evtypeof (env ident)))
  | RecClosure (func, _, expr, env) ->
      TRecClosure (func, infer expr (fun ident -> evtypeof (env ident)))

and infer (expr : expr) env =
  match expr with
  | Int _ -> TInt
  | True -> TBool
  | False -> TBool
  | String _ -> TString
  | Unit -> TUnit
  | Add _ -> TInt
  | Sub _ -> TInt
  | Mul _ -> TInt
  | Div _ -> TInt
  | Mod _ -> TInt
  | Equal _ -> TBool
  | LessThan _ -> TBool
  | GreaterThan _ -> TBool
  | And _ -> TBool
  | Or _ -> TBool
  | Not _ -> TBool
  | Concat _ -> TString
  | Den ide -> env ide
  | IfThenElse (_, x, _) -> infer x env
  | Let (ide, x, y) -> infer y (bind env ide (infer x env))
  | LetRec (func, arg, x, y) ->
      let rec_env = bind env func (TRecClosure (func, infer x env)) in
      infer y (bind rec_env arg (infer x env))
  | Fun (_, x) -> TClosure (infer x env)
  | Call (x, y) -> (
      match (infer x env, infer y env) with
      | TClosure evtype, _ -> evtype
      | TRecClosure (_, evtype), _ -> evtype
      | evtype, _ ->
          type_error [ TClosure TWildcard; TRecClosure ("_", TWildcard) ] evtype
      )
  | Sequence (_, x) -> infer x env
  | Print x -> infer x env

let typecheck evtype evalue = evtype = evtypeof evalue

(* language primitives *)

let int_add x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' -> Int (x' + y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_sub x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' -> Int (x' - y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_mul x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' -> Int (x' * y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_div x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' ->
      if y' <> 0 then Int (x' / y') else runtime_error "Division by zero"
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_mod x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' ->
      if y' <> 0 then Int (x' mod y') else runtime_error "Division by zero"
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_equal x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' -> Bool (x' = y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_less_than x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' -> Bool (x' < y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let int_greater_than x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TInt, TInt, Int x', Int y' -> Bool (x' > y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let bool_and x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TBool, TBool, Bool x', Bool y' -> Bool (x' && y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let bool_or x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TBool, TBool, Bool x', Bool y' -> Bool (x' || y')
  | TInt, t, _, _ -> type_error [ TInt ] t
  | t, _, _, _ -> type_error [ TInt ] t

let bool_not x =
  match (evtypeof x, x) with
  | TBool, Bool x' -> Bool (not x')
  | t, _ -> type_error [ TBool ] t

let string_concat x y =
  match (evtypeof x, evtypeof y, x, y) with
  | TString, TString, String x', String y' -> String (x' ^ y')
  | TString, TInt, String x', Int y' -> String (x' ^ string_of_int y')
  | TString, TBool, String x', Bool y' -> String (x' ^ string_of_bool y')
  | TString, t, _, _ -> type_error [ TString ] t
  | t, _, _, _ -> type_error [ TString ] t

(* interpreter *)

let rec eval (expr : expr) (env : evalue env) ({ call; scoping } as options) :
    evalue =
  match expr with
  | Int num -> Int num
  | True -> Bool true
  | False -> Bool false
  | String str -> String str
  | Unit -> Unit
  | Den ident -> (
      match env ident with
      | Lazy (expr, env) -> eval expr env options
      | value -> value)
  | Add (x, y) -> int_add (eval x env options) (eval y env options)
  | Sub (x, y) -> int_sub (eval x env options) (eval y env options)
  | Mul (x, y) -> int_mul (eval x env options) (eval y env options)
  | Div (x, y) -> int_div (eval x env options) (eval y env options)
  | Mod (x, y) -> int_mod (eval x env options) (eval y env options)
  | Equal (x, y) -> int_equal (eval x env options) (eval y env options)
  | LessThan (x, y) -> int_less_than (eval x env options) (eval y env options)
  | GreaterThan (x, y) ->
      int_greater_than (eval x env options) (eval y env options)
  | And (x, y) -> bool_and (eval x env options) (eval y env options)
  | Or (x, y) -> bool_or (eval x env options) (eval y env options)
  | Not x -> bool_not (eval x env options)
  | Concat (x, y) -> string_concat (eval x env options) (eval y env options)
  | IfThenElse (x, y, z) -> (
      let condition = eval x env options in
      match (typecheck TBool condition, condition) with
      | true, Bool true -> eval y env options
      | true, Bool false -> eval z env options
      | _, _ -> type_error [ TBool ] (evtypeof condition))
  | Let (ident, expr, body) ->
      eval body (bind env ident (eval expr env options)) options
  | LetRec (func, arg, body, expr) ->
      eval expr (bind env func (RecClosure (func, arg, body, env))) options
  | Fun (arg, body) -> Closure (arg, body, env)
  | Call (func, expr) -> (
      let actual_arg =
        if call = ByValue then eval expr env options else Lazy (expr, env)
      in
      let closure = eval func env options in
      match closure with
      | Closure (arg, body, closure_env) ->
          let actual_env =
            bind (if scoping = Static then closure_env else env) arg actual_arg
          in
          eval body actual_env options
      | RecClosure (func, arg, body, closure_env) ->
          let rec_env =
            bind (if scoping = Static then closure_env else env) func closure
          in
          let actual_env = bind rec_env arg actual_arg in
          eval body actual_env options
      | _ ->
          type_error
            [ TClosure TWildcard; TRecClosure ("_", TWildcard) ]
            (evtypeof closure))
  | Sequence (x, y) ->
      let _ = eval x env options in
      eval y env options
  | Print expr ->
      let value = eval expr env options in
      print_endline (string_of_evalue value);
      value

let start expr options =
  eval expr (fun ident -> runtime_error ("Unbound variable" ^ ident)) options
