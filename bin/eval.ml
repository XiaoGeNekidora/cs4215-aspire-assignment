open Aspire
open Aspire.Value
open Aspire.Core

(* NOTE: Some of these functions should raise an EvalExn for some inputs.  No
other exceptions should be raised. *)
exception EvalExn of string

type evaluated_binding = Ident.t * Value.t
(* Adds a name and value to an environment.

Hint: Use ConsEnv. *)
let env_update (name: Ident.t) (value: Value.t) (env: env): env =
  ConsEnv (name, value, env)

let rec env_update_all (bindings: evaluated_binding list) (env: env): env = 
  match bindings with
  | [] -> env
  | a :: l -> env_update_all l (env_update (fst a) (snd a) env)

(** Merges two environments, with bindings from the second environment shadowing those from the first*)
let rec merge_env (env1: env) (env2: env): env =
  match env2 with
  | EmptyEnv -> env1
  | ConsEnv (n, v, rest) -> merge_env (env_update n v env1) rest

(* Looks up a name in an env and returns its corresponding Value.t.  Returns
None if the name is not present.

Hint: When comparing the name against an Ident.t, use `=`. *)
let rec env_lookup (name: Ident.t) (env: env): Value.t option =
  match env with
  | EmptyEnv -> None
  | ConsEnv (n, v, rest) -> if n = name then Some v else env_lookup name rest

(* Evaluate a binop applied to two floats.  May raise EvalExn on division by
zero.

Hint: For compaing a float with zero, use `=`. *)
let eval_binop (op: Untyped.binop) (x: float) (y: float): float =
  match op with
  | Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> if y = 0.0 then raise (EvalExn "Division by zero") else x /. y


(* Evaluate an expression in a given environment.  EvalExn on errors. *)
let rec eval_expr (e: Untyped.expr) (env: env): Value.t =
  match e with
  | Float f -> Float f
  | BinOp (op, e1, e2) ->
    (
      match (eval_expr e1 env, eval_expr e2 env) with
      | (Float f1, Float f2) -> Float (eval_binop op f1 f2)
      | _ -> raise (EvalExn "Binary operation on non-float values")
    )
  | If0 (cond, tc, fc) -> 
    (
      match eval_expr cond env with 
      | Float f -> if f=0.0 then eval_expr tc env else eval_expr fc env
      | res -> raise (EvalExn ("Non-number in if0: "^Value.show res))
    )
  | Let (bindings, expr) -> 
    (
      let f_eval_binding (b: binding): evaluated_binding =
        (let (name, code) = b in
        (name, eval_expr code env))
      in
      let evaluated_bindings = List.map f_eval_binding bindings in
      let new_env = env_update_all evaluated_bindings env in
      eval_expr expr new_env
    )
  | Var id -> 
    (
      match env_lookup id env with 
      | Some v -> v
      | None -> raise (EvalExn ("Undefined variable: " ^ (Ident.show id)))
    )
  | Fun (params, body) -> 
    (
      Closure (params, body, env)
    )
  | App (func, args) ->
    (
      match eval_expr func env with
      | Closure (params, body, f_env) ->
        (
          if List.length params <> List.length args then
            raise (EvalExn ("Function expected " ^ string_of_int (List.length params) ^ " arguments but got " ^ string_of_int (List.length args)))
          else
            let evaluated_args = List.map (fun arg -> eval_expr arg env) args in
            let new_f_env = env_update_all (List.combine params evaluated_args) f_env in
            eval_expr body new_f_env
        )
      | _ -> raise (EvalExn "Attempting to call a non-function value")
    )
(* Evaluate an entire program.

Hint: You do not need to change the code below, only the code above. *)
let eval (p: Untyped.prog) : Value.t Value.result =
  try Value (eval_expr p EmptyEnv)
  with EvalExn msg -> ValueError msg
