open Sexplib
open Aspire
open Aspire.Surface

(* NOTE: Some of these functions may raise a ParseExn.  No other exceptions
should be raised. *)

(* Convert an operator name to a `binop` *)
let parse_binop (s: string) : binop =
  match s with 
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> raise (ParseExn ("Unknown binary operator: " ^ s))

let make_ident (s: string) : Ident.t option =
  match float_of_string_opt s with
  | Some _ -> None
  | None ->
    if List.mem s ["+"; "-"; "*"; "/"; "let"; "if0"; "fun"]
    then None
    else Some (Ident.Ident s)

(* Raise a ParseExn if any of the idneitifiers are duplicates of each other.

Hint: Use List.exists and recursion *)
let rec check_identifiers (bindings: Ident.t list) : unit =
  match bindings with
  | [] -> ()
  | a :: l -> if List.exists (fun x -> x = a) l
              then raise (ParseExn ("Duplicate identifier: " ^ (Ident.show a)))
              else check_identifiers l 


(** Checks whether list has exactly size elements *)
let ensure_size (l: 'a list) (size: int) : unit = 
  if List.length l = size then () else raise (ParseExn ("Unexpected length of parameters: Expected " ^ string_of_int size))

(** Unwrap Atom --> String. Due to the error message should only be used in `fun` *)
let unwrap (l: Sexp. t) : string = 
  match l with 
  | Sexp.Atom s -> s
  | _ -> raise (ParseExn "Invalid argument format")

(* Parse an s-expression into an expr.

NOTE: use float_of_string_opt to distinguish an Atom that should be a float from
one that should be an identifier, then use make_ident to make identifiers *)
let rec parse_expr (sexp: Sexp.t) : expr =
  match sexp with 
  | Sexp.Atom s -> (* This handles float and identifier *)
    (
      match float_of_string_opt s with
      | Some f -> Float f
      | None -> 
        (
          match make_ident s with
          | Some id -> Var id
          | None -> raise (ParseExn ("Invalid identifier in variable reference: " ^ s))
        )
    )
  | List t -> 
    (
      match t with 
      | Sexp.Atom op :: l when List.mem op ["+"; "-"; "*"; "/"] -> (* This handles binop *)
        (
          match l with 
          | [e1; e2] -> 
            (
              let x1 = parse_expr e1 in
              let x2 = parse_expr e2 in
              BinOp (parse_binop op, x1, x2)
            )
          | _ -> raise (ParseExn ("Invalid binary operation format"))
        )
      | Sexp.Atom "if0" :: l -> 
        (
          match l with 
          | [cond; then_branch; else_branch] -> (
            let c = parse_expr cond in
            let t_b = parse_expr then_branch in
            let e_b = parse_expr else_branch in
            If0 (c, t_b, e_b)
          )
          | _ -> raise (ParseExn "Invalid if0 format")
        )
      | Sexp.Atom "let" :: l -> 
        (
          match l with 
          | [Sexp.List bindings; body] -> (
            let parsed_bindings = List.map parse_binding bindings in
            let idents = List.map (fun (name, _) -> name) parsed_bindings in 
            check_identifiers idents;
            Let (parsed_bindings, parse_expr body)
          )
          | _ -> raise (ParseExn "Invalid let format")
        )
      | Sexp.Atom "fun" :: l -> 
        (
          match l with
          | [Sexp.List params; body] ->
            (
              let make_ident_then_get (s: string) : Ident. t =
                match make_ident s with
                | Some id -> id
                | None -> raise(ParseExn ("Invalid identifier in function parameter:"^s)) in
              let param_names = List.map make_ident_then_get (List.map unwrap params) in
              check_identifiers param_names;
              Fun (param_names, parse_expr body)
            )
          | _ -> raise (ParseExn "Invalid fun format")
        )
      | _ -> match t with 
        | func :: args -> App (parse_expr func, List.map parse_expr args)
        | _ -> raise (ParseExn "Null expression")
    )

(* Parse an expression representing a binding  *)
and parse_binding (sexp: Sexp.t) : binding =
  match sexp with
  | Sexp.List [Sexp.Atom name; expr] ->
    (
      match make_ident name with
      | Some id -> (id, parse_expr expr)
      | None -> raise (ParseExn ("Invalid identifier in binding:" ^ name))
    )
  | _ -> raise (ParseExn "Invalid binding format")

(* Parse a list of tokens into a program.

Hint: You do not need to change the code below, only the code above. *)
let parse (tokens : Token.t Pos.t list) : prog =
  match tokens with
  | [sexp, _, _] -> parse_expr sexp (* Ignore Pos.t and list, they are framework artifacts *)
  | _ -> raise (ParseExn "Expected a single S-expression for the program")
