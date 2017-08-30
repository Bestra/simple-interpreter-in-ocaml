open Core.Std
open Core.Std.Result.Let_syntax

let is_term_op = function
  | Token.Operator.Plus
  | Token.Operator.Minus -> false
  | Token.Operator.Mult
  | Token.Operator.Div -> true

let print_rule rule_name tokens =
  print_endline rule_name;
  print_endline (List.to_string ~f:(Token.to_string) tokens)

let empty tokens =
  Ok(Ast.NoOp, tokens)

(*
   statement: compound_statement | assignment_statement | empty
*)
let rec statement tokens =
  print_rule "statement" tokens;
  match tokens with
  | Token.Begin :: _ ->
    compound_statement tokens
  | Token.Id _ :: _ ->
    assignment_statement tokens
  | _ -> empty tokens

(*
statement_list : statement | statement SEMI statement_list
*)
and statement_list tokens =
  print_rule "statement_list" tokens;
  let rec eat_list statements tokens =
    match tokens with
    | Token.Semi :: tl ->
      let%bind (s, ts) = statement tl in
      eat_list (List.append statements [s]) ts
    | _ -> Ok (statements, tokens)
  in
  let%bind (s, tl) = (statement tokens) in
  let%bind (statements, ts) = (eat_list [s] tl) in
  (match ts with
   | Token.Id _ :: _ -> Error "can't have an id after statements"
   | _ -> Ok (statements, ts))

(*
assignment_statement : variable ASSIGN expr
*)
and assignment_statement tokens =
  print_rule "assignment_statement" tokens;
  let%bind (left, tl) = variable tokens in
  (match tl with
   | Token.Assign as token :: the_rest ->
     let%map (right, tls) = expr the_rest in
     (Ast.Assign {left; right; token}, tls)
   | _ -> Error "variable needs to have an assign afterwards")

(*
   variable: ID
*)
and variable tokens =
  print_rule "variable" tokens;
  match tokens with
  | (Token.Id value  as token) :: tl -> Ok (Ast.Var {token; value}, tl)
  | _ -> Error "next token should be an Id"

(*
block: declarations compound_statement
*)
and block tokens =
  print_rule "block" tokens;
  let%bind (decs, tl) = declarations tokens in
  let%bind (statement, tl') = compound_statement tl in
  Ok (Ast.Block {declarations = decs; compound_statement = statement}, tl')

(*
declarations: VAR (variable_declaration SEMI)+
              | empty
*)
and declarations tokens =
  print_rule "declarations" tokens;
  let rec parse_declarations (token_list: Token.t list) (acc: Ast.t list) =
    (match token_list with
     | Token.Id _ :: tl ->
       let%bind (new_declarations, tl') = variable_declaration tl in
       parse_declarations tl' (List.append new_declarations acc)
     | _ -> Ok (acc, token_list)
    )
  in
  match tokens with
  | Token.Var :: tl -> parse_declarations tl []
  | _ -> Error "fixme"

(*
variable_declaration : ID (COMMA ID)* COLON type_spec
*)
and variable_declaration tokens : (Ast.t list * Token.t list, string) Core.Std.Result.t =
  print_rule "declarations" tokens;
  match tokens with
  | (Token.Id first_id as first_token) :: tl ->
    let rec eat_ids t acc =
      (match t with
       | Token.Comma :: (Token.Id i as token) :: tl' ->
         eat_ids tl' ((Ast.Var {token; value = i}) :: acc)
       | _ -> (acc, t)
      ) in
    let (new_vars, tl') = eat_ids tl [Ast.Var {token = first_token; value = first_id}] in
    Ok (new_vars, tl')
  | _ -> Error "variable_declaration needs a id token"

(*
type_spec : INTEGER | REAL
*)
and type_spec = function
  | Token.IntegerType as token :: _ -> Ok (Ast.Type {token})
  | Token.RealType as token :: _ -> Ok (Ast.Type {token})
  | _ -> Error "incorrect token for AST.Type"

(*
compound_statement : BEGIN statement_list END
*)
and compound_statement tokens =
  print_rule "compound_statement" tokens;
  match tokens with
  | Token.Begin :: tl ->
    let%bind (nodes, ts) = statement_list tl in
     (match ts with
       | Token.End :: ts' ->
         Ok (Ast.Compound {children = nodes}, ts')
       | _ -> Error "compound statements must end with End")
  | some_token :: _ -> Error (Printf.sprintf "compound statements must begin with 'Begin', not %s" (Token.to_string some_token))
  | _ -> Error "empty list, or something else bad"
(*
program : PROGRAM variable SEMI block DOT
*)
and program = function
  | Token.Program :: tl ->
    (match%bind variable tl with
     | (Ast.Var v, tl') ->
       (match tl' with
        | Token.Semi :: tl'' ->
          (match%bind block tl'' with
           | (b, Token.Dot :: the_rest) ->
             Ok (Ast.Program {name = v.value; block = b}, the_rest)
           | _ -> Error "syntax error"
          )
        | _ -> Error "syntax error")
     | _ -> Error "syntax error")
  | _ -> Error "programs must start with PROGRAM"

(*
factor :  PLUS factor
        | MINUS factor
        | INTEGER_CONST
        | REAL_CONST
        | LPAREN expr RPAREN
        | variable
*)
and factor tokens =
  print_rule "factor" tokens;
  match tokens with
  | Token.Operator o :: tl when not (is_term_op o) ->
    let%map (new_factor, tl') = (factor tl) in
    (Ast.UnaryOp {token = o; expr = new_factor}, tl')
  | Token.IntegerConst i as t :: tl ->
    printf "factor %i" i;
    print_newline (); Ok (Ast.Number {token = t; value = i}, tl)
  | Token.LParen :: tl ->
    print_endline "found a left paren";
    (match expr tl with
     | Ok (res, Token.RParen :: ts) ->
       Ok (res, ts)
     | Ok (_, hd :: _) -> Error (Printf.sprintf "next token %s is not an right paren" (Token.to_string hd))
     | Ok (_, []) -> Error "The list of tokens is empty"
     | Error _ as e -> e
    )
  | Token.Id _ :: _ -> variable tokens
  | hd :: _ -> Error (Printf.sprintf "next token %s is not part of a factor" (Token.to_string hd))
  | [] -> Error "no remaining tokens to factor"

(*
term : factor ((MUL | DIV) factor)*
*)
and term tokens =
  print_rule "term" tokens;
  let term' node t =
    match t with
    | Token.Operator o :: tl when is_term_op o ->
      printf "term op %s" (Token.Operator.to_string o); print_newline();
      let%map (next_factor_result, ts) = factor tl in
      (Ast.BinOp {token = o; left = node; right = next_factor_result}, ts)
    | _ -> Ok (node, t)
  in
  let%bind (n, t) = factor tokens in term' n t

(*
expr: term ((PLUS|MINUS) term)*
*)
and expr tokens =
  print_rule "expr" tokens;
  let rec expr' node t =
    match t with
    | Token.Operator o :: tl when not (is_term_op o) ->
      printf "expr op %s" (Token.Operator.to_string o); print_newline();
      let%bind (next_term_result, ts) = term tl in
      expr' (Ast.BinOp {token = o; left = node; right = next_term_result}) ts
    | _ :: _ -> Ok (node, t)
    | [] -> Ok (node, [])
  in
  let%bind (ast, remaining_tokens) = term tokens in
  expr' ast remaining_tokens

let parse text =
  printf "you input %s" text;
  print_newline ();
  let tokens = (Token.remove_whitespace (Lexer.tokenize text)) in
  program tokens
