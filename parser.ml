open Core.Std

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
  | Token.Begin :: tl ->
    compound_statement tokens
  | Token.Id _ :: tl ->
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
      (match statement tl with
       | Ok (s, ts) -> eat_list (List.append statements [s]) ts
       | Error _ as e -> e)
    | _ -> Ok (statements, tokens)
  in
  match statement tokens with
  | Ok (s, tl) ->
    (match eat_list [s] tl with
     | Ok (statements, ts) -> (match ts with
         | Token.Id i :: _ -> Error "can't have an id after statements"
         | _ -> Ok (statements, ts))
     | Error _ as e -> e)
  | Error _ as e -> e

(*
assignment_statement : variable ASSIGN expr
*)
and assignment_statement tokens =
  print_rule "assignment_statement" tokens;
  match variable tokens with
  | Ok (left, tl) ->
    (match tl with
     | Token.Assign as token :: the_rest ->
       (match expr the_rest with
        | Ok (right, tls) ->
          Ok (Ast.Assign {left; right; token}, tls)
        | Error _ as e -> e)
     | _ -> Error "variable needs to have an assign afterwards")
  | Error _ as e -> e

(*
   variable: ID
*)
(* and variable tokens : (Ast.t * Token.t list, string) Core.Std.Result.t = *)
and variable tokens =
  print_rule "variable" tokens;
  match tokens with
  | (Token.Id value  as token) :: tl -> Ok (Ast.Var {token; value}, tl)
  | _ -> Error "next token should be an Id"

(*
compound_statement : BEGIN statement_list END
*)
and compound_statement tokens =
  print_rule "compound_statement" tokens;
  match tokens with
  | Token.Begin :: tl ->
    (match statement_list tl with
     | Ok (nodes, ts) -> (match ts with
         | Token.End :: ts' ->
           Ok (Ast.Compound {children = nodes}, ts')
         | _ -> Error "compound statements must end with End")
     | Error _ as e -> e)
  | some_token :: _ -> Error (Printf.sprintf "compound statements must begin with 'Begin', not %s" (Token.to_string some_token))
  | _ -> Error "empty list, or something else bad"
(*
program : compound_statement DOT
*)
and program tokens =
  print_rule "program" tokens;
  match compound_statement tokens with
  | Ok(c, tl) -> (match tl with
      | Token.Dot :: _ -> Ok (c, [])
      | _ -> Error "Programs must end with a Dot")
  | Error _ as e -> e

(*
factor : (PLUS | MINUS) factor
        | INTEGER
        | LPAREN expr RPAREN
        | variable
*)
and factor tokens =
  print_rule "factor" tokens;
  match tokens with
  | Token.Operator o :: tl when not (is_term_op o) ->
    (match factor tl with
    | Ok (new_factor, ts) ->
      Ok (Ast.UnaryOp {token = o; expr = new_factor}, ts)
    | Error _ as e -> e
    )
  | Token.Integer i as t :: tl ->
    printf "factor %i" i;
    print_newline (); Ok (Ast.Number {token = t; value = i}, tl)
  | Token.LParen :: tl ->
    print_endline "found a left paren";
    (match expr tl with
     | Error _ as e -> e
     | Ok (res, Token.RParen :: ts) ->
       Ok (res, ts)
     | Ok (_, hd :: _) -> Error (Printf.sprintf "next token %s is not an right paren" (Token.to_string hd))
     | Ok (_, []) -> Error "The list of tokens is empty"
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
      (match factor tl with
       | Ok (next_factor_result, ts) ->
         Ok (Ast.BinOp {token = o; left = node; right = next_factor_result}, ts)
       | Error _ as e -> e
      )
    | _ -> Ok (node, t)
  in
  match factor tokens with
  | Ok (n, t) -> term' n t
  | Error _ as e -> e

(*
expr: term ((PLUS|MINUS) term)*
*)
and expr tokens =
  print_rule "expr" tokens;
  let rec expr' node t =
    match t with
    | Token.Operator o :: tl when not (is_term_op o) ->
      printf "expr op %s" (Token.Operator.to_string o); print_newline();
      (match term tl with
       | Ok (next_term_result, ts) ->
         expr' (Ast.BinOp {token = o; left = node; right = next_term_result}) ts
       | Error _ as e -> e
      )
    | _ :: _ -> Ok (node, t)
    | [] -> Ok (node, [])
  in
  match term tokens with
  | Ok (ast, remaining_tokens) -> expr' ast remaining_tokens
  | Error _ as e -> e

let parse text =
  printf "you input %s" text;
  print_newline ();
  let tokens = (Token.remove_whitespace (Lexer.tokenize text)) in
  program tokens
