open Core.Std

let is_term_op = function
  | Token.Operator.Plus
  | Token.Operator.Minus -> false
  | Token.Operator.Mult
  | Token.Operator.Div -> true

(* 
factor : (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN
*)
let rec factor tokens =
  match tokens with
  | Token.Operator o :: tl when not (is_term_op o) ->
    (match factor tl with
    | Ok (new_factor, ts) ->
      Ok (Ast.UnaryOp (o, new_factor), ts)
    | Error _ as e -> e
    )
  | Token.Integer i as t :: tl ->
    printf "factor %i" i;
    print_newline (); Ok (Ast.Number (t, i), tl)
  | Token.LParen :: tl ->
    print_endline "found a left paren";
    (match expr tl with
     | Error _ as e -> e
     | Ok (res, Token.RParen :: ts) ->
       Ok (res, ts)
     | Ok (_, hd :: _) -> Error (Printf.sprintf "next token %s is not an right paren" (Token.to_string hd))
     | Ok (_, []) -> Error "The list of tokens is empty"
    )
  | hd :: _ -> Error (Printf.sprintf "next token %s is not an integer" (Token.to_string hd))
  | [] -> Error "no remaining tokens to factor"

(* 
term : factor ((MUL | DIV) factor)*
*)
and term tokens =
  let term' node t =
    match t with
    | Token.Operator o :: tl when is_term_op o ->
      printf "term op %s" (Token.Operator.to_string o); print_newline();
      (match factor tl with
       | Ok (next_factor_result, ts) ->
         Ok (Ast.BinOp(o, node, next_factor_result), ts)
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
  let rec expr' node t =
    match t with
    | Token.Operator o :: tl when not (is_term_op o) ->
      printf "expr op %s" (Token.Operator.to_string o); print_newline();
      (match term tl with
       | Ok (next_term_result, ts) ->
         expr' (Ast.BinOp (o, node, next_term_result)) ts
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
  expr tokens
