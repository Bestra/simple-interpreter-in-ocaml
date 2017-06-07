open Core.Std

let is_term_op = function
  | Token.Operator.Plus
  | Token.Operator.Minus -> false
  | Token.Operator.Mult
  | Token.Operator.Div -> true


type 'a t = Parser of 'a

let parseToken token_to_match =
  let fn token_list =
    match token_list with
    | [] -> Error "no remaining tokens to factor"
    | hd :: tl when hd = token_to_match -> Ok (hd, tl)
    | hd :: _ -> Error (Printf.sprintf "next token %s is not %s" (Token.to_string hd) (Token.to_string token_to_match))
  in Parser fn

let parse_int =
  let fn token_list =
    match token_list with
    | [] -> Error "no remaining tokens to factor"
    | Token.Integer i as t:: tl -> Ok (Ast.Number {token = t; value = i}, tl)
    | hd :: _ -> Error (Printf.sprintf "next token %s is not an integer" (Token.to_string hd))
  in Parser fn

let parse_operator op_to_match =
  let fn token_list =
    match token_list with
    | [] -> Error "no remaining tokens to factor"
    | Token.Operator o as t :: tl when o = op_to_match -> Ok (t, tl)
    | hd :: _ -> Error (Printf.sprintf "next token %s is not %s" (Token.to_string hd) (Token.Operator.to_string op_to_match))
  in Parser fn

let run_parser parser input =
  let (Parser f) = parser in
  f input

let andThen parser1 parser2 =
  let parse_input input =
    match run_parser parser1 input with
    | Error _ as e -> e
    | Ok (result1, tl) ->
      (match run_parser parser2 tl with
       | Error _ as e -> e
       | Ok (result2, tl2) -> Ok (((result1, result2), tl2)))
  in
  Parser parse_input


let orElse parser1 parser2 =
  let parse_input input =
    match run_parser parser1 input with
    | Ok _ as r1 -> r1
    | Error _  ->
      (match run_parser parser2 input with
       | Error _ as e -> e
       | Ok (result2, tl) -> Ok (result2, tl))
  in
  Parser parse_input

let map_p f a_parser =
  let fn input =
    match run_parser a_parser input with
    | Ok (v, tl) -> Ok (f v, tl)
    | Error _ as e -> e
  in
  Parser fn

let return_p x =
  let fn input =
    Ok (x, input)
  in
  Parser fn

let (<|>) a b = orElse a b

let (<!>) = map_p
let (|>>) x f = map_p f x

let (>.>.) a b = andThen a b
let (>.>) a b =
  a >.>. b
  |> map_p (fun (r1, _) -> r1)

let (>>.) a b =
  a >.>. b
  |> map_p (fun (_, r2) -> r2)

let apply_p f_p x_p =
  (f_p >.>. x_p) |> map_p (fun (f, x) -> f x)

let (<*>) = apply_p

let lift2 f xP yP =
  return_p f <*> xP <*> yP

let rec sequence parser_list =
  let cons head tail = head::tail in
  let cons_p = lift2 cons in
  match parser_list with
  | [] as l -> return_p l
  | hd :: tl -> cons_p hd (sequence tl)

let rec parse_zero_or_more a_parser input =
  match run_parser a_parser input with
  | Error _ -> ([], input)
  | Ok (first_value, tl) ->
    let (later_values, remaining_input) = parse_zero_or_more a_parser tl in
    let values = first_value :: later_values in
    (values, remaining_input)

let many a_parser =
  let rec fn input =
    Ok (parse_zero_or_more a_parser input)
  in
  Parser fn

let many1 a_parser =
  let rec fn input =
    match run_parser a_parser input with
    | Error _ as e -> e
    | Ok (first_value, tl) ->
      let (later_values, remaining_input) = parse_zero_or_more a_parser tl in
      let values = first_value :: later_values in
      Ok (values, remaining_input)
  in
  Parser fn

let opt p =
  let some = map_p (fun k -> Some k) p in
  let none = return_p None in
  some <|> none

let between p1 p2 p3 =
  p1 >>. p2 >.> p3

let choice parser_list =
  match List.reduce ~f:(<|>) parser_list with
  | Some p -> p
  | None -> failwith "No parser matched"


let parseL = parseToken Token.LParen
let parseR = parseToken Token.RParen
let parse_parens = andThen parseL parseR
let parse_either = orElse parseL parseR

let empty tokens =
  Ok(Ast.NoOp, tokens)

(*
   statement: compound_statement | assignment_statement | empty
*)
let rec statement tokens =
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
  match variable tokens with
  | Ok (left, tl) ->
    (match tl with
     | Token.Assign as token :: _ ->
       (match expr tl with
        | Ok (right, tls) ->
          Ok (Ast.Assign {left = left; right =  right; token = token}, tls)
        | Error _ as e -> e)
     | _ -> Error "variable needs to have an assign afterwards")
  | Error _ as e -> e

(*
   variable: ID
*)
(* and variable tokens : (Ast.t * Token.t list, string) Core.Std.Result.t = *)
and variable tokens =
  match tokens with
  | (Token.Id s as t) :: tl -> Ok (Ast.Var {token = t; value = s}, tl)
  | _ -> Error "next token should be an Id"

(*
compound_statement : BEGIN statement_list END
*)
and compound_statement tokens =
  match tokens with
  | Token.Begin :: tl ->
    (match statement_list tl with
     | Ok (nodes, ts) -> (match ts with
         | Token.End :: ts' ->
           Ok (Ast.Compound {children = nodes}, ts')
         | _ -> Error "compound statements must end with End")
     | Error _ as e -> e)
  | _ -> Error "compound statements must begin with Begin"
(*
program : compound_statement DOT
*)
and program tokens =
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
  | _ :: _ -> Error "next token is not part of a factor"
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
