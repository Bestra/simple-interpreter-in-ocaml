open Core.Std

let perform_op a op b =
  match op with
  | Token.Operator.Plus -> a + b
  | Token.Operator.Minus -> a - b

(* valid cases: Some op, next token is integer
                No op, next token is Operator *)
let rec eval_expr accum op tokens =
  match tokens with
  | [] -> Ok accum
  | hd :: tl ->
    match (op, hd) with
    | (Some o, Token.Integer i) ->
      eval_expr (perform_op accum o i) None tl
    | (None, Token.Operator o) ->
      eval_expr accum (Some o) tl
    | _ -> Error "syntax error in evaluation. try again"

let  evaluate_expr tokens =
  match Token.remove_whitespace(tokens) with
  | Token.Integer i :: Token.Operator o :: tl -> eval_expr i (Some o) tl
  | _ -> Error "syntax error in tokens. try again"

let rec build_expr tokens chars =
  match chars with
  | [] -> tokens
  | _ ->
    let (new_token, remaining_chars) = Token.parse chars in
    build_expr (List.append tokens [new_token]) remaining_chars

let expr text =
  match evaluate_expr (build_expr [] text) with
  | Ok result ->
    printf "%i" result;
    print_newline ()
  | Error e ->
    printf "%s" e;
    print_newline ()


let interpret text =
  printf "you input %s" text;
  print_newline ();
  expr (String.to_list text)

let rec read_and_interpret () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> printf "done"
  | Some x -> interpret x; read_and_interpret ()

let () =
  printf "Calculator:";
  print_newline ();
  read_and_interpret ()
