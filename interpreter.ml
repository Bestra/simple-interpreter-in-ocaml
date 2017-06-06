open Core.Std

let interpret text =
  printf "you input %s" text;
  print_newline ();
  Parser.parse text

let rec eval ast =
  match ast with
  | Ast.Number n -> n.value
  | Ast.BinOp b ->
    (match b.token with
     | Token.Operator.Plus ->
       (eval b.left) + (eval b.right)
     | Token.Operator.Minus ->
       (eval b.left) - (eval b.right)
     | Token.Operator.Mult ->
       (eval b.left) * (eval b.right)
     | Token.Operator.Div ->
       (eval b.left) / (eval b.right)
    )

  | Ast.UnaryOp u ->
    (match u.token with
     | Token.Operator.Plus ->
       eval u.expr
     | Token.Operator.Minus ->
       -(eval u.expr)
     | Token.Operator.Mult | Token.Operator.Div ->
       (* no op *)
       eval u.expr
    )

let rec read_and_interpret () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> print_endline "done"
  | Some x ->
    (match interpret x with
     | Ok (i, _) ->
       printf "The ast is: %s" (Ast.to_string i); print_newline();
       printf "The result is %i" (eval i); print_newline();
     | Error s -> print_endline s);
    read_and_interpret ()

let () =
  print_endline "Calculator:";
  read_and_interpret ()
