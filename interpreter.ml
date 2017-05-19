open Core.Std

let interpret text =
  printf "you input %s" text;
  print_newline ();
  Parser.parse text

let rec eval ast =
  match ast with
  | Ast.Number(t, i) -> i
  | Ast.BinOp(t, l, r) ->
    (match t with
     | Token.Operator.Plus ->
       (eval l) + (eval r)
     | Token.Operator.Minus ->
       (eval l) - (eval r)
     | Token.Operator.Mult ->
       (eval l) * (eval r)
     | Token.Operator.Div ->
       (eval l) / (eval r)
    )

  | Ast.UnaryOp(t, l) ->
    (match t with
     | Token.Operator.Plus ->
       eval l
     | Token.Operator.Minus ->
       -(eval l)
     | Token.Operator.Mult | Token.Operator.Div ->
       (* no op *)
       eval l
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
