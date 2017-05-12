open Core.Std

let run_expr tokens =
  let sans_whitespace = List.filter ~f:(fun x -> match x with
      | Token.Whitespace -> false
      | _ -> true) tokens in
  match sans_whitespace with
  | Token.Integer a :: Token.Plus :: Token.Integer b :: _ ->
    printf "a is %i, b is %i" a b;
    print_newline ();
    a + b
  | _ -> 0

let rec build_expr tokens chars =
  match chars with
  | [] -> tokens
  | _ ->
    let (new_token, remaining_chars) = Token.parse chars in
    build_expr (List.append tokens [new_token]) remaining_chars

let expr text =
  let result = run_expr (build_expr [] text) in
  printf "%i" result;
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
