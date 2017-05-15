open Core.Std

let perform_op a op b =
  match op with
  | Token.Operator.Plus -> a + b
  | Token.Operator.Minus -> a - b

(* valid cases: Some op, next token is integer
                No op, next token is Operator *)

let factor tokens =
  match tokens with
  | Token.Integer i :: tl -> Ok (i, tl)
  | hd :: _ -> Error (Printf.sprintf "next token %s is not an integer" (Token.to_string hd))
  | [] -> Error "no remaining tokens to factor"

let term tokens =
  let rec term' acc t =
    match t with
    | Token.Operator o :: tl ->
      (match factor tl with
       | Ok (next_factor_result, ts) ->
         term' (perform_op acc o next_factor_result) ts
       | Error _ as e -> e
      )
    | _ -> Ok (acc, t)
  in
  match factor tokens with
  | Ok (i, t) -> term' i t
  | Error _ as e -> e

let expr tokens =
  let rec expr' acc t =
    match t with
    | Token.Operator o :: tl ->
      (match term tl with
       | Ok (next_term_result, ts) ->
         expr' (perform_op acc o next_term_result) ts
       | Error _ as e -> e
      )
    | _ -> Ok acc
  in
  match term tokens with
  | Ok (result, remaining_tokens) -> expr' result remaining_tokens
  | Error _ as e -> e

let interpret text =
  printf "you input %s" text;
  print_newline ();
  let tokens = (Token.remove_whitespace (Lexer.tokenize text)) in
  expr tokens

let rec read_and_interpret () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> print_endline "done"
  | Some x ->
    (match interpret x with
     | Ok i -> printf "The result is %i" i; print_newline();
     | Error s -> print_endline s);
    read_and_interpret ()

let () =
  print_endline "Calculator:";
  read_and_interpret ()
