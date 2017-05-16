open Core.Std
let rec parse_integer int_str chars =
  match chars with
  | c :: tl when Token.is_digit c ->
    parse_integer (int_str ^ (Char.escaped c)) tl
  | _ -> (Token.Integer (int_of_string int_str), chars)

(* get a single token from the list of chars*)
let parse chars =
  match chars with
  | [] -> Token.EOF, chars (* shouldn't happen *)
  | hd :: tl ->
    if Token.is_digit hd then
      parse_integer (Char.escaped hd) tl
    else
      (Token.of_char hd, tl)

let rec tokenize' tokens chars =
  match chars with
  | [] -> tokens
  | _ ->
    let (new_token, remaining_chars) = parse chars in
    tokenize' (List.append tokens [new_token]) remaining_chars

let tokenize str =
  tokenize' [] (String.to_list str)

