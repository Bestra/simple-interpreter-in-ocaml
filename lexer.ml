open Core.Std

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alphanum = function
  | '0'..'9' -> true
  | 'a'..'z' -> true
  | 'A'..'Z' -> true
  | _ -> false

let rec parse_integer int_str chars =
  print_string int_str;
  match chars with
  | c :: tl when is_digit c ->
    parse_integer (int_str ^ (Char.escaped c)) tl
  | _ -> (Token.Integer (int_of_string int_str), chars)

let rec parse_id str chars =
  match chars with
  | c :: tl when is_alphanum c ->
    parse_id (str ^ (Char.escaped c)) tl
  | _ ->
    (match str with
     | "BEGIN" -> (Token.Begin, chars)
     | "END" -> (Token.End, chars)
     | _ as s -> (Token.Id s, chars)
    )

(* get a single token from the list of chars*)
let parse chars =
  match chars with
  | [] -> Token.EOF, chars (* shouldn't happen *)
  | ':' :: '=' :: tl -> (Token.Assign, tl)
  | ';' :: tl -> (Token.Semi, tl)
  | '.' :: tl -> (Token.Dot, tl)
  | hd :: tl when is_digit hd ->
      parse_integer (Char.escaped hd) tl
  | hd :: tl when is_alphanum hd ->
      parse_id (Char.escaped hd) tl
  | hd :: tl ->
      (Token.of_char hd, tl)

let tokenize str =
  let rec tokenize' tokens chars =
    match chars with
    | [] -> tokens
    | _ ->
      let (new_token, remaining_chars) = parse chars in
      tokenize' (List.append tokens [new_token]) remaining_chars
  in
  tokenize' [] (String.to_list str)

