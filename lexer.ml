open Core.Std

let rec tokenize' tokens chars =
  match chars with
  | [] -> tokens
  | _ ->
    let (new_token, remaining_chars) = Token.parse chars in
    tokenize' (List.append tokens [new_token]) remaining_chars

let tokenize str =
  tokenize' [] (String.to_list str)
