type t = Integer of int
       | Plus
       | Minus
       | Whitespace
       | EOF
       | Unknown of char

let to_string token =
  match token with
  | Integer i -> (string_of_int i)
  | Plus -> "+"
  | Minus -> "-"
  | Whitespace -> " "
  | EOF -> ""
  | Unknown c -> Char.escaped c

let is_digit = function
  | '1' | '2' | '3' | '4' | '5'
  | '6' | '7' | '8' | '9' | '0' -> true
  | _ -> false

let of_char = function
  | c when is_digit c -> Integer (int_of_string (Char.escaped c))
  | '+' -> Plus
  | '-' -> Minus
  | ' ' -> Whitespace
  | _ as c -> Unknown c

let remove_whitespace tokens =
  List.filter
    (fun x -> match x with
       | Whitespace -> false
       | _ -> true)
    tokens

let rec parse_integer int_str chars =
  match chars with
  | c :: tl when is_digit c ->
    parse_integer (int_str ^ (Char.escaped c)) tl
  | _ -> (Integer (int_of_string int_str), chars)

(* get a single token from the list of chars*)
let parse chars =
  match chars with
  | [] -> EOF, chars (* shouldn't happen *)
  | hd :: tl ->
    if is_digit hd then
      parse_integer (Char.escaped hd) tl
    else
      (of_char hd, tl)

