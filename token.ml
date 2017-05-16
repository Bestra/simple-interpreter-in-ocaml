module Operator = struct
  type t = Plus | Minus | Mult | Div
  let to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
end

type t = Integer of int
       | Operator of Operator.t
       | LParen
       | RParen
       | Whitespace
       | EOF
       | Unknown of char

let to_string token =
  match token with
  | Integer i -> (string_of_int i)
  | Operator o -> (Operator.to_string o)
  | LParen -> "("
  | RParen -> ")"
  | Whitespace -> " "
  | EOF -> ""
  | Unknown c -> Char.escaped c

let is_digit = function
  | '1' | '2' | '3' | '4' | '5'
  | '6' | '7' | '8' | '9' | '0' -> true
  | _ -> false

let of_char = function
  | c when is_digit c -> Integer (int_of_string (Char.escaped c))
  | '+' -> Operator Operator.Plus
  | '-' -> Operator Operator.Minus
  | '*' -> Operator Operator.Mult
  | '/' -> Operator Operator.Div
  | '(' -> LParen
  | ')' -> RParen
  | ' ' -> Whitespace
  | _ as c -> Unknown c

let remove_whitespace tokens =
  List.filter
    (fun x -> match x with
       | Whitespace -> false
       | _ -> true)
    tokens
