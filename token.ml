open Core.Std
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
       | Begin
       | End
       | Dot
       | Assign
       | Semi
       | Id of string
       | Unknown of char

let to_string token =
  match token with
  | Integer i -> (string_of_int i)
  | Operator o -> (Operator.to_string o)
  | LParen -> "("
  | RParen -> ")"
  | Whitespace -> " "
  | EOF -> ""
  | Begin -> "BEGIN"
  | End -> "END"
  | Dot -> "."
  | Assign -> ":="
  | Semi -> ";"
  | Id i -> i
  | Unknown c -> Char.escaped c

let of_char = function
  | '0'..'9' as c -> Integer (int_of_string (Char.escaped c))
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
    ~f: (fun x -> match x with
       | Whitespace -> false
       | _ -> true)
    tokens
