open Core.Std
module Operator = struct
  type t = Plus | Minus | Mult | Div
  let to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
end

type t = Assign
  | Begin
  | Comma
  | Colon
  | Dot
  | EOF
  | End
  | Id of string
  | IntegerConst of int
  | IntegerType
  | LParen
  | Operator of Operator.t
  | Program
  | RealType
  | RParen
  | Semi
  | Unknown of char
  | Var
  | Whitespace

let to_string token =
  match token with
  | Assign -> ":="
  | Begin -> "BEGIN"
  | Comma -> ","
  | Colon -> ":"
  | Dot -> "."
  | EOF -> ""
  | End -> "END"
  | Id i -> Printf.sprintf "Id: %s" i
  | IntegerConst i -> (string_of_int i)
  | IntegerType -> "INTEGER"
  | LParen -> "("
  | Operator o -> (Operator.to_string o)
  | Program -> "PROGRAM"
  | RealType -> "REAL"
  | RParen -> ")"
  | Semi -> ";"
  | Unknown c -> Printf.sprintf "Unknown: %s" (Char.escaped c)
  | Var -> "VAR"
  | Whitespace -> " "

let of_char = function
  | '0'..'9' as c -> IntegerConst (int_of_string (Char.escaped c))
  | '+' -> Operator Operator.Plus
  | '-' -> Operator Operator.Minus
  | '*' -> Operator Operator.Mult
  | '/' -> Operator Operator.Div
  | '(' -> LParen
  | ')' -> RParen
  | ';' -> Semi
  | ':' -> Colon
  | ',' -> Comma
  | '.' -> Dot
  | ' ' -> Whitespace
  | _ as c -> Unknown c

let remove_whitespace tokens =
  List.filter
    ~f: (fun x -> match x with
       | Whitespace -> false
       | _ -> true)
    tokens
