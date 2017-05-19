open Core.Std

type t =
    Number of Token.t * int
  | BinOp of Token.Operator.t * t * t
  | UnaryOp of Token.Operator.t * t

let rec to_string = function
  | Number(_, i) -> Printf.sprintf "%i" i
  | BinOp(t, l, r) -> "(" ^ (to_string l) ^ " " ^ (Token.Operator.to_string t) ^ " "^ (to_string r) ^ ")"
  | UnaryOp(t, l) -> "(" ^ (Token.Operator.to_string t) ^ " " ^ (to_string l) ^ " )"
