open Core.Std

type t =
    Number of Token.t * int
  | BinOp of Token.Operator.t * t * t

let rec to_string = function
  | Number(_, i) -> Printf.sprintf "%i" i
  | BinOp(t, l, r) -> "(" ^ (to_string l) ^ " " ^ (Token.Operator.to_string t) ^ " "^ (to_string r) ^ ")"
