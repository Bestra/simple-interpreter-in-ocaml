open Core.Std

type t = Number of {token: Token.t; value: int}
       | BinOp of {token: Token.Operator.t; left: t; right: t}
       | UnaryOp of {token: Token.Operator.t; expr: t}

let rec to_string = function
  | Number n -> Printf.sprintf "%i" n.value
  | BinOp b -> "(" ^ (to_string b.left) ^ " " ^ (Token.Operator.to_string b.token) ^ " "^ (to_string b.right) ^ ")"
  | UnaryOp u -> "(" ^ (Token.Operator.to_string u.token) ^ " " ^ (to_string u.expr) ^ " )"
