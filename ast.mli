type t = Number of Token.t * int | BinOp of Token.Operator.t * t * t | UnaryOp of Token.Operator.t * t

val to_string : t -> string
