type t = Number of {token: Token.t; value: int}
       | BinOp of {token: Token.Operator.t; left: t; right: t}
       | UnaryOp of {token: Token.Operator.t; expr: t}

val to_string : t -> string
