type t = Number of {token: Token.t; value: int}
       | BinOp of {token: Token.Operator.t; left: t; right: t}
       | UnaryOp of {token: Token.Operator.t; expr: t}
       | Compound of {children: t list}
       | Assign of {token: Token.t; left: t; right: t}
       | Var of {token: Token.t; value: string}
       | NoOp

val to_string : t -> string
