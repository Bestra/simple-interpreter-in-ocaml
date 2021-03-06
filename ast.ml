open Core.Std

type t = Assign of {token: Token.t; left: t; right: t}
       | Block of {declarations: t list; compound_statement: t}
       | BinOp of {token: Token.Operator.t; left: t; right: t}
       | Compound of {children: t list}
       | NoOp
       | Program of {name: string; block: t}
       | Type of {token: Token.t}
       | UnaryOp of {token: Token.Operator.t; expr: t}
       | Var of {token: Token.t; value: string}
       | VarDecl of {var: t; var_type: t}
       | Number of {token: Token.t; value: int}

let rec to_string = function
  | Assign a -> Printf.sprintf "Assign %s to %s" (to_string a.left) (to_string a.right)
  | BinOp b -> "(" ^ (to_string b.left) ^ " " ^ (Token.Operator.to_string b.token) ^ " "^ (to_string b.right) ^ ")"
  | Block b -> "block"
  | Compound c -> "Compound"
  | NoOp -> "NoOp"
  | Number n -> Printf.sprintf "%i" n.value
  | Program k -> "Program"
  | Type t -> "Type"
  | UnaryOp u -> "(" ^ (Token.Operator.to_string u.token) ^ " " ^ (to_string u.expr) ^ " )"
  | Var v -> Printf.sprintf "Var %s" v.value
  | VarDecl v -> "variable declaration"
