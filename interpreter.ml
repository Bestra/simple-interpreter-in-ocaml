open Core.Std

type symbol_table = int String.Map.t
type state = {accum: int; symbols: int String.Map.t}

let interpret text =
  printf "you input %s" text;
  print_newline ();
  Parser.parse text

(*
TODO: figure out what to do with symbols
*)
let rec visit ast (env: state) : state =
  match ast with
  | Ast.NoOp -> env
  | Ast.Compound c ->
    List.fold c.children ~init:env ~f:(fun acc child ->
        let new_env = visit child env in
        let new_symbols = Map.merge env.symbols new_env.symbols ~f:(fun ~key r ->
            match r with
            | `Left v -> Some v
            | `Right v -> Some v
            | `Both (a, b) -> Some b)
        in
        {accum = env.accum; symbols = new_symbols}
      )

  | Ast.Assign a ->
    (match a.left with
     | Ast.Var v ->
       let new_state = (visit a.right env) in
       let new_symbols = Map.add env.symbols ~key:v.value ~data:(new_state.accum) in
       {accum = env.accum; symbols = new_symbols}
     | _ -> failwith "assigments must start with a variable"
    )
  | Ast.Var v ->
    {accum = Map.find_exn env.symbols v.value; symbols = env.symbols}
  | Ast.Number n -> {accum = n.value; symbols = env.symbols}
  | Ast.BinOp b ->
    (match b.token with
     | Token.Operator.Plus ->
       let l_e = (visit b.left env) in
       let r_e = (visit b.right env) in
       {accum = l_e.accum + r_e.accum; symbols = l_e.symbols}
     | Token.Operator.Minus ->
       let l_e = (visit b.left env) in
       let r_e = (visit b.right env) in
       {accum = l_e.accum - r_e.accum; symbols = l_e.symbols}
     | Token.Operator.Mult ->
       let l_e = (visit b.left env) in
       let r_e = (visit b.right env) in
       {accum = l_e.accum * r_e.accum; symbols = l_e.symbols}
     | Token.Operator.Div ->
       let l_e = (visit b.left env) in
       let r_e = (visit b.right env) in
       {accum = l_e.accum / r_e.accum; symbols = l_e.symbols}
    )
  | Ast.UnaryOp u ->
    (match u.token with
     | Token.Operator.Plus ->
       visit u.expr env
     | Token.Operator.Minus ->
       let e = (visit u.expr env) in
       {accum = -e.accum; symbols = e.symbols}
     | Token.Operator.Mult | Token.Operator.Div ->
       (* no op *)
       visit u.expr env
    )

let eval_ast an_ast : string =
  let final_state = visit an_ast {accum = 0; symbols = String.Map.empty} in
  Sexp.to_string (String.Map.sexp_of_t (fun i -> Sexp.Atom (Int.to_string i)) final_state.symbols)

let rec read_and_interpret () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> print_endline "done"
  | Some x ->
    (match interpret x with
     | Ok (a, _) ->
       printf "The ast is: %s" (Ast.to_string a); print_newline();
       printf "The result is %s" (eval_ast a); print_newline();
     | Error s -> print_endline s);
    read_and_interpret ()

let () =
  print_endline "Calculator:";
  read_and_interpret ()
