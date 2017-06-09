open Core.Std

type symbol_table = int String.Map.t
type state = {accum: int; symbols: int String.Map.t}

let print_symbols (syms: int String.Map.t) =
  let sexp =
  List.sexp_of_t
    (fun (k, v) -> Sexp.List [Sexp.Atom k; Sexp.Atom (Int.to_string v)] )
    (String.Map.to_alist syms)
  in
  Sexp.to_string sexp

let interpret text =
  printf "you input %s" text;
  print_newline ();
  Parser.parse text

let rec visit ast (env: state) : state =
  match ast with
  | Ast.NoOp -> env
  | Ast.Compound c ->
    List.fold c.children ~init:env ~f:(fun acc child ->
        let new_env = visit child env in
        print_endline "existing symbols";
        print_endline (print_symbols acc.symbols);
        print_endline "incoming symbols";
        print_endline (print_symbols new_env.symbols);
        let new_symbols = Map.merge acc.symbols new_env.symbols ~f:(fun ~key r ->
            match r with
            | `Left v -> Some v
            | `Right v -> Some v
            | `Both (a, b) -> Some b)
        in
        print_endline "merged symbols";
        print_endline (print_symbols new_symbols);
        {acc with symbols = new_symbols}
      )

  | Ast.Assign a ->
    (match a.left with
     | Ast.Var v ->
       let new_state = (visit a.right env) in
       let new_symbols = Map.add env.symbols ~key:v.value ~data:(new_state.accum) in
       print_endline "assign";
       print_endline (print_symbols new_symbols);
       {env with symbols = new_symbols}
     | _ -> failwith "assigments must start with a variable"
    )
  | Ast.Var v ->
    {env with accum = Map.find_exn env.symbols v.value}
  | Ast.Number n -> {env with accum = n.value}
  | Ast.BinOp b ->
    let l_e = (visit b.left env) in
    let r_e = (visit b.right env) in
    let new_syms = l_e.symbols in
    (match b.token with
     | Token.Operator.Plus ->
       {accum = l_e.accum + r_e.accum; symbols = new_syms}
     | Token.Operator.Minus ->
       {accum = l_e.accum - r_e.accum; symbols = new_syms}
     | Token.Operator.Mult ->
       {accum = l_e.accum * r_e.accum; symbols = new_syms}
     | Token.Operator.Div ->
       {accum = l_e.accum / r_e.accum; symbols = new_syms}
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
  print_symbols final_state.symbols

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
