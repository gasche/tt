(** Pretty-printing of expressions with the Ocaml [Format] library. *)

(** Print an expression, possibly placing parentheses around it. We always
    print things at a given "level" [at_level]. If the level exceeds the
    maximum allowed level [max_level] then the expression should be parenthesized.

    Let us consider an example. When printing nested applications, we should print [App
    (App (e1, e2), e3)] as ["e1 e2 e3"] and [App(e1, App(e2, e3))] as ["e1 (e2 e3)"]. So
    if we assign level 1 to applications, then during printing of [App (e1, e2)] we should
    print [e1] at [max_level] 1 and [e2] at [max_level] 0.
*)
let block ?(max_level=9999) ?(at_level=0) () =
  if max_level < at_level
  then format_of_string "(@[", format_of_string "@])"
  else format_of_string "@[", format_of_string "@]"

let print ?max_level ?at_level ppf =
  let before, after = block ?max_level ?at_level () in
  Format.fprintf ppf before ;
  Format.kfprintf (fun ppf -> Format.fprintf ppf after) ppf

(** Print the given source code position. *)
let position loc ppf =
  match loc with
  | Syntax.Nowhere ->
      Format.fprintf ppf "unknown position"
  | Syntax.Position (begin_pos, end_pos) ->
      let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let begin_line = begin_pos.Lexing.pos_lnum in
      let filename = begin_pos.Lexing.pos_fname in

      if String.length filename != 0 then
        Format.fprintf ppf "file %S, line %d, charaters %d-%d" filename begin_line begin_char end_char
      else
        Format.fprintf ppf "line %d, characters %d-%d" (begin_line - 1) begin_char end_char

(** Print the name of a variable. *)
let variable x ppf =
  match x with
    | Syntax.Dummy -> print ppf "_"
    | Syntax.String x -> print ppf "%s" x
    | Syntax.Gensym (x, k) -> print ppf "%s_%d" x k

(** [expr e ppf] prints (beautified) expression [e] using formatter [ppf]. *)
let expr e ppf =
  let rec expr ?max_level (e, _) ppf =  expr' ?max_level e ppf
  and expr' ?max_level e ppf =
    let print ?at_level = print ?max_level ?at_level ppf in
      match e with
        | Syntax.Var x -> variable x ppf
        | Syntax.Universe k -> print "Type %d" k
        | Syntax.Pi (Syntax.Dummy, t1, t2) ->
          print ~at_level:3 "%t ->@;%t" (expr ~max_level:2 t1) (expr t2)
        | Syntax.Pi (x, t1, t2) ->
          print ~at_level:3 "forall %t : %t,@;%t"
            (variable x) (expr ~max_level:4 t1) (expr t2)
        | Syntax.Lambda (x, t, e) ->
          print ~at_level:3 "fun %t : %t =>@;%t"
            (variable x) (expr t) (expr e)
        | Syntax.App (e1, e2) ->
          let before, after = block ?max_level ~at_level:1 () in
          Format.fprintf ppf before;
          Format.fprintf ppf "%t@;" (expr ~max_level:1 e1);
          expr_cont ~max_level:0 e2 ppf (fun () -> Format.fprintf ppf after)
  and expr_cont ?max_level (e, _) ppf ret =
    expr'_cont ?max_level e ppf ret
  and expr'_cont ?max_level e ppf ret =
    match e with
      | Syntax.App (e1, e2) ->
        let before, after = block ?max_level ~at_level:1 () in
        Format.fprintf ppf before;
        Format.fprintf ppf "%t@;" (expr ~max_level:1 e1);
        expr_cont ~max_level:0 e2 ppf (fun () -> Format.fprintf ppf after; ret ())
      | other -> expr' ?max_level e ppf; ret ()
  in
  expr (Beautify.beautify e) ppf
    
let expr' e ppf = expr (Syntax.nowhere e) ppf
  
(** Support for printing of errors, warning and debugging information. *)
let verbosity = ref 3
let message ?(loc=Syntax.Nowhere) msg_type v =
  if v <= !verbosity then
    begin
      Format.eprintf "%s at %t:@\n  @[" msg_type (position loc) ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@]@.") Format.err_formatter
    end
  else
    Format.ifprintf Format.err_formatter

let error (loc, err_type, msg) = message ~loc (err_type) 1 "%s" msg
let warning msg = message "Warning" 2 msg
let debug msg = message "Debug" 3 msg
