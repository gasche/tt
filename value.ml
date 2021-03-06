(** Values used for normalization-by-evaluation. *)

type variable = Syntax.variable

(** The type of values. The [Neutral] values are applications whose head is a variable. *)
type value =
  | Neutral of neutral
  | Universe of int
  | Pi of abstraction
  | Lambda of abstraction

and abstraction = variable * value * (value -> value continuation)

and 'a continuation = { call : 'b . ('a -> 'b) -> 'b }

and neutral =
  | Var of variable
  | App of neutral * value

let id x = x

(** Comparison of values for equality. It descends into abstractions by first applying them
    to freshly generated variables, which has the effect of alpha-equivalence. *)
let rec equal v1 v2 =
  match v1, v2 with
    | Neutral n1, Neutral n2 -> equal_neutral n1 n2
    | Universe k1, Universe k2 -> k1 = k2
    | Pi a1, Pi a2 -> equal_abstraction a1 a2
    | Lambda a1, Lambda a2 -> equal_abstraction a1 a2
    | (Neutral _ | Universe _ | Pi _ | Lambda _), _ -> false

and equal_abstraction (x, v1, f1) (_, v2, f2) =
  equal v1 v2 &&
    (let x = Neutral (Var (Syntax.refresh x)) in equal ((f1 x).call id) ((f2 x).call id))

and equal_neutral n1 n2 =
  match n1, n2 with
    | Var x1, Var x2 -> x1 = x2
    | App (n1, v1), App (n2, v2) -> equal_neutral n1 n2 && equal v1 v2
    | (Var _ | App _), _ -> false

(** [eval ctx e] evaluates expression [e] in context [ctx] to a value. *)
let eval ctx =
  let rec eval env (e, loc) =
    match e with
      | Syntax.Var x ->
        begin
          try List.assoc x env
          with Not_found ->
            begin
              match
                (try Ctx.lookup_value x ctx
                 with Not_found -> Error.runtime ~loc "unkown identifier %t" (Print.variable x))
              with
                | None -> Neutral (Var x)
                | Some e -> eval env e
            end
        end
      | Syntax.Universe k -> Universe k
      | Syntax.Pi a -> Pi (eval_abstraction env a)
      | Syntax.Lambda a -> Lambda (eval_abstraction env a)
      | Syntax.App (e1, e2) ->
        let v2 = eval env e2 in
        (match eval env e1 with
          | Lambda (_, _, f) -> (f v2).call id
          | Neutral n -> Neutral (App (n, v2))
          | Universe _ | Pi _ -> Error.runtime ~loc:(snd e2) "Function expected")
  and eval_abstraction env (x, t, e) =
    (x, eval env t, fun v -> { call = fun cont -> cont (eval ((x, v) :: env) e) })
  in
    eval []

(** [eval' ctx e] is like [eval ctx e] except that [e] is an expression without
    position. *)
let eval' ctx e = eval ctx (Syntax.nowhere e)

(** [reify v] reifies value [v] to an expression. *)
let rec reify v = Syntax.nowhere (reify' v)

and reify' = function
  | Neutral n -> reify_neutral' n
  | Universe k -> Syntax.Universe k
  | Pi a -> Syntax.Pi (reify_abstraction a)
  | Lambda a -> Syntax.Lambda (reify_abstraction a)

and reify_abstraction (x, t, f) =
  let x = Syntax.refresh x in
  (f (Neutral (Var x))).call (fun body ->
    (x, reify t, reify body))

and reify_neutral n = Syntax.nowhere (reify_neutral' n)

and reify_neutral' = function
  | Var x -> Syntax.Var x
  | App (n, v) ->
    (* Syntax.App (reify_neutral n, reify v) *)
    reify_cont v (fun v ->
      Syntax.App (reify_neutral n, v))

and reify_cont v ret =
  reify'_cont v (fun v -> ret (Syntax.nowhere v))

and reify'_cont v ret = match v with
  | Universe _ | Pi _ | Lambda _ -> ret (reify' v)
  | Neutral n -> reify_neutral'_cont n ret

and reify_neutral'_cont n ret = match n with
  | Var x -> ret (Syntax.Var x)
  | App (n, v) ->
    reify_cont v (fun v ->
      reify_neutral_cont n (fun n ->
        ret (Syntax.App (n, v))))

and reify_neutral_cont n ret =
  reify_neutral'_cont n (fun n -> ret (Syntax.nowhere n))
