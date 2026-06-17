open! Core
module Expr = Receipt_math.Expr_float

(* The property under test: simplifying an expression must not change the value that
   [eval] produces at any coordinate. [x]/[y] are always in [0, 1]. Every [simplify]
   rewrite is an exact algebraic identity of the float operator semantics, so the two
   values should agree to within floating-point noise (they are in fact bit-identical, but
   we compare with a tolerance to stay robust). *)
let eval_direct_and_simplified t ~x ~y =
  let direct = Expr.eval ~x ~y t in
  let simplified = Expr.eval ~x ~y (Expr.For_testing.simplify t) in
  direct, simplified
;;

let show t = print_s [%sexp (Expr.For_testing.simplify t : Expr.t)]

(* Each new algebraic identity fires and reduces the expression. Correctness (that they
   preserve [eval]) is covered by the property test below. *)

let%expect_test "sub by/of zero and self" =
  show Expr.(Sub (X, C 0.));
  [%expect {| X |}];
  show Expr.(Sub (C 0., X));
  [%expect {| X |}];
  show Expr.(Sub (X, X));
  [%expect {| (C 0) |}]
;;

let%expect_test "add of self is self (not doubled)" =
  show Expr.(Add (X, X));
  [%expect {| X |}]
;;

let%expect_test "mod with zero numerator" =
  show Expr.(Mod (C 0., Y));
  [%expect {| (C 0) |}]
;;

let%expect_test "mul by zero and one" =
  show Expr.(Mul (X, C 0.));
  [%expect {| (C 0) |}];
  show Expr.(Mul (X, C 1.));
  [%expect {| X |}]
;;

let%expect_test "xor with zero" =
  show Expr.(Xor (X, C 0.));
  [%expect {| X |}];
  show Expr.(Xor (C 0., X));
  [%expect {| X |}]
;;

let%expect_test "and (min) with zero, one, self" =
  show Expr.(And (X, C 0.));
  [%expect {| (C 0) |}];
  show Expr.(And (X, C 1.));
  [%expect {| X |}];
  show Expr.(And (X, X));
  [%expect {| X |}]
;;

let%expect_test "or (max) with one, zero, self" =
  show Expr.(Or (X, C 1.));
  [%expect {| (C 1) |}];
  show Expr.(Or (X, C 0.));
  [%expect {| X |}];
  show Expr.(Or (X, X));
  [%expect {| X |}]
;;

let%expect_test "and/or absorption" =
  show Expr.(And (X, Or (X, Y)));
  [%expect {| X |}];
  show Expr.(Or (X, And (Y, X)));
  [%expect {| X |}]
;;

let%expect_test "and/or idempotent nesting" =
  show Expr.(And (X, And (X, Y)));
  [%expect {| (And X Y) |}];
  show Expr.(Or (Or (Y, X), X));
  [%expect {| (Or Y X) |}]
;;

(* Quickcheck: [simplify] preserves [eval] across the whole unit square. *)
let%test_unit "simplify preserves eval" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map t = Expr.quickcheck_generator
    and x = Float.gen_incl 0. 1.
    and y = Float.gen_incl 0. 1. in
    t, x, y
  in
  Quickcheck.test
    gen
    ~trials:2000
    ~sexp_of:[%sexp_of: Expr.t * float * float]
    ~f:(fun (t, x, y) ->
      let direct, simplified = eval_direct_and_simplified t ~x ~y in
      if Float.( > ) (Float.abs (direct -. simplified)) 1e-4
      then
        raise_s
          [%message
            "simplify changed eval"
              (t : Expr.t)
              (x : float)
              (y : float)
              (direct : float)
              (simplified : float)])
;;
