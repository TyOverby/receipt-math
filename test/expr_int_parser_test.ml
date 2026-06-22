open! Core
module Expr = Receipt_math.Expr_int

(* A few hand-built expressions exercise the printer's precedence and
   associativity rules and the [mirror*]/negative-literal spellings. *)
let show t = print_endline (Expr.to_string t)

let%expect_test "operator spellings" =
  show Expr.(Or (Xor (And (X, Y), C 5), Add (X, Y)));
  [%expect {| x & y ^ 5 | x + y |}];
  show Expr.(Mul (Add (X, Y), Sub (X, C 3)));
  [%expect {| (x + y) * (x - 3) |}];
  show Expr.(Mod (Mul (X, Y), C 7));
  [%expect {| x * y % 7 |}]
;;

let%expect_test "left associativity needs parens only on the right" =
  show Expr.(Sub (Sub (X, Y), C 1));
  [%expect {| x - y - 1 |}];
  show Expr.(Sub (X, Sub (Y, C 1)));
  [%expect {| x - (y - 1) |}]
;;

let%expect_test "mirrors use call syntax" =
  show Expr.(MirrorX (MirrorY (Add (X, Y))));
  [%expect {| mirrorX(mirrorY(x + y)) |}]
;;

let%expect_test "negative constants are parenthesised" =
  show Expr.(Add (X, C (-5)));
  [%expect {| x + (-5) |}]
;;

(* Structural round-trip on the hand-built cases: parsing the printed form must
   reproduce the original tree exactly. *)
let%test_unit "round-trip is structural" =
  let check t = [%test_eq: Expr.t] t (Expr.of_string_exn (Expr.to_string t)) in
  check Expr.(Or (Xor (And (X, Y), C 5), Add (X, Y)));
  check Expr.(Mul (Add (X, Y), Sub (X, C 3)));
  check Expr.(Sub (X, Sub (Y, C 1)));
  check Expr.(MirrorX (MirrorY (Add (X, Y))));
  check Expr.(Add (X, C (-5)));
  check (C Int.min_value)
;;

(* The property under test: for any expression, printing it and parsing it back
   must yield a tree that [eval]s identically at every cell of the 256x256 grid
   (and at the scale exponent [p] the mirrors depend on). We generate random
   programs, render and re-parse them, then compare the two over the whole grid. *)
let%test_unit "printer/parser round-trip preserves eval on the 256x256 grid" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map t = Expr.quickcheck_generator
    and p = Int.gen_incl 0 8 in
    t, p
  in
  Quickcheck.test
    gen
    (* Bound the generator size: the default schedule grows trees up to ~[trials]
       nodes, and re-evaluating a 200-node tree at all 65536 cells per trial is
       needlessly slow (especially under the js inline-test runner). Trees of up
       to ~12 nodes already nest every operator deeply enough to exercise the
       precedence and associativity rules. *)
    ~sizes:(Sequence.cycle_list_exn (List.init 13 ~f:Fn.id))
    ~trials:64
    ~sexp_of:[%sexp_of: Expr.t * int]
    ~f:(fun (t, p) ->
      let printed = Expr.to_string t in
      let t' =
        match Expr.of_string printed with
        | Ok t' -> t'
        | Error e ->
          raise_s [%message "failed to parse printed expression" printed (e : Error.t)]
      in
      for x = 0 to Expr.dimension - 1 do
        for y = 0 to Expr.dimension - 1 do
          let direct = Expr.eval ~x ~y p t in
          let roundtripped = Expr.eval ~x ~y p t' in
          if direct <> roundtripped
          then
            raise_s
              [%message
                "round-trip changed eval"
                  printed
                  (x : int)
                  (y : int)
                  (p : int)
                  (direct : int)
                  (roundtripped : int)]
        done
      done)
;;
