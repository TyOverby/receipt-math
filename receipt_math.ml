open Core
open Js_of_ocaml

(* A tiny demonstration that we are really running Core-compiled-to-JS in the
   browser: sum a list of "line item" prices and render the total. *)
let line_items = [ 4.50; 3.25; 12.00; 0.99 ]

let total = List.fold line_items ~init:0. ~f:( +. )

let summary =
  sprintf
    "%d items, subtotal $%.2f (avg $%.2f)"
    (List.length line_items)
    total
    (total /. Float.of_int (List.length line_items))

let () =
  let document = Dom_html.document in
  let body = document##.body in
  let p = Dom_html.createP document in
  p##.textContent := Js.some (Js.string summary);
  Dom.appendChild body p;
  Console.console##log (Js.string ("receipt-math (OCaml + Core) loaded: " ^ summary))
