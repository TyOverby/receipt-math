open! Core
open Canvas2d

(* A bitmap font loaded from the JSON dump in [departure.json]: one glyph per
   ASCII code, each a grid of 'X'/' ' pixel rows. *)
type t

(* Parse the font from the JSON text produced for [departure.json] (an array of
   [{ code; width; pixels }] records). Parsed with [Yojson]. *)
val of_json_string : string -> t

(* Overlay [text] onto [image_data], anchored at the bottom-left corner. The
   glyphs are drawn in [fg] over a rectangle filled with [bg]; the rectangle is
   1px larger than the text on every side (1px padding between text and rect).
   Long lines are wrapped to fit the image width, and embedded newlines start a
   new line. Pixels outside the image bounds are clipped. Colours are [r, g, b]
   triples in [0, 255]. *)
val draw_label
  :  t
  -> Image_data.t
  -> text:string
  -> fg:int * int * int
  -> bg:int * int * int
  -> unit
