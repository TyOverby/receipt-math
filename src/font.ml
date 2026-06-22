open! Core
open Canvas2d

(* A single glyph: its advance [width] in pixels and its [rows] of pixels, where
   an 'X' is an inked pixel and anything else (a space) is transparent. *)
type glyph =
  { width : int
  ; rows : string array
  }

(* Glyphs indexed by ASCII code (0..127); [None] for codes absent from the dump.
   [height] is the common pixel height of every glyph. *)
type t =
  { glyphs : glyph option array
  ; height : int
  }

let of_json_string s =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string s in
  let glyphs = Array.create ~len:128 None in
  let height = ref 0 in
  List.iter (to_list json) ~f:(fun entry ->
    let code = entry |> member "code" |> to_int in
    let width = entry |> member "width" |> to_int in
    let rows = entry |> member "pixels" |> to_list |> List.map ~f:to_string |> Array.of_list in
    height := Int.max !height (Array.length rows);
    if code >= 0 && code < 128 then glyphs.(code) <- Some { width; rows });
  { glyphs; height = !height }
;;

let glyph t c =
  let code = Char.to_int c in
  if code >= 0 && code < 128 then t.glyphs.(code) else None
;;

(* The advance width of [c], falling back to the space glyph (then to the font
   height) for codes that have no glyph. *)
let char_width t c =
  match glyph t c with
  | Some g -> g.width
  | None ->
    (match glyph t ' ' with
     | Some g -> g.width
     | None -> t.height)
;;

let line_width t line = String.fold line ~init:0 ~f:(fun acc c -> acc + char_width t c)

(* Greedily break [text] into lines no wider than [max_px], preferring spaces as
   break points but hard-splitting any single word that is too long on its own.
   Embedded newlines always force a break. *)
let wrap t text ~max_px =
  let lines = Queue.create () in
  let flush buf = Queue.enqueue lines (Buffer.contents buf); Buffer.clear buf in
  List.iter (String.split_lines text) ~f:(fun paragraph ->
    let cur = Buffer.create 64 in
    let cur_w = ref 0 in
    List.iter (String.split paragraph ~on:' ') ~f:(fun word ->
      let word_w = line_width t word in
      let space_w = char_width t ' ' in
      let fits extra = !cur_w + extra <= max_px in
      if Buffer.length cur = 0
      then
        if fits word_w
        then (Buffer.add_string cur word; cur_w := word_w)
        else
          (* Hard-break a single over-long word, character by character. *)
          String.iter word ~f:(fun c ->
            let cw = char_width t c in
            if Buffer.length cur > 0 && not (fits cw) then (flush cur; cur_w := 0);
            Buffer.add_char cur c;
            cur_w := !cur_w + cw)
      else if fits (space_w + word_w)
      then (Buffer.add_char cur ' '; Buffer.add_string cur word; cur_w := !cur_w + space_w + word_w)
      else (flush cur; cur_w := 0; Buffer.add_string cur word; cur_w := word_w));
    flush cur);
  Queue.to_list lines
;;

(* Paint a solid [w]x[h] rectangle of [color] with its top-left at [x, y],
   clipping to the image bounds. *)
let fill_rect image_data ~x ~y ~w ~h ~color:(r, g, b) =
  let iw = Image_data.width image_data in
  let ih = Image_data.height image_data in
  for py = Int.max 0 y to Int.min (ih - 1) (y + h - 1) do
    for px = Int.max 0 x to Int.min (iw - 1) (x + w - 1) do
      Image_data.set image_data ~x:px ~y:py ~r ~g ~b ~a:255
    done
  done
;;

(* Draw a single [line] with its top-left at [x, y] in [color], advancing one
   glyph at a time. Pixels outside the image are clipped. *)
let draw_line t image_data ~line ~x ~y ~color:(r, g, b) =
  let iw = Image_data.width image_data in
  let ih = Image_data.height image_data in
  let cursor = ref x in
  String.iter line ~f:(fun c ->
    (match glyph t c with
     | None -> ()
     | Some { width = _; rows } ->
       Array.iteri rows ~f:(fun ry row ->
         String.iteri row ~f:(fun rx pixel ->
           if Char.equal pixel 'X'
           then begin
             let px = !cursor + rx
             and py = y + ry in
             if px >= 0 && px < iw && py >= 0 && py < ih
             then Image_data.set image_data ~x:px ~y:py ~r ~g ~b ~a:255
           end)));
    cursor := !cursor + char_width t c)
;;

let draw_label t image_data ~text ~fg ~bg =
  let pad = 1 in
  (* The glyphs leave more blank space below their ink than above, so give the
     top 2 extra pixels of padding to visually balance the box. *)
  let top_pad = pad + 2 in
  let iw = Image_data.width image_data in
  let ih = Image_data.height image_data in
  (* The text (plus 1px of padding on each side) has to fit inside the image. *)
  let max_px = iw - (2 * pad) in
  let lines = wrap t text ~max_px in
  if not (List.is_empty lines)
  then begin
    let line_h = t.height in
    let n = List.length lines in
    let text_w = List.fold lines ~init:0 ~f:(fun acc l -> Int.max acc (line_width t l)) in
    let text_h = n * line_h in
    let rect_w = text_w + (2 * pad) in
    let rect_h = text_h + top_pad + pad in
    (* Anchor the whole block to the bottom-left corner. *)
    let rect_x = 0 in
    let rect_y = ih - rect_h in
    fill_rect image_data ~x:rect_x ~y:rect_y ~w:rect_w ~h:rect_h ~color:bg;
    (* A 1px foreground-coloured border just outside the box on the top and
       right (the top row extends one pixel right to fill the corner). *)
    fill_rect image_data ~x:rect_x ~y:(rect_y - 1) ~w:(rect_w + 1) ~h:1 ~color:fg;
    fill_rect image_data ~x:(rect_x + rect_w) ~y:(rect_y - 1) ~w:1 ~h:(rect_h + 1) ~color:fg;
    let tx = rect_x + pad in
    let ty = rect_y + top_pad in
    List.iteri lines ~f:(fun i line ->
      draw_line t image_data ~line ~x:tx ~y:(ty + (i * line_h)) ~color:fg)
  end
;;
