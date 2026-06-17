open! Core

type t =
  { scale : int
  ; gradient : [ `linear | `square | `sqrt | `sin | `cos ]
  ; lightness : float
  ; lightness_delta : float
  ; chroma : float
  ; chroma_delta : float
  ; hue : float
  ; hue_delta : float
  }
[@@deriving quickcheck, sexp_of, equal]

val get_start_color : t -> Oklab.t
val get_end_color : t -> Oklab.t
