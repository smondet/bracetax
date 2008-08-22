
type environment = [
    | `italic
    | `bold
    | `typeWriter
    | `verbatim of string list
]

type t = environment list ref

let empty () = ref []

let push s cmd = s := cmd :: !s

let pop s = match !s with [] -> None | h :: t -> s:= t; Some h

let head s = match !s with [] -> None | h :: t -> Some h

module Names = struct

    let italic = "i"
    let bold = "b"
    let mono_space = "t"
    let under_line = "u"
    let superscript = "sup"
    let subscript = "sub"
    let paragraph = "p"
    let new_line = "br"

    let quotation = "q"
    (* arg1: ' | " | fr | de | sp, default '"' (en) (fr = sp + nbsp's)
     * http://en.wikipedia.org/wiki/Quotation_mark_glyphs
     * http://en.wikipedia.org/wiki/Quotation_mark,_non-English_usage
     * *)

    let non_break_space = "~"
    let open_brace = "{"
    let close_brace = "}"
    let sharp = "#"
    let utf8_char = "utf" (* arg1:ocaml int *)


end


