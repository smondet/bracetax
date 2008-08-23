
module Stack = struct
    type environment = [
        | `unknown of string * string list
        | `italic
        | `bold
        | `mono_space
        | `under_line
        | `superscript
        | `subscript
        | `paragraph
        | `new_line
        | `quotation of string * string
        | `non_break_space
        | `open_brace
        | `close_brace
        | `sharp
        | `utf8_char of int
        | `verbatim of string list
    ]

    type t = environment list ref

    let empty () = ref []

    let push s cmd = s := cmd :: !s

    let pop s = match !s with [] -> None | h :: t -> s:= t; Some h

    let head s = match !s with [] -> None | h :: t -> Some h
end

module Names = struct

    let italic = "i"
    let bold = "b"
    let mono_space = "t"
    (* let under_line = "u" *)
    let superscript = "sup"
    let subscript = "sub"
    let paragraph = "p"
    let new_line = "br"

    let quotation = "q"
    (* arg1: ' | " | fr | de | sp, default '"' (en) (fr = sp + nbsp's)
     * 
     * http://en.wikipedia.org/wiki/Quotation_mark_glyphs
     * http://en.wikipedia.org/wiki/Quotation_mark,_non-English_usage
     * *)

    let non_break_space = "~"
    let open_brace = "{"
    let close_brace = "}"
    let sharp = "#"
    let utf8_char = "utf" (* arg1:ocaml int *)


end


let non_env_cmd_of_name name args = (
    let module C = Names in
    let ios s = try int_of_string s with e -> 0 in
    let (env:Stack.environment) =
        match name with
        | s when s = C.paragraph        ->  `paragraph          
        | s when s = C.new_line         ->  `new_line          
        | s when s = C.non_break_space  ->  `non_break_space          
        | s when s = C.open_brace       ->  `open_brace          
        | s when s = C.close_brace      ->  `close_brace          
        | s when s = C.sharp            ->  `sharp          
        | s when s = C.utf8_char        -> 
            (try `utf8_char (ios (List.hd args)) with e -> `unknown (s, args))
        | s -> `unknown (s, args)
    in
    env
)


