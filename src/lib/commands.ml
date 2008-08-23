
module Stack = struct
    type environment = [
        | `cmd_end
        | `cmd_inside of environment
        | `cmd_begin of string * string list
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

    let is_begin name = name = "begin"
    let is_end name = name = "end"

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

let env_to_string (e:Stack.environment) = (
    let spr = Printf.sprintf in
    match e with
    | `cmd_end                   -> spr "cmd_end                "
    | `cmd_inside  environment   -> spr "cmd_inside  environment"
    | `cmd_begin (s, l)          -> spr "cmd_begin (%s, l)       " s
    | `unknown (s, l)            -> spr "unknown (%s, l)         " s
    | `italic                    -> spr "italic                 "
    | `bold                      -> spr "bold                   "
    | `mono_space                -> spr "mono_space             "
    | `under_line                -> spr "under_line             "
    | `superscript               -> spr "superscript            "
    | `subscript                 -> spr "subscript              "
    | `paragraph                 -> spr "paragraph              "
    | `new_line                  -> spr "new_line               "
    | `quotation (s)             -> spr "quotation (s)          "
    | `non_break_space           -> spr "non_break_space        "
    | `open_brace                -> spr "open_brace             "
    | `close_brace               -> spr "close_brace            "
    | `sharp                     -> spr "sharp                  "
    | `utf8_char i               -> spr "utf8_char i            "
    | `verbatim l                -> spr "verbatim l             "
)
