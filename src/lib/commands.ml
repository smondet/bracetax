
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
        | `list of [`itemize | `numbered ] * string list * bool ref
        | `item
        | `section of int * string
        | `link of string * string list
        | `image of string * [`w of int | `h of int ] list * string
        | `header
        | `title
        | `authors
        | `subtitle
    ]

    type t = environment list ref

    let empty () = ref []

    let push s cmd = s := cmd :: !s

    let pop s = match !s with [] -> None | h :: t -> s:= t; Some h

    let head s = match !s with [] -> None | h :: t -> Some h
end

module Names = struct

    let is_italic = (=) "i"
    let is_bold = (=) "b"
    let is_mono_space = (=) "t"
    let is_superscript = (=) "sup"
    let is_subscript = (=) "sub"
    let is_quotation = (=) "q"
    (* arg1: ' | " | fr | de | sp, default '"' (en) (fr = sp + nbsp's)
     * 
     * http://en.wikipedia.org/wiki/Quotation_mark_glyphs
     * http://en.wikipedia.org/wiki/Quotation_mark,_non-English_usage
     * *)

    let is_non_break_space = (=) "~"
    let is_open_brace = (=) "{"
    let is_close_brace = (=) "}"
    let is_sharp = (=) "#"
    let is_paragraph = (=) "p"
    let is_new_line = (=) "br"

    let is_utf8_char = (=) "utf" (* arg1:ocaml int *)

    let is_begin name = name = "begin"
    let is_end name = name = "end"

    let is_list name = name = "list"
    let list_style = function
        | "item" -> `itemize
        | "enum" -> `numbered
        | _ -> `itemize

    let is_item name = name = "item"

    let is_section = (=) "section"
    let section_params =
        let default = 1 in
        let level n = 
            let nn = (try int_of_string n with e -> default) in
            if 1 <= nn && nn <= 4 then nn else default
        in
        function
        | [] -> (default, "")
        | [n] -> (level n, "")
        | n :: l :: q -> (level n, l)

    let is_link = (=) "link"
    let is_local name =
        let l = String.length name in
        if l >= 6 then
            if String.sub name 0 6 = "local:" then (
                (true, String.sub name 6 (l - 6))
            ) else (false, name)
        else (false, name)

    let is_image = (=) "image"
    let image_params =
        let parse_options str =
            try 
                begin try Scanf.sscanf str "%d,%d" (fun w h -> [`w w; `h h])
                with _ -> Scanf.sscanf str "%d" (fun w -> [`w w])
                end
            with  _ -> []
        in
        function
        | [] -> ("", [], "")
        | [s] -> (s, [], "")
        | s :: o :: [] -> (s, parse_options o, "")
        | s :: o :: l :: t -> (s, parse_options o, l)

    let is_header = (=) "header"
    let is_title  = (=) "title"
    let is_authors  = (=) "author"
    let is_subtitle = (=) "subtitle"

end


let non_env_cmd_of_name name args = (
    let module C = Names in
    let ios s = try int_of_string s with e -> 0 in
    let (env:Stack.environment) =
        match name with
        | s when C.is_paragraph s        ->  `paragraph          
        | s when C.is_new_line s         ->  `new_line          
        | s when C.is_non_break_space s  ->  `non_break_space          
        | s when C.is_open_brace s       ->  `open_brace          
        | s when C.is_close_brace s      ->  `close_brace          
        | s when C.is_sharp s            ->  `sharp          
        | s when C.is_utf8_char s        -> 
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
    | `list l                    -> spr "list                   "
    | `item                      -> spr "item                   "
    | `section (n, l)            -> spr "section %d  %s     " n l
    | `link    (n, l)            -> spr "link to %s        " n 
    | `image (src, _,_)          -> spr "image %s" src
    | `header                    -> spr "header  "       
    | `title                     -> spr "title   "      
    | `authors                   -> spr "authors "        
    | `subtitle                  -> spr "subtitle"         
)
