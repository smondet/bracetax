(******************************************************************************)
(*      Copyright (c) 2008, Sebastien MONDET                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

let (~%) = Printf.sprintf

module Link = struct

    type kind = [`local | `generic]
    type t = {
        kind: kind;
        linkto: string option;
        other_args: string list;
        contents: Buffer.t;
    }

    let get_kind name = (
        let l = String.length name in
        if l >= 6 then
            if String.sub name 0 6 = "local:" then (
                (`local, String.sub name 6 (l - 6))
            ) else
                (`generic, name)
        else 
            (`generic, name)
    )
    let create ?(kind=`generic) ?(linkto) ?(other_args=[]) () = {
        kind = kind; linkto = linkto;
        other_args = other_args; contents = Buffer.create 42;
    }

    let start args = (
        let l =
            match args with
            | link :: other_args ->
                let kind, linkto = get_kind link in
                create ~kind ~linkto ~other_args ()
            | [] -> create ()
        in
        (`link l, Buffer.add_string l.contents)
    )
    let stop link = (
        let str =
            match Buffer.contents link.contents with 
            | "" -> None
            | s -> Some s
        in
        (link.kind, link.linkto, str)
    )

end

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
        | `horizontal_ellipsis
        | `open_brace
        | `close_brace
        | `sharp
        | `utf8_char of int
        | `verbatim of string list
        | `list of [`itemize | `numbered ] * string list * bool ref
        | `item
        | `section of int * string
        | `link of Link.t
        | `image of string * [`w of int | `h of int ] list * string
        | `header
        | `title
        | `authors
        | `subtitle
        | `table of int * string option
        | `cell of bool * int * [`right | `center | `left | `default]
        | `note
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
    let is_ellipsis = (=) "..."
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

    let is_item name = name = "*"

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
    let is_authors  = (=) "authors"
    let is_subtitle = (=) "subtitle"

    let is_table = (=) "table"

    let is_cell = (=) "c"

    let is_note = (=) "note"

end


let non_env_cmd_of_name name args = (
    let module C = Names in
    let ios s = try int_of_string s with e -> 0 in
    let (env:Stack.environment) =
        match name with
        | s when C.is_paragraph s        ->  `paragraph          
        | s when C.is_new_line s         ->  `new_line          
        | s when C.is_non_break_space s  ->  `non_break_space          
        | s when C.is_ellipsis s  ->  `horizontal_ellipsis          
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
    | `horizontal_ellipsis       -> spr "horizontal_ellipsis    "
    | `open_brace                -> spr "open_brace             "
    | `close_brace               -> spr "close_brace            "
    | `sharp                     -> spr "sharp                  "
    | `utf8_char i               -> spr "utf8_char i            "
    | `verbatim l                -> spr "verbatim l             "
    | `list l                    -> spr "list                   "
    | `item                      -> spr "item                   "
    | `section (n, l)            -> spr "section %d  %s     " n l
    | `link l                    -> spr "link          "
    | `image (src, _,_)          -> spr "image %s" src
    | `header                    -> spr "header  "       
    | `title                     -> spr "title   "      
    | `authors                   -> spr "authors "        
    | `subtitle                  -> spr "subtitle"         
    | `table _                   ->     "table"
    | `cell _                    ->     "cell"
    | `note                      ->     "note"
)

module Table = struct
    type cell = {
        is_head: bool;
        cols_used: int;
        align: [`right | `center | `left | `default];
        cell_text: Buffer.t;
    }

    type table = {
        col_nb: int;
        label: string option;
        mutable cells: cell list;
        mutable current_cell: cell option;
        caption: Buffer.t;
    }

    let table_args =
        let default = 1 in
        let parse_int i = try int_of_string i with e -> default in
        function
        | [] -> (default, None)
        | [s] -> (parse_int s, None)
        | s :: o :: t -> (parse_int s, Some o)

    let cell_args args =
        let head = ref false in
        let cols = ref 1 in
        let alig = ref `default in
        let rec parse_str str =
            if str = "" then ()
            else (
                try Scanf.sscanf str "%d%s" (fun c s -> cols := c; parse_str s)
                with
                e ->
                    begin match str.[0] with
                    | 'h' -> head := true;
                    | 'r' -> alig := `right;
                    | 'l' -> alig := `left;
                    | 'c' -> alig := `center;
                    | _   -> ()
                    end;
                    parse_str (String.sub str 1 (String.length str - 1));
            )
        in
        if args <> [] then (
            parse_str (List.hd args);
        );
        (!head, !cols, !alig)

    let write table str = (
        let the_buffer =
            match table.current_cell with
            | None -> table.caption
            | Some c -> c.cell_text
        in
        Buffer.add_string the_buffer str;
    )
    
    let start args = (
        let col_nb, label = table_args args in
        let table = {
            col_nb = col_nb;
            label = label;
            cells = [];
            current_cell = None;
            caption = Buffer.create 64;
        } in
        (table, `table (col_nb, label), write table)
    )
    let cell_start ~warn tab args = (
        let head, cnb, align = cell_args args in
        let def_cell = `cell (head, cnb, align) in
        begin match tab.current_cell with
        | Some c -> 
            warn (~% "Warning: no use for a cell inside a cell !\n");
            def_cell
        | None ->
            let cell_t = {
                is_head= head;
                cols_used = cnb;
                align = align;
                cell_text = Buffer.create 64;
            } in
            tab.current_cell <- Some cell_t;
            def_cell
        end
    )

    let cell_stop ~warn tab = (
        begin match tab.current_cell with
        | Some c -> 
            tab.cells <- c :: tab.cells;
            tab.current_cell <- None;
        | None ->
            warn "Should not be there... unless you already know you shouldn't \
                cells put in cells...\n";
        end
    )



end
