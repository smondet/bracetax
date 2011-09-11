(******************************************************************************)
(*      Copyright (c) 2008, 2009, Sebastien MONDET                            *)
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

(** Syntaxic helper functions for {!type:Signatures.printer}s and
    {!type:GenericPrinter.output_t}s.*)

(**/**)
let (~%) = Printf.sprintf
module Error = Bracetax_error
(**/**)

(** Standard raw commands *)
module Raw = struct

  (** The four standard kinds of {i non-parsed} blocs. *)    
  type t = [ `bypass | `code | `text | `ignore ]
      
  let str_of_raw_cmd : t -> string = function
    | `bypass -> "bypass"
    | `code -> "code"
    | `text -> "text"
    | `ignore -> "ignore"
      
  let raw_cmd_of_str: string -> t = function
    | "bypass"   -> `bypass
    | "code" -> `code
    | "ignore" -> `ignore
    | "text" -> `text
    | s -> failwith (~% "Bad usage of raw_cmd_of_str: %S" s)

  let is_raw_cmd c =
    (c = "bypass") || (c = "code") || (c = "ignore") || (c = "text")


  let default_raw_end = fun _ -> "end"

end





(** Module for handling links. *)
module Link = struct

  type kind = [`local | `generic]
  type t = {
    kind: kind;
    linkto: string option;
    other_args: string list;
    contents: Buffer.t;
  }

  (** In the returned string, if it is a "local:" link, removes the
      ["local:"] part. *)
  let get_kind name =
    let l = String.length name in
    if l >= 6 then
      if String.sub name 0 6 = "local:" then (
        (`local, String.sub name 6 (l - 6))
      ) else
        (`generic, name)
    else 
      (`generic, name)
        
  let create ?(kind=`generic) ?(linkto) ?(other_args=[]) () = {
    kind = kind; linkto = linkto;
    other_args = other_args; contents = Buffer.create 42;
  }

  (** Start recording a link, it returns the {!type:Stack.environment}
      element to put in the stack, and new "output" function to
      write/record the text of the link. *)
  let start ?(url_hook=fun (s:string) -> s) args =
    let l =
      match args with
      | link :: other_args ->
          let kind, linkto = get_kind (url_hook link) in
          create ~kind ~linkto ~other_args ()
      | [] -> create ()
    in
    (`link l, Buffer.add_string l.contents)

  (** Stop recoring a link, returns the kind of link, the optional
      target, and the optional link text (already transformed). *)
  let stop link =
    let str =
      match Buffer.contents link.contents with 
      | "" -> None
      | s -> Some s
    in
    (link.kind, link.linkto, str)
      
  (** Transform back to a Bracetax string (treats the [text] as pure
      text). *)
  let unparse kind target text =
    let skind =
      match kind with `local -> "local:" | `generic -> "" in
    let starget = match target with None -> "" | Some s -> s in
    let stext = match text with None -> "" | Some s -> s in
    Printf.sprintf "{link %s%s|%s}" skind starget stext

end

(** A stack of bracetax environments. *)
module Stack = struct
  type image_size = [
  | `none
  | `wpx of int
      (*| `hpx of int*)
      (*| `whpx of int * int*)
  | `wpercent of int
      (* | `hpercent of int *)
      (* | `whpercent of int * int *)
  ]
  type environment = [
  | `cmd_end
  | `cmd_inside of environment
  | `cmd_begin of string * string list
  | `unknown of string * string list
  | `italic
  | `bold
  | `mono_space
  | `superscript
  | `subscript
  | `paragraph
  | `new_line
  | `quotation of string * string
      (* Special chars: *)
  | `non_break_space
  | `horizontal_ellipsis
  | `en_dash
  | `em_dash
  | `open_brace
  | `close_brace
  | `sharp
  | `utf8_char of int
  | `code of string list
  | `bypass
  | `text
  | `ignore
  | `list of [`itemize | `numbered ] * string list * bool ref
  | `item
  | `section of int * string
  | `link of Link.t
  | `image of string * image_size * string
  | `header
  | `title
  | `authors
  | `subtitle
  | `table of int * string option
  | `cell of bool * int * [`right | `center | `left]
  | `note
  | `quote
  ]

  type t = environment list ref

  let empty () = ref []

  let push s cmd = s := cmd :: !s
    
  let pop s = match !s with [] -> None | h :: t -> s:= t; Some h
    
  let head s = match !s with [] -> None | h :: t -> Some h

  let to_list = (!)
end

(** Operations to identify Bracetax commands, and "parse" their arguments. *)
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
  let is_en_dash = (=) "--"
  let is_em_dash = (=) "---"
  let is_open_brace = (=) "{"
  let is_close_brace = (=) "}"
  let is_sharp = (=) "#"
  let is_paragraph = (=) "p"
  let is_new_line = (=) "br"
    
  let is_utf8_char = (=) "utf" (* arg1:ocaml int *)
    
  let is_begin name = name = "begin"
  let is_end name = name = "end"
    
  let is_list name = name = "list"
  let list_style (error_msg:Error.message -> unit) = function
    | "item" -> `itemize
    | "enum" -> `numbered
    | s -> error_msg (`unknown_list_style s); `itemize
        
  let is_item name = name = "*"
    
  let is_section = (=) "section"

  (** Returns section's depth and label (maybe [""]).*)
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

  (** Get the parameters of an "image", the path (optinally treated by
      [img_hook]), the size specification, and the label (the path and the
      label may be [""]). *)
  let image_params ?(img_hook=fun s -> s) (error_msg:Error.message -> unit) =
    let parse_options str =
      (* let l = String.length l in *)
      (* if str.[l - 1] = '%' then ( *)
      try 
        begin try Scanf.sscanf str "%dpx" (fun w -> `wpx w)
        with _ -> Scanf.sscanf str "%d%%" (fun w -> `wpercent w)
        end
      with  _ -> error_msg (`bad_size_specification_in_image str); `none
    in
    function
    | [] -> ("", (`none :Stack.image_size) , "")
    | [s] -> (img_hook s, `none, "")
    | s :: o :: [] -> (img_hook s, parse_options o, "")
    | s :: o :: l :: t -> (img_hook s, parse_options o, l)

  let is_header = (=) "header"
  let is_title  = (=) "title"
  let is_authors  = (=) "authors"
  let is_subtitle = (=) "subtitle"
    
  let is_table = (=) "table"
    
  let is_cell = (=) "c"
    
  let is_note = (=) "note"
    
  let is_quote = (=) "quote"
end

(** Transforms some commands to their {!type:Stack.environment} counter part. Returns 
[`undefined (name, args)] if it is not a "simple" command. *)
let non_env_cmd_of_name name args =
  let module C = Names in
  let ios s = try int_of_string s with e -> 0 in
  let (env:Stack.environment) =
    match name with
    | s when C.is_paragraph s        ->  `paragraph          
    | s when C.is_new_line s         ->  `new_line          
    | s when C.is_non_break_space s  ->  `non_break_space          
    | s when C.is_ellipsis s  ->  `horizontal_ellipsis          
    | s when C.is_en_dash s  ->  `en_dash          
    | s when C.is_em_dash s  ->  `em_dash          
    | s when C.is_open_brace s       ->  `open_brace          
    | s when C.is_close_brace s      ->  `close_brace          
    | s when C.is_sharp s            ->  `sharp          
    | s when C.is_utf8_char s        -> 
        (try `utf8_char (ios (List.hd args)) with e -> `unknown (s, args))
    | s -> `unknown (s, args)
  in
  env

(** Converts to a (human readable) string. Used in error messages. *)
let rec env_to_string (e:Stack.environment) =
  let spr = Printf.sprintf in
  match e with
  | `cmd_end                   -> spr "cmd_end"
  | `cmd_inside  environment   -> spr "begin %s" (env_to_string environment)
  | `cmd_begin (s, l)          -> spr "cmd_begin %s" s
  | `unknown (s, l)            -> spr "unknown %s" s
  | `italic                    -> spr "italic"
  | `bold                      -> spr "bold"
  | `mono_space                -> spr "mono_space (t)"
  | `superscript               -> spr "superscript (sup)"
  | `subscript                 -> spr "subscript (sub)"
  | `paragraph                 -> spr "paragraph (p)"
  | `new_line                  -> spr "new_line (br)"
  | `quotation (s)             -> spr "quotation (q)"
  | `non_break_space           -> spr "non_break_space (~)"
  | `horizontal_ellipsis       -> spr "horizontal_ellipsis (...)"
  | `en_dash                   ->     "en dash"
  | `em_dash                   ->     "em dash"
  | `open_brace                -> spr "open_brace ({)"
  | `close_brace               -> spr "close_brace (})"
  | `sharp                     -> spr "sharp (#)"
  | `utf8_char i               -> spr "utf8_char 0x%x" i
  | `code l                    -> spr "code"
  | `bypass                    -> spr "bypass"
  | `text                      -> "text"
  | `ignore                    -> "ignore"
  | `list l                    -> spr "list"
  | `item                      -> spr "item (*)"
  | `section (n, l)            -> spr "section %d" n
  | `link l                    -> spr "link"
  | `image (src, _,_)          -> spr "image %s" src
  | `header                    -> spr "header"       
  | `title                     -> spr "title"      
  | `authors                   -> spr "authors"        
  | `subtitle                  -> spr "subtitle"         
  | `table _                   ->     "table"
  | `cell _                    ->     "cell"
  | `note                      ->     "note"
  | `quote                     ->     "quote"

(** Module to build tables. *)
module Table = struct
  (* New version target:
     {begin table 4 label defalign}
     {c lrch 2|on two columns}
     {c r3| for backward compat}
     {c r 2x3|new multirow format}
     {c _ 2x3|multirow, with default align}
     {end}
  *)
  type alignment = [`right | `center | `left ]

  type cell = {
    is_head: bool;
    cols_used: int;
    rows_used: int;
    align: alignment;
    cell_text: Buffer.t;
  }

  let default_default_align = (`left : alignment)
  type table = {
    col_nb: int;
    label: string option;
    mutable cells: cell list;
    mutable current_cell: cell option;
    caption: Buffer.t;
    default_align: alignment;
  }

  let table_args =
    let default = 1 in
    let parse_int i = try int_of_string i with e -> default in
    let parse_align str = 
      match str.[0] with
      | 'r' -> `right
      | 'l' -> `left
      | 'c' -> `center
      | _   -> (* TODO warning *) default_default_align in
    function
    | [] -> (default, None, default_default_align)
    | s :: [] -> (parse_int s, None, default_default_align)
    | s :: l :: [] -> (parse_int s, Some l, default_default_align)
    | s :: l :: a :: _ -> (parse_int s, Some l, parse_align a)

  let cell_arguments tab args =
    let parse_1arg str =
      let head = ref false in
      let cols = ref 1 in
      let alig = ref tab.default_align in
      let rec p str =
        if str = "" then ()
        else (
          try Scanf.sscanf
            str "%d%s" (fun c s -> cols := c; p s)
          with
            e ->
              begin match str.[0] with
              | 'h' -> head := true;
              | 'r' -> alig := `right;
              | 'l' -> alig := `left;
              | 'c' -> alig := `center;
              | '_' -> ()
              | _   -> (* TODO warning *) ()
              end;
              p (String.sub str 1 (String.length str - 1));
        );
      in
      p str;
      (!head, 1, !cols, !alig) in
    let parse_2args a1 a2 =
      let h, _, _, a = parse_1arg a1 in
      let r, c =
        try 
          (try Scanf.sscanf a2 "%dx%d" (fun r c -> (r, c))
           with e -> Scanf.sscanf a2 "%d" (fun c -> (1, c)))
        with e ->
          (* TODO: Warning *) (1, 1)
      in
      (h, r, c, a) in
    match args with
    | [] -> (false, 1, 1, tab.default_align)
    | arg :: [] ->
        (parse_1arg arg)
    | arg1 :: arg2 :: _ ->
        (parse_2args arg1 arg2)

  let write table str =
    let the_buffer =
      match table.current_cell with
      | None -> table.caption
      | Some c -> c.cell_text
    in
    Buffer.add_string the_buffer str;
    ()
      
  let start args =
    let col_nb, label, def_alig = table_args args in
    let table = {
      col_nb = col_nb;
      label = label;
      cells = [];
      current_cell = None;
      caption = Buffer.create 64;
      default_align = def_alig;
    } in
    (table, `table (col_nb, label), write table)
      
  let cell_start ~loc ~(error:Error.error_fun) tab args =
    let head, rnb, cnb, align = cell_arguments tab args in
    let def_cell = `cell (head, cnb, align) in
    begin match tab.current_cell with
    | Some c -> 
        error (Error.mk loc `error `cell_inside_cell);
        def_cell
    | None ->
        let cell_t = {
          is_head= head;
          cols_used = cnb;
          rows_used = rnb;
          align = align;
          cell_text = Buffer.create 64;
        } in
        tab.current_cell <- Some cell_t;
        def_cell
    end
      

  let cell_stop ~loc ~(error:Error.error_fun) tab =
    begin match tab.current_cell with
    | Some c -> 
        tab.cells <- c :: tab.cells;
        tab.current_cell <- None;
    | None ->
        (* already errored *)
        ()
    end
      

  (** Utilities for printing tables without too much headache. *)
  module Util = struct

    let nb_rows table =
      let nb_cells = 
        List.fold_left
          (fun sum cell ->
             sum + (cell.cols_used * cell.rows_used))
          0 table.cells in
      (nb_cells / table.col_nb)
        

    let make_riddle table =
      let rows, cols = nb_rows table, table.col_nb in
      (* one row too much to simplify next_coordinates *)
      Array.make_matrix (rows + 1) (cols) (-1, -1)

    let fill_riddle riddle from_row from_col offset_row offset_col =
      for i = 1 to offset_row do
        for j = 1 to offset_col do
          riddle.(from_row + i - 1).(from_col + j - 1) <- 
            (from_row, from_col);
        done;
      done;
      ()

    (** Find next false in riddle *)
    let next_coordinates riddle table prev_row prev_col =
      let row = ref prev_row in
      let col = ref prev_col in
      let next_coord () =
        if !col = table.col_nb - 1 then
          (col := 0; incr row;) else (incr col;) in
      next_coord ();
      while riddle.(!row).(!col) <> (-1, -1) do
        next_coord ();
      done;
      (!row, !col)
        


    type in_table = [
    | `none
    | `cell of cell
    | `filed of int * int
    ]
    let matrix_next_coordinates mat table prev_row prev_col =
      let row = ref prev_row in
      let col = ref prev_col in
      let next_coord () =
        if !col = table.col_nb - 1 then
          (col := 0; incr row;) else (incr col;) in
      next_coord ();
      while mat.(!row).(!col) <> `none do
        next_coord ();
      done;
      (!row, !col)
        
    let cells_to_matrix table = 
      let rows, cols = nb_rows table, table.col_nb in
      let mat = Array.make_matrix (rows + 1) (cols) `none in
      let rec iter_on_cells cur_row cur_col = function
        | [] -> ()
        | cell :: t ->
            (* Printf.eprintf "===mat.(%d).(%d) <- `cell str_cell;\n%!" cur_row cur_col; *)
            (* Printf.eprintf "   rows: %d, cols: %d\n" cell.rows_used cell.cols_used; *)
            for i = 0 to cell.rows_used - 1 do
              for j = 0 to cell.cols_used - 1 do
                (* Printf.eprintf "     at.( %d + %d , %d + %d\n" cur_row i cur_col j; *)
                if i = 0 && j = 0 then (
                  mat.(cur_row).(cur_col) <- `cell cell;
                ) else (
                  mat.(cur_row + i).(cur_col + j) <- 
                    `filled (cur_row, cur_col);
                );
              done;
            done;
            let next_row, next_col = 
              matrix_next_coordinates mat table cur_row cur_col in
            iter_on_cells next_row next_col t
      in
      iter_on_cells 0 0 (List.rev table.cells);
      (rows, cols, mat)
        

  end


end
