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

(** The (X)HTML output format, implements {!type:Signatures.printer}
    functions. *)

(**/**)

type t = {
  stack: Commands.Stack.t;
  mutable write: string -> unit;
  write_mem: (string -> unit) Stack.t;
  mutable current_line: int;
  mutable started_text: bool;
  mutable inside_header:bool;
  mutable current_table: Commands.Table.table option;
  mutable current_section: (int * string * Buffer.t) option;
  error: Error.error -> unit;
  mutable loc: Error.location;
  class_hook: string option;
  url_hook: string -> string;
  img_hook: string -> string;
  separate_header: (string * string * string) ref option;
  make_section_links: [ `never | `when_labeled | `always ];
}

module CS = Commands.Stack

let (~%) = Printf.sprintf

module AddClass = struct
  let name add style =
    match add with
    | None -> ""
    | Some s -> ~% " %s%s" s style
  
  let attribute add style =
    match add with
    | None -> ""
    | Some s -> ~% " class=\"%s%s\"" s style

end

let create
    ~writer ?class_hook ?separate_header
    ?(make_section_links=`when_labeled)
    ?(img_hook=fun s -> s) ?(url_hook=fun s -> s) () =
  let module S = Signatures in
  let write = writer.S.w_write in
  {
    stack = CS.empty ();
    write = write;
    write_mem = Stack.create ();
    current_line = 1;
    started_text = false;
    inside_header = false;
    current_table = None;
    current_section = None;
    error = writer.S.w_error;
    loc = {Error.l_line = -1; l_char = -1; l_file = "NO FILE";};
    class_hook = class_hook;
    url_hook = url_hook;
    img_hook = img_hook;
    separate_header = separate_header;
    make_section_links = make_section_links;
  }
    

let strstat s = (~% "[%d:%d]" s.Error.l_line s.Error.l_char)
let debugstr t s msg = 
  if false then
    (~% "<!--DEBUG:[%s] Loc:[%d;%d] CurLine:%d-->"
       msg s.Error.l_line s.Error.l_char t.current_line)
  else
    ""

let sanitize_comments line =
  let patterns = [('<', "LT"); ('>', "GT"); ('&', "AMP"); ('-', "DASH")] in
  Escape.replace_chars ~src:line ~patterns
    (* let src = Escape.replace_string ~src:line ~find:"-->" ~replace_with:"XXX" in *)
    (* Escape.replace_string ~src ~find:"<!--" ~replace_with:"XXXX" *)

let sanitize_pcdata line =
  let patterns = [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;")] in
  Escape.replace_chars ~src:line ~patterns

let sanitize_xml_attribute src =
  let patterns =
    [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;"); ('"', "&quot;")] in
  Escape.replace_chars ~src ~patterns


let quotation_open_close t a =
  let default = ("&ldquo;", "&rdquo;") in
  try
    match List.hd a with
    | "'"  -> ("&lsquo;", "&rsquo;")
    | "en" -> ("&ldquo;", "&rdquo;")
    | "fr" -> ("&laquo;&nbsp;", "&nbsp;&raquo;")
    | "de" -> ("&bdquo;", "&rdquo;")
    | "es" -> ("&laquo;", "&raquo;")
    | s    -> 
        t.error (Error.mk t.loc `warning (`unknown_quotation_style s));
        default
  with
  | e -> default

let list_start t =
  function `itemize -> ~% "\n<ul%s>\n" (AddClass.attribute t.class_hook "ul")
  | `numbered -> ~%  "\n<ol%s>\n" (AddClass.attribute t.class_hook "ol")
let list_item t = function
  | `itemize -> ~% "</li>\n<li%s>" (AddClass.attribute t.class_hook "li")
  | `numbered -> ~% "</li>\n<li%s>"  (AddClass.attribute t.class_hook "li")
let list_firstitem t = 
  function `itemize -> ~% "<li%s>" (AddClass.attribute t.class_hook "li")
  | `numbered -> ~% "<li%s>" (AddClass.attribute t.class_hook "li")
let list_stop t = 
  function `itemize -> "</li>\n</ul>\n" | `numbered -> "</li>\n</ol>\n"

let section_start t n l =
  let buf = Buffer.create 42 in
  match t.current_section with
  | Some _ ->
    failwith "Nested Sections not allowed"
  | None ->
    t.current_section <- Some (n, l, buf);
    Stack.push t.write t.write_mem;
    t.write "</div>\n";
    t.write <- Buffer.add_string buf;
    ()

let section_stop t n l =
  begin match t.current_section with
  | None -> failwith "fatal error: section_stop with no section_start"
  | Some (n, l, buf) ->
    let lsan =
      match t.make_section_links, sanitize_xml_attribute l with
      | `never, _ -> ""
      | `when_labeled, "" -> "" 
      | `when_labeled, s -> ~% " id=\"%s\"" s
      | `always, "" -> ~% " id=\"%s\"" (Escape.clean_string (Buffer.contents buf))
      | `always, s  -> ~% " id=\"%s\"" s
    in
    t.write <- Stack.pop t.write_mem;
    t.current_section <- None;
    let tag = ~% "h%d" (n + 1) in
    ~% "<%s%s%s>%s</%s>\n<div class=\"p%s\">"
      tag lsan (AddClass.attribute t.class_hook tag)
      (Buffer.contents buf)
      tag (AddClass.name t.class_hook "p")
  end

let link_start t args =
  let link, new_write = Commands.Link.start ~url_hook:t.url_hook args in
  Stack.push t.write t.write_mem;
  t.write <- new_write;
  link

let link_stop t l =
  t.write <- Stack.pop t.write_mem;
  let kind, target, text = Commands.Link.stop l in
  let target_str = 
    (match target with Some s -> s | None -> "#") in
  t.write
    (~% "<a href=\"%s%s\"%s>%s</a>" 
       (match kind with `local -> "#" | `generic -> "")
       (sanitize_xml_attribute target_str)
       (AddClass.attribute t.class_hook "a")
       (match text with Some s -> s | None -> sanitize_pcdata target_str));
  ()

let image_start t args =
  (* http://www.w3.org/Style/Examples/007/figures *)
  let error_msg m = t.error (Error.mk t.loc `error m) in
  let src, opts, lbl =
    Commands.Names.image_params ~img_hook:t.img_hook error_msg args in
  let opts_str =
    match opts with
    | `wpx px -> (~% "width=\"%dpx\""  px)
    | `wpercent w -> (~% "width=\"%d%%\"" w)
    | `none -> "" 
  in
  let sansrc =
    match sanitize_xml_attribute src with
      "" -> "http://IMAGEWITHNOSOURCE" | s -> s in
  let sanlbl =
    match sanitize_xml_attribute lbl with 
    | "" -> "" | s -> ~% "id=\"%s\" " s in
  t.write
    (~% "\n<div class=\"figure%s\" %s>\n  <a href=\"%s\">\
        \n    <img src=\"%s\" %s %s alt=\"%s\"%s/>\n  </a><br/>\n"
       (AddClass.name t.class_hook "figure")
        sanlbl sansrc sansrc opts_str sanlbl sansrc
        (AddClass.attribute t.class_hook "img"));
  `image (src, opts, lbl)

let image_stop = "</div>"

let header_start t =
  t.inside_header <- true; 
  begin match t.separate_header with
  | None ->
      t.write (~% "%s\n<div class=\"header%s\">\n"
                 (if t.started_text then "</div>" else "")
                 (AddClass.name t.class_hook "header"))
  | Some r ->
      Stack.push t.write t.write_mem;
      t.write <- (fun str -> 
                    match CS.head t.stack with
                    | Some `title    -> let t,a,s = !r in r := (t ^ str, a, s);
                    | Some `authors  -> let t,a,s = !r in r := (t, a ^ str, s);
                    | Some `subtitle -> let t,a,s = !r in r := (t, a, s ^ str);
                    | _ -> ());
  end

let header_stop t =
  t.inside_header <- false;
  begin match t.separate_header with
  | None ->
      t.started_text <- true; (* we put the <p> *)
      t.write (~% "</div> <!-- END HEADER -->\n<div class=\"p%s\">\n"
                 (AddClass.name t.class_hook "p"))
  | Some r ->
      t.write <- Stack.pop t.write_mem;
  end

let title_start t =
  t.write (~% "\n  <h1%s>" (AddClass.attribute t.class_hook "h1"))
let title_stop t = t.write "</h1>\n"
let authors_start t =
  t.write (~% "  <div class=\"authors%s\">"
             (AddClass.name t.class_hook "authors"))
let authors_stop t = t.write "</div>\n"
let subtitle_start t =
  t.write (~% "  <div class=\"subtitle%s\">"
             (AddClass.name t.class_hook "subtitle"))
let subtitle_stop t = t.write "</div>\n"

let table_start t args =
  (* http://www.topxml.com/xhtml/articles/xhtml_tables/ *)
  let table, to_stack, new_write = Commands.Table.start args in
  t.current_table <- Some table;
  Stack.push t.write t.write_mem;
  t.write <- new_write;
  to_stack

let print_table t table =
  let module CT = Commands.Table in
  let write = t.write in
  let lbl_str =
    match table.CT.label with
    | None -> ""
    | Some s -> (~% "id=\"%s\"" (sanitize_xml_attribute s))
  in
  write
    (~% "<div class=\"tablefigure%s\">\n\
         <table class=\"tablefigure%s\"  border=\"1\" %s >\n"
       (AddClass.name t.class_hook "tablefigure")
         (AddClass.name t.class_hook "table")
         lbl_str);
  let caption_str = (Buffer.contents table.CT.caption) in
  if not (Escape.is_white_space caption_str) then (
    write (~% "<caption  class=\"tablefigure%s\" %s>%s</caption>\n<tr>"
             (AddClass.name t.class_hook "p")
             lbl_str caption_str);
  );

  let riddle = CT.Util.make_riddle table in

  let rec write_cells cells cur_row cur_col =
    match cells with
    | [] -> (* fill the gap + warning *)
        ()
    | c :: tl ->
        let typ_of_cell = if c.CT.is_head then "h" else "d" in
        let alignement =
          match c.CT.align with
          | `right ->
              ~% "class=\"rightalign%s\" style=\"text-align:right;\""
                (AddClass.name t.class_hook "cellrightalign")
          | `center ->
              ~% "class=\"centeralign%s\" style=\"text-align:center;\""
                (AddClass.name t.class_hook "cellcenteralign")
          | `left ->
              ~% "class=\"leftalign%s\" style=\"text-align:left;\""
                (AddClass.name t.class_hook "cellleftalign")
        in
        write (~% "<t%s  rowspan=\"%d\" colspan=\"%d\" %s >%s</t%s>"
                 typ_of_cell c.CT.rows_used c.CT.cols_used alignement
                 (Buffer.contents c.CT.cell_text)
                 typ_of_cell);
        CT.Util.fill_riddle riddle
          cur_row cur_col c.CT.rows_used c.CT.cols_used;
        let next_row, next_col = 
          CT.Util.next_coordinates riddle table cur_row cur_col in
        if cur_row <> next_row then (
          write "</tr>\n";
          if tl <> [] then
            write (~% "<tr%s>" (AddClass.attribute t.class_hook "tr"));
        );
        write_cells tl next_row next_col
  in
  write_cells (List.rev table.CT.cells) 0 0;
  write "</table></div>\n"


let table_stop t =
  begin match t.current_table with
  | None -> failwith "Why am I here ??? no table to end."
  | Some tab ->
      (* p (~% "End of table: %s\n" (Buffer.contents tab.caption)); *)
      t.write <- Stack.pop t.write_mem;
      t.current_table <- None;
      print_table t tab;
  end

let cell_start t args =
  begin match t.current_table with
  | None ->
      t.error (Error.mk t.loc `error `cell_out_of_table);
      `cell (false, 1, `center)
  | Some tab ->
      Commands.Table.cell_start ~loc:t.loc ~error:t.error tab args
  end

let cell_stop t env =
  match t.current_table with
  | None -> (* Already warned *) ()
  | Some tab -> Commands.Table.cell_stop ~loc:t.loc ~error:t.error tab

let note_start t =
  t.write
    (~% "<small class=\"notebegin%s\">(</small><small class=\"note%s\">"
       (AddClass.name t.class_hook "notebegin")
       (AddClass.name t.class_hook "note"));
  `note

let note_stop t =
  ~% "</small><small class=\"noteend%s\">)</small>"
    (AddClass.name t.class_hook "noteend")

let may_start_text t =
  if not t.started_text && not t.inside_header then (
    t.started_text <- true;
    t.write (~% "<div class=\"p%s\">" (AddClass.name t.class_hook "p"));
  )

let start_environment ?(is_begin=false) t location name args =
  t.loc <- location;
  let module C = Commands.Names in
  let cmd name args =
    match name with
    | s when C.is_header s -> (header_start t); `header
    | s when C.is_title s ->  (title_start t); `title
    | s when C.is_subtitle s -> (subtitle_start t); `subtitle
    | s when C.is_authors s -> (authors_start t); `authors
    | _ ->
        may_start_text t;
        begin match name with
        | s when C.is_quotation s        ->
            let op, clo = quotation_open_close t args in
            t.write op;
            `quotation (op, clo)
        | s when C.is_italic s ->
            t.write (~% "<i%s>" (AddClass.attribute t.class_hook "i"));
            `italic
        | s when C.is_bold s ->
            t.write (~% "<b%s>" (AddClass.attribute t.class_hook "b"));
            `bold
        | s when C.is_mono_space s ->
            t.write (~% "<tt%s>" (AddClass.attribute t.class_hook "tt"));
            `mono_space
        | s when C.is_superscript s ->
            t.write (~% "<sup%s>" (AddClass.attribute t.class_hook "sup"));
            `superscript
        | s when C.is_subscript s ->
            t.write (~% "<sub%s>" (AddClass.attribute t.class_hook "sub"));
            `subscript
        | s when (C.is_end s)           -> `cmd_end
        | s when C.is_list s             ->
            let style, other_args, waiting =
              let error_msg m = t.error (Error.mk t.loc `error m) in
              match args with
              | [] -> (`itemize, [], ref true)
              | h :: t -> (C.list_style error_msg h, t, ref true) in
            t.write (list_start t style);
            `list (style, other_args, waiting)
        | s when C.is_item s -> `item
        | s when C.is_section s -> 
            let level, label = C.section_params args in
            section_start t level label;
            `section (level, label)
        | s when C.is_link s -> (link_start t args)
        | s when C.is_image s -> image_start t args
        | s when C.is_table s -> table_start t args
        | s when C.is_cell s -> cell_start t args
        | s when C.is_note s -> note_start t
        | s when C.is_quote s ->
            t.write (~% "<blockquote%s>" (AddClass.attribute t.class_hook "quote"));
            t.write (~% "<div class=\"p%s\">" (AddClass.name t.class_hook "p"));
            `quote
        | s ->
            t.error (Error.mk t.loc `error (`unknown_command  s));
            `unknown (s, args)
        end
  in
  let the_cmd =
    if C.is_begin name then (
      match args with
      | [] ->
          t.error (Error.mk t.loc `error `begin_without_arg);
          (`cmd_begin ("", []))
      | h :: t -> (`cmd_begin (h, t))
    ) else (
      cmd name args
    )
  in
  if is_begin then (
    CS.push t.stack (`cmd_inside the_cmd);
  ) else (
    CS.push t.stack the_cmd;
  )

let start_command t location name args =
  t.loc <- location;
  (* p (~% "Command: \"%s\"(%s)\n" name (String.concat ", " args)); *)
  match Commands.non_env_cmd_of_name name args with
  | `unknown (name, args) -> start_environment t location name args
  | cmd -> CS.push t.stack cmd

let stop_command t location =
  t.loc <- location;
  let rec out_of_env env =
    match env with
    | `cmd_end ->
        begin match CS.pop t.stack with
        | Some (`cmd_inside benv) ->
            (* p (~% "{end} %s\n" (Commands.env_to_string benv)); *)
            out_of_env benv
        | Some c ->
            t.error (Error.mk t.loc `error `non_matching_end);
            CS.push t.stack c;
        | None ->
            t.error (Error.mk t.loc `error `non_matching_end);
        end
    | `cmd_begin (nam, args) ->
        (* p (~% "cmd begin %s(%s)\n" nam (String.concat ", " args)); *)
        start_environment ~is_begin:true t location nam args;
    | `paragraph ->
        t.write (~% "</div>\n<div class=\"p%s\">"
                   (AddClass.name t.class_hook "p"))
    | `new_line -> 
        t.write (~% "<br%s/>\n" (AddClass.attribute t.class_hook "br"))
    | `non_break_space -> t.write "&nbsp;"
    | `horizontal_ellipsis -> t.write "&hellip;"
    | `en_dash -> t.write "&ndash;"
    | `em_dash -> t.write "&mdash;"
    | `open_brace -> t.write "{"
    | `close_brace -> t.write "}"
    | `sharp -> t.write "#"
    | (`utf8_char i) -> t.write (~% "&#%d;" i)
    | (`quotation (op, clo)) -> t.write clo
    | `italic       ->  t.write "</i>"  
    | `bold         ->  t.write "</b>"  
    | `mono_space   ->  t.write "</tt>" 
    | `superscript  ->  t.write "</sup>"
    | `subscript    ->  t.write "</sub>"
    | `list (style, _, r) -> t.write (list_stop t style)
    | `item ->
        begin match CS.head t.stack with
        | Some (`list (style, _, r))
        | Some (`cmd_inside (`list (style, _, r))) ->
            if !r then (
              t.write (list_firstitem t style);
              r := false;
            ) else (
              t.write (list_item t style);
            );
        | Some c ->
            t.error (Error.mk t.loc `error `item_out_of_list);
            CS.push t.stack c;
        | None ->
            t.error (Error.mk t.loc `error `item_out_of_list);
        end
    | `section (level, label) ->
      let section = (section_stop t level label) in
      t.write section;
    | `link l -> link_stop t l;
    | `image _ -> t.write image_stop;
    | `header ->  (header_stop t);
    | `title -> title_stop t;
    | `subtitle -> subtitle_stop t;
    | `authors -> authors_stop t;
    | `table _ -> table_stop t
    | `cell _ as c -> cell_stop t c
    | `note -> t.write (note_stop t)
    | `quote ->  t.write "</div></blockquote>"
    | `cmd_inside c ->
        t.error (Error.mk t.loc `error `closing_brace_matching_begin);
    | `unknown c -> () (* Already "t.error-ed" in start_environment *)
    | c -> (* shouldn't be there !! *)
        t.error (Error.mk t.loc `fatal_error 
                   (`transformer_lost (Commands.env_to_string c)));
  in
  match CS.pop t.stack with
  | Some env -> out_of_env env
  | None ->
      t.error (Error.mk t.loc `error `nothing_to_end_with_brace)


let handle_comment_line t location line =
  t.loc <- location;
  t.write (~% "%s<!--%s-->\n" (debugstr t location "Comment")
             (sanitize_comments line));
  t.current_line <- t.current_line + 1;
  ()

let handle_text t location line =
  t.loc <- location;
  if not (Escape.is_white_space line) then (
    may_start_text t;
  );
  
  if (t.started_text && (not t.inside_header)) ||
    (t.inside_header && (CS.head t.stack <> Some `header)) then (

      let debug = debugstr t location "Text" in
      let pcdata = sanitize_pcdata line in
      if location.Error.l_line > t.current_line then (
        t.write (~% "%s%s" debug pcdata);
        t.current_line <- location.Error.l_line;
      ) else (
        t.write (~% "%s%s" debug pcdata);
      )
    ) else (
      if
        CS.head t.stack = Some `header
        && (not (Escape.is_white_space line))
      then (
        t.write (~% "<!-- IGNORED TEXT: %s -->" (sanitize_comments line));
      );

    )


let terminate t location =
  t.loc <- location;
  if (CS.to_list t.stack) <> [] then (
    let l = List.map Commands.env_to_string (CS.to_list t.stack) in
    t.error (Error.mk t.loc `error (`terminating_with_open_environments l));
  );  
  t.write (if t.started_text then "</div>\n" else "");
  () 


let start_raw_mode t location kind_str args =
  t.current_line <- location.Error.l_line;
  let kind = Commands.Raw.raw_cmd_of_str kind_str in
  begin match kind with
  | `code ->
      CS.push t.stack (`code args);
      begin match args with
      | _ :: q :: _ ->
        t.write (~% "\n<!--verbatimbegin:%s -->\n" (sanitize_comments q))
      | _ -> ()
      end;
      t.write (~% "<pre%s>"  (AddClass.attribute t.class_hook "pre"));
  | `bypass | `text | `ignore as env_kind ->
      CS.push t.stack env_kind;
  end

let handle_raw_text t location text =
  t.current_line <- location.Error.l_line;
  begin match CS.head t.stack with
  | Some (`code _) | Some `text ->
      let pcdata = sanitize_pcdata text in
      t.write (~% "%s" pcdata);
  | Some `bypass ->
      t.write text;
  | Some `ignore -> ()
  | _ ->
      failwith "handle_raw_text: Shouldn't be there, Parser's fault ?";
  end
  
let stop_raw_mode t location =
  t.current_line <- location.Error.l_line;
  begin match CS.pop t.stack with
  | Some (`code args) ->
      t.write "</pre>";
      begin match args with
      | _ :: q :: _ ->
        t.write (~% "\n<!--verbatimend:%s -->\n" (sanitize_comments q))
      | _ -> ()
      end;
  | Some `bypass | Some `text | Some `ignore -> ()
  | _ ->
      (* warning ? error ? anyway, *)
      failwith "Shouldn't be there, Parser's fault ?";
  end

(**/**)

(** Build a [printer] to feed {!val:Parser.do_transformation}, the
optional arguments have the same meaning than for
{!val:Transform.brtx_to_html}. *)
let build ?(print_comments=false)
    ?make_section_links
    ?separate_header ?img_hook ?url_hook ?class_hook ~writer () =
  let t =
    create ~writer ?make_section_links
      ?class_hook ?separate_header ?img_hook ?url_hook () in
  { Signatures.
      print_comment =
      if print_comments then 
        (handle_comment_line t)
      else 
        (fun a b -> ());
    print_text =    handle_text t;
    enter_cmd =     start_command t;
    leave_cmd =     stop_command t;
    terminate =     terminate t;
    is_raw = Commands.Raw.is_raw_cmd;
    default_raw_end = Commands.Raw.default_raw_end;
    enter_raw =     start_raw_mode t;
    print_raw =     handle_raw_text t;
    leave_raw =     stop_raw_mode t;
    error = writer.Signatures.w_error; }

(** Build an HTML header. *)
let header ?(title="") ?(comment="") ?stylesheet_link () =
  let css_str =
    match stylesheet_link with
    | None -> ""
    | Some f ->
        ~% "<link rel=\"stylesheet\"  type=\"text/css\" href=\"%s\" />\n"
          (sanitize_xml_attribute f)
  in
  ~% "<!DOCTYPE html
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
    <!-- %s -->
    <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    %s<title>%s</title>
    </head>
    <body>" (sanitize_comments comment) css_str (sanitize_pcdata title)

(** Close an HTML document. *)
let footer () = "</body>\n</html>\n"

