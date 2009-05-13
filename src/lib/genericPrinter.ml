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

type write_fun = string -> unit
type string_of_args_fun = string list -> string
type string_fun = unit -> string
type transform_string_fun = string -> string

type output_t = {

    start_text: string_fun;
    terminate: string_fun;
    start_verbatim: string option -> string;
    verbatim_line: transform_string_fun;
    stop_verbatim: string option -> string;

    line: transform_string_fun;
    comment_line: transform_string_fun;

    quotation_open_close: string list -> (string * string);
    
    start_italic: string_fun;
    start_bold:   string_fun;
    start_type:   string_fun;
    start_sup:    string_fun;
    start_sub:    string_fun;
    stop_italic:  string_fun;
    stop_bold:    string_fun;
    stop_type:    string_fun;
    stop_sup:     string_fun;
    stop_sub:     string_fun;

    list_start: [`itemize | `numbered ] -> string;
    list_first_item: [`itemize | `numbered ] -> string;
    list_item: [`itemize | `numbered ] -> string;
    list_stop: [`itemize | `numbered ] -> string;

    section_start : int -> string -> string;
    section_stop : int -> string -> string;

    paragraph : string_fun;
    new_line : string_fun;
    non_break_space : string_fun;
    horizontal_ellipsis : string_fun;
    open_brace : string_fun;
    close_brace : string_fun;
    sharp : string_fun;

    utf8_char: int -> string;

    link: Commands.Link.kind -> string option -> string option -> string;

    start_header: string_fun;
    start_title: string_fun;
    start_authors: string_fun;
    start_subtitle: string_fun;
    stop_header: string_fun;
    stop_title: string_fun;
    stop_authors: string_fun;
    stop_subtitle: string_fun;

    start_image: string -> Commands.Stack.image_size -> string -> string; 
    stop_image : string -> Commands.Stack.image_size -> string -> string; 

    print_table: (string -> unit) -> Commands.Table.table -> unit;

    start_note: string_fun;
    stop_note: string_fun;

}

type t = {
    stack: Commands.Stack.t;
    mutable write: write_fun;
    write_mem: write_fun Stack.t;
    mutable started_text: bool;
    mutable inside_header:bool;
    mutable current_table: Commands.Table.table option;
    error: Error.error -> unit;
    mutable loc: Error.location;
    mutable output: output_t;
}

type aux = output_t

module CS = Commands.Stack

let spr = Printf.sprintf

let create ~writer output =  (
    let module S = Signatures in
    let write = writer.S.w_write in
    {
        stack = CS.empty ();
        write = write;
        write_mem = Stack.create ();
        started_text = false;
        inside_header = false;
        current_table = None;
        error = writer.S.w_error;
        loc = {Error.l_line = -1; l_char = -1;l_file = "NO FILE";};
        output = output;
    }
)


let start_environment ?(is_begin=false) t location name args = (
    t.loc <- location;
    let o = t.output in
    let wr = t.write in
    let module C = Commands.Names in
    let cmd name args =
        match name with
        | s when C.is_quotation s        ->
            let op, clo = o.quotation_open_close args in
            t.write op;
            `quotation (op, clo)
        | s when C.is_italic s      -> wr (o.start_italic ()); `italic
        | s when C.is_bold s        -> wr (o.start_bold   ()); `bold
        | s when C.is_mono_space s  -> wr (o.start_type   ()); `mono_space
        | s when C.is_superscript s -> wr (o.start_sup    ()); `superscript
        | s when C.is_subscript s   -> wr (o.start_sub    ()); `subscript
        | s when C.is_end s -> `cmd_end
        | s when C.is_list s ->
            let style, other_args, waiting =
                let error_msg m = t.error (Error.mk t.loc `error m) in
                match args with
                | [] -> (`itemize, [], ref true)
                | h :: t -> (C.list_style error_msg h, t, ref true) in
            wr (o.list_start style);
            `list (style, other_args, waiting)
        | s when C.is_item s -> `item
        | s when C.is_section s -> 
            let level, label = C.section_params args in
            wr (o.section_start level label);
            `section (level, label)
        | s when C.is_link s ->
            let link, new_write = Commands.Link.start args in
            Stack.push t.write t.write_mem;
            t.write <- new_write;
            link
        | s when C.is_image s ->
            let error_msg m = t.error (Error.mk t.loc `error m) in
            let src, opts, lbl = Commands.Names.image_params error_msg args in
            t.write (o.start_image src opts lbl);
            `image (src, opts, lbl)
        | s when C.is_header s ->
            t.inside_header <- true; 
            t.write (o.start_header ());
            `header
        | s when C.is_title s -> t.write (o.start_title ()); `title
        | s when C.is_subtitle s -> t.write (o.start_subtitle ()); `subtitle
        | s when C.is_authors s -> t.write (o.start_authors ()); `authors
        | s when C.is_table s ->
            let table, to_stack, new_write = Commands.Table.start args in
            t.current_table <- Some table;
            Stack.push t.write t.write_mem;
            t.write <- new_write;
            to_stack
        | s when C.is_cell s ->
            let head, cnb, align = Commands.Table.cell_args args in
            let def_cell = `cell (head, cnb, align) in
            begin match t.current_table with
            | None ->
                t.error (Error.mk t.loc `error `cell_out_of_table);
                def_cell
            | Some tab -> Commands.Table.cell_start ~error:t.error tab args
            end;
        | s when C.is_note s -> t.write (o.start_note ()); `note
        | s ->
            t.error (Error.mk t.loc `error (`unknown_command  s));
            `unknown (s, args)
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
    );
)

let start_command t location name args = (
    t.loc <- location;
    (* p (~% "Command: \"%s\"(%s)\n" name (String.concat ", " args)); *)
    match Commands.non_env_cmd_of_name name args with
    | `unknown (name, args) -> start_environment t location name args
    | cmd -> CS.push t.stack cmd
)
let stop_command t location = (
    t.loc <- location;
    let o = t.output in
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
        | `paragraph -> t.write (o.paragraph ())
        | `new_line -> t.write (o.new_line ())
        | `non_break_space -> t.write (o.non_break_space ())
        | `horizontal_ellipsis -> t.write (o.horizontal_ellipsis ())
        | `open_brace -> t.write (o.open_brace ())
        | `close_brace -> t.write (o.close_brace ())
        | `sharp -> t.write (o.sharp ())
        | (`utf8_char i) -> t.write (o.utf8_char i)
        | (`quotation (op, clo)) -> t.write clo
        | `italic       ->  t.write (o.stop_italic ())
        | `bold         ->  t.write (o.stop_bold   ())
        | `mono_space   ->  t.write (o.stop_type   ())
        | `superscript  ->  t.write (o.stop_sup    ())
        | `subscript    ->  t.write (o.stop_sub    ())
        | `list (style, _, r) -> t.write (o.list_stop style)
        | `item ->
            begin match CS.head t.stack with
            | Some (`list (style, _, r))
            | Some (`cmd_inside (`list (style, _, r))) ->
                if !r then (
                    t.write (o.list_first_item style);
                    r := false;
                ) else (
                    t.write (o.list_item style);
                );
            | Some c ->
                t.error (Error.mk t.loc `error `item_out_of_list);
                CS.push t.stack c;
            | None ->
                t.error (Error.mk t.loc `error `item_out_of_list);
            end
        | `section (level, label) ->
            t.write (o.section_stop level label);
        | `link l ->
            t.write <- Stack.pop t.write_mem;
            let kind, target, text = Commands.Link.stop l in
            t.write (o.link kind target text);
        | `image (src, opts, lbl) -> t.write (o.stop_image src opts lbl);
        | `header -> t.inside_header <- false; t.write (o.stop_header ());
        | `title -> t.write (o.stop_title ());
        | `subtitle -> t.write (o.stop_subtitle ());
        | `authors -> t.write (o.stop_authors ());
        | `table _ ->
            begin match t.current_table with
            | None -> failwith "Why am I here ??? no table to end."
            | Some tab ->
                (* p (~% "End of table: %s\n" (Buffer.contents tab.caption)); *)
                t.write <- Stack.pop t.write_mem;
                t.current_table <- None;
                o.print_table t.write tab;
            end;
        | `cell _ ->
            begin match t.current_table with
            | None -> (* Already warned: *) ()
            | Some tab -> Commands.Table.cell_stop ~error:t.error tab
            end;
        | `note -> t.write (o.stop_note ())
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
        t.error (Error.mk t.loc `error `nothing_to_end_with_brace);
) 

let handle_comment_line t location line = (
    t.loc <- location;
    t.write (t.output.comment_line line);
)

let handle_text t location line = (
    t.loc <- location;
    if
        not t.started_text &&
        not t.inside_header &&
        not (Escape.is_white_space line)
    then (
        t.started_text <- true;
        t.write (t.output.start_text ());
    );
        
    if (* We should write <=> (text/noheader OR header fields) *)
        (t.started_text && (not t.inside_header)) ||
        (t.inside_header && (CS.head t.stack <> Some `header)) then (

        t.write (t.output.line line);
    ) else (
        if
            CS.head t.stack = Some `header
            && (not (Escape.is_white_space line))
        then (
            t.write (t.output.comment_line ("Ignored text: " ^ line));
        );

    )
)

let terminate t location = (
    t.loc <- location;
    if (CS.to_list t.stack) <> [] then (
        let l = List.map Commands.env_to_string (CS.to_list t.stack) in
        t.error (Error.mk t.loc `error (`terminating_with_open_environments l));
    );  
    t.write (t.output.terminate ());
) 

let enter_verbatim t location args = (
    CS.push t.stack (`code args);
    let postpro =
        match args with
        | q :: _ -> Some q | _ -> None
    in
    t.write (t.output.start_verbatim postpro);
)
let exit_verbatim t location = (
    let env =  (CS.pop t.stack) in
    match env with
    | Some (`code args) ->
        let postpro =
            match args with
            | q :: _ -> Some q | _ -> None
        in
        t.write (t.output.stop_verbatim postpro);
    | _ ->
        (* warning ? error ? anyway, *)
        failwith "Shouldn't be there, Parser's fault ?";
)

let handle_verbatim_line t location line = (
    t.write (t.output.verbatim_line line);
)

