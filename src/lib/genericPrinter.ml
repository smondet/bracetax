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


(** A generic printer, it implements {!type:Signatures.printer}
    functions from {!type:GenericPrinter.output_t}s. *)

(** The so-called "outputs" are structures of type
    {!type:GenericPrinter.output_t}. *)



type write_fun = string -> unit
type string_of_args_fun = string list -> string
type string_fun = unit -> string
type transform_string_fun = string -> string


(** The type to "implement" to use the generic printer. *)
type output_t = {


  start_text: string_fun; (** Function called when printing starts. *)
  terminate: string_fun; (** Function called before stopping the printing. *)

  start_raw: Signatures.raw_t -> string option -> string;
  (** Begin a non-parsed bloc, the [string option] is the optional
      post-processing plugin "identifier". *)

  raw_line: Signatures.raw_t -> transform_string_fun;
  stop_raw: Signatures.raw_t -> string option -> string;

  line: transform_string_fun; (** Handle a line of "pure" text. *)
  comment_line: transform_string_fun;

  quotation_open:  string -> string; 
  (** Open "quotation marks" of the kind given as argument. *)
  
  quotation_close: string -> string;
  
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
  (** Start a section title given its level and its label. *)

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
  (** [(o.link kind target text)] to create a link. *)

  start_header: string_fun;
  start_title: string_fun;
  start_authors: string_fun;
  start_subtitle: string_fun;
  stop_header: string_fun;
  stop_title: string_fun;
  stop_authors: string_fun;
  stop_subtitle: string_fun;

  start_image: string -> Commands.Stack.image_size -> string -> string;
  (** Start an "image" (i.e. just before the caption), given the URL,
      the size, and the optional label. *)

  stop_image : string -> Commands.Stack.image_size -> string -> string; 

  print_table: (string -> unit) -> Commands.Table.table -> unit;
  (** Print the table using the first argument (print function). *)

  start_note: string_fun;
  stop_note: string_fun;

}

(**/**)

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

module CS = Commands.Stack

let spr = Printf.sprintf

let create ~writer output =
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


let start_environment ?(is_begin=false) t location name args =
  t.loc <- location;
  let o = t.output in
  let wr = t.write in
  let module C = Commands.Names in
  let cmd name args =
    match name with
    | s when C.is_quotation s ->
        let style =
          match args with [] -> "" | h :: q -> h in
        let op = o.quotation_open style in
        t.write op;
        `quotation (op, style)
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
        begin match t.current_table with
        | None ->
            t.error (Error.mk t.loc `error `cell_out_of_table);
            `cell (false, 1, `center)
        | Some tab -> Commands.Table.cell_start ~loc:t.loc ~error:t.error tab args
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
  ()

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
    | (`quotation (op, style)) -> t.write (o.quotation_close style)
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
        | Some tab -> Commands.Table.cell_stop ~loc:t.loc ~error:t.error tab
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

let start_raw_mode t location kind args =
  t.loc <- location;
  begin match kind with
  | `code ->
      CS.push t.stack (`code args);
      begin match args with
      | q :: _ -> t.write (t.output.start_raw kind (Some q))
      | _ -> t.write (t.output.start_raw kind None);
      end;
  | `bypass | `text | `ignore as env_kind->
      CS.push t.stack (env_kind :> Commands.Stack.environment);
      t.write (t.output.start_raw kind None);
  end

let handle_raw_text t location text =
  t.loc <- location;
  begin match CS.head t.stack with
  | Some (`code args) ->
      t.write (t.output.raw_line `code text);
  | Some `bypass -> t.write (t.output.raw_line `bypass text)
  | Some `text -> t.write (t.output.raw_line `text text)
  | Some `ignore -> t.write (t.output.raw_line `ignore text)
  | _ ->
      failwith "GenericPrinter.handle_raw_text in wrong state";
  end
        
let stop_raw_mode t location = 
  t.loc <- location;
  begin match CS.pop t.stack with
  | Some (`code args) ->
      begin match args with
      | q :: _ -> t.write (t.output.stop_raw `code (Some q))
      | _ -> t.write (t.output.stop_raw `code None)
      end;
  | Some `bypass -> t.write (t.output.stop_raw `bypass None)
  | Some `text   -> t.write (t.output.stop_raw `text None)
  | Some `ignore -> t.write (t.output.stop_raw `ignore None)
  | _ ->
      failwith "GenericPrinter.stop_raw_mode in wrong state";
  end


(**/**)

(** Build a [printer] from a {!type:GenericPrinter.output_t} to feed
{!val:Parser.do_transformation}, the optional argument has the same
meaning than for {!val:Transform.brtx_to_latex}.  *)
let build ?(print_comments=false) ~writer ~output_funs () =
  let t = create ~writer output_funs in
  {
    Signatures.
    print_comment =
      if print_comments then 
        (handle_comment_line t)
      else 
        (fun a b -> ());
    print_text =    handle_text t;
    enter_cmd =     start_command t;
    leave_cmd =     stop_command t;
    terminate =     terminate t;
    enter_raw =     start_raw_mode t;
    print_raw =     handle_raw_text t;
    leave_raw =     stop_raw_mode t;
    error = writer.Signatures.w_error; } 


