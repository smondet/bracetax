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
module Sig = Signatures

let (~%) = Printf.sprintf

module Make =
functor (Printer: Sig.PRINTER) -> struct
    type t = {
        t_printer: Printer.t;
        t_read: unit -> string option;
        t_write: string -> unit;
        t_error: Error.error_fun;
        t_filename: string;
    }
    type aux = Printer.aux

    let create ~read ~writer ?(filename="NO FILE NAME") aux = {
        t_printer = Printer.create ~writer aux;
        t_read = read;
        t_write = writer.Sig.w_write;
        t_error = writer.Sig.w_error;
        t_filename = filename;
    }

    let make_loc l c f = {Error.l_line = l; l_char = c;l_file = f;}

    type meta_state =
        | Parsing
        | BeganVerbatim of string * string list

    type parser_state =
        | ReadText of int
        | ReadCommand of int * string option (* the current read command *)

    let string_of_state =
        let so = function None -> "_" | Some s -> s in
        function
        | ReadText o -> ~% "ReadText:%d" o
        | ReadCommand (i, o) -> ~% "ReadCommand:%d:%s" i (so o)


    module S = String (* to be able to swicth easily *)

    (* Substring with indexes *)
    let sub_i s i j = S.sub s i (j - i + 1)
    (* Substring from 'since' to the end *)
    let sub_end s since = let l = S.length s in S.sub s since (l - since)

    let split_str str = (
        let l = S.length str in
        let escaping = ref false in
        let escaping_next = ref false in
        let res = ref [] in
        let buf = Buffer.create 64 in
        let new_split () =
            let s = Buffer.contents buf in
            if s <> "" then (
                res := s :: !res;
                Buffer.reset buf;
            );
        in
        for i = 0 to l - 1 do
            escaping := !escaping_next;
            escaping_next := false;
            begin match S.get str i with
            | '\\' ->
                if not !escaping then
                    escaping_next := true
                else
                    Buffer.add_char buf '\\'
            | ' ' ->
                if !escaping then (
                    Buffer.add_char buf ' '
                ) else (
                    new_split ();
                );
            | c -> Buffer.add_char buf c
            end;
        done;
        new_split ();
        List.rev !res
    )

    let debug t s i state  = (
        if false then (
            let l = S.length s in
            try
                t.t_error (`undefined
                    (~%
                        "---[State: %s] \"%s[%s]%s\"    (%d)\n%!"
                        (string_of_state state)
                        (S.sub s 0 i)
                        (S.sub s i 1)
                        (S.sub s (i + 1) (l - i - 1))
                        i))
            with
            e -> ()
        );
    )

    let parse_line t line number state = (
        (* let i = ref 0 in *)
        let l = S.length line in
        let opt_from_to ?(add_space=false) ?(opt=None) str i_from i_to =
            let substr = S.sub str i_from (1 + i_to - i_from) in
            match opt with
            | None -> substr ^ (if add_space then " " else "")
            | Some s -> s ^ substr ^ (if add_space then " " else "")
        in
        let flush_text ?add_space since last =
            Printer.handle_text t.t_printer
                (make_loc number last t.t_filename)
                (opt_from_to ?add_space line since last)
        in
        let handle_read_text i since = function
            | '#' ->
                flush_text since (i - 1);
                Printer.handle_comment_line t.t_printer
                    (make_loc number i t.t_filename) (sub_end line (i+1));
                (l, ReadText l)
            | '{' ->
                flush_text since (i - 1);
                (i + 1, ReadCommand (i + 1, None))
            | '}' ->
                flush_text since (i - 1);
                Printer.stop_command t.t_printer
                    (make_loc number i t.t_filename);
                (i + 1, ReadText (i + 1))
            | '\n' ->
                if since <> i then (
                    flush_text ~add_space:true since (i - 1);
                ) else (
                    Printer.handle_text t.t_printer
                        (make_loc number i t.t_filename) " ";
                );
                (4242, ReadText 0)
            | _ -> (* characters *) (i + 1, ReadText since)
        in
        let escaping = ref false in
        let escaping_next = ref false in
        let split_and_start t loc str =
            let l = split_str str in
            Printer.start_command t.t_printer loc (List.hd l) (List.tl l);
        in
        let handle_read_command i since opt = function
            | '\\' ->
                if not !escaping then escaping_next := true;
                (i + 1, ReadCommand (since, opt))
            | '|' ->
                if not !escaping then (
                    split_and_start t (make_loc number i t.t_filename)
                        (opt_from_to ~opt line since (i-1));
                    (i + 1, ReadText (i + 1))
                ) else (
                    (i + 1, ReadCommand (since, opt))
                )
            | '}' ->
                let ni = i + 1 in
                let nstate =
                    if since = i then (* it's a '}' command *)
                        ReadCommand (since, opt)
                    else (
                        if not !escaping then (
                            let loc = (make_loc number i t.t_filename) in
                            split_and_start t loc
                                (opt_from_to ~opt line since (i-1));
                                Printer.stop_command t.t_printer loc;
                            ReadText ni
                        ) else (
                            ReadCommand (since, opt)
                        )
                    )
                in
                (ni, nstate)
            | '\n' ->
                (4242, ReadCommand (0,
                    Some (opt_from_to ~add_space:true ~opt line since (l-1))))
            | _ -> (* characters *) (i + 1, ReadCommand (since, opt))
        in
        let rec loop (i, state) =
            if i < l then (
                debug t line i state;
                escaping := !escaping_next;
                escaping_next := false;
                let the_char = S.get line i in
                let next_state =
                    match state with
                    | ReadText since ->
                        handle_read_text i since the_char
                    | ReadCommand (since, opts) ->
                        handle_read_command i since opts the_char
                in
                loop next_state
            ) else (
                (* EOL *)
                let _, next_state =
                    match state with
                    | ReadText since ->
                        handle_read_text i since '\n'
                    | ReadCommand (since, opts) ->
                        handle_read_command i since opts '\n'
                in
                next_state
            )
        in loop (0, state)
    )

    let verb_pattern = "{verbatim"
    let verb_default_end = "{endverbatim}"

    let is_begin_verb loc t line = (
        let l_pattern = S.length verb_pattern in
        let l_line = (S.length line) in
        if not (l_line >= (l_pattern + 1)) then (
            None
        ) else if not ((S.sub line 0 l_pattern) = verb_pattern) then (
            None
        ) else (
            match S.get line l_pattern with
            | '}' ->
                if l_pattern + 1 < l_line then (
                    (* warning if more data after *)
                    t.t_error (Error.mk loc `warning
                        (`ignored_text_after_verbatim_begin 
                            (~% "%S" (S.sub line (l_pattern + 1)
                                (l_line - l_pattern - 1)))));
                );
                (* start with defaults *)
                Some (verb_default_end, []) 
            | ' ' | '\t' ->
                begin try
                    let next_cbra = S.index_from line (l_pattern + 1) '}' in
                    if next_cbra + 1 < l_line then (
                        (* warning if more data after *)
                        t.t_error (Error.mk loc `warning
                            (`ignored_text_after_verbatim_begin 
                             (~% "%S" (S.sub line (next_cbra + 1)
                                       (l_line - next_cbra - 1)))));
                    );
                    let args_string =
                        sub_i line (l_pattern + 1) (next_cbra - 1) in
                    let args = split_str args_string in
                    let end_token, actual_args =
                        match args with
                        | [] -> (verb_default_end, [])
                        | "_" :: t -> (verb_default_end, t)
                        | h :: t -> (~% "{%s}" h,t)
                    in
                    Some (end_token, actual_args)
                with Not_found ->
                    (* no '}' *)
                    t.t_error (Error.mk loc `error `malformed_verbatim_begin);
                    None
                end
            | _ ->
                    t.t_error (Error.mk loc `error `malformed_verbatim_begin);
                    None
        )

    )

    let do_transformation t = (
        let rec while_loop lineno state meta_state = 
            match t.t_read () with
            | Some s ->
                let new_state, new_metastate =
                    match meta_state with
                    | Parsing ->
                        let beg_verb = 
                            is_begin_verb (make_loc lineno 0 t.t_filename) t s
                        in
                        begin match beg_verb with
                        | None ->
                            (parse_line t s lineno state, Parsing)
                        | Some (endtok, opts) ->
                            Printer.enter_verbatim
                                t.t_printer (make_loc lineno 0 t.t_filename)
                                opts;
                            (ReadText 0, BeganVerbatim (endtok, opts))
                        end
                    | BeganVerbatim (end_token, opts) ->
                        if (
                            ((S.length s) >= (S.length end_token))
                            && (* assumption on evaluation order... *)
                            (S.sub s 0 (S.length end_token) = end_token)
                        ) then (
                            Printer.exit_verbatim
                                t.t_printer (make_loc lineno 0 t.t_filename);
                            (ReadText 0, Parsing)
                        ) else (
                            Printer.handle_verbatim_line t.t_printer
                                (make_loc lineno 0 t.t_filename) s;
                            (state, meta_state)
                        )
                in
                while_loop (lineno + 1) new_state new_metastate
            | None -> lineno,state
        in
        let last_line, last_state = while_loop 1 (ReadText 0) (Parsing) in
        (* call Printer.this_is_the_end *)
        Printer.terminate t.t_printer (make_loc last_line 0 t.t_filename);
    )
end

type raw_t = [ `escape| `code ]
type printer = {
    print_comment: Error.location -> string -> unit;
    print_text:    Error.location -> string -> unit;
    enter_cmd:     Error.location -> string -> string list -> unit;
    leave_cmd:     Error.location -> unit;
    terminate:     Error.location -> unit;

    enter_raw:     Error.location -> raw_t -> string list -> unit;
    print_raw:     Error.location -> string -> unit;
    leave_raw:     Error.location -> unit;
    error: Error.error -> unit;
}

let err pr loc typ = pr.error (Error.mk loc `error typ)
let loc line file = { Error.l_line = line; l_char = -1; l_file = file }
let incr_loc location = loc (location.Error.l_line + 1) location.Error.l_file

let rec parse_text printer read_fun location = (
    let buf = Buffer.create 42 in
    let rec read_loop location = 
        match read_fun () with
        | None ->
            printer.print_text location (Buffer.contents buf);
            printer.terminate location;
        | Some '\n' ->
            Buffer.add_char buf ' ';
            read_loop (incr_loc location)
        | Some '#' ->
            printer.print_text location (Buffer.contents buf);
            parse_comment printer read_fun location
        | Some '{' ->
            printer.print_text location (Buffer.contents buf);
            parse_command printer read_fun location
        | Some '}' ->
            printer.print_text location (Buffer.contents buf);
            Buffer.reset buf;
            printer.leave_cmd location;
            read_loop location
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location
    in
    read_loop location
)
and parse_comment printer read_fun location = (
    let buf = Buffer.create 42 in
    let rec read_loop location = 
        match read_fun () with
        | None ->
            printer.print_comment location (Buffer.contents buf);
            printer.terminate location;
        | Some '\n' ->
            printer.print_comment location (Buffer.contents buf);
            parse_text printer read_fun (incr_loc location)
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location
    in
    read_loop location
)
and parse_command printer read_fun location = (
    let buf = Buffer.create 42 in
    let cmd = ref [] in
    let rec read_loop location escaping = 
        match read_fun () with
        | None ->
            err printer location (`end_of_input_not_in_text "Reading Command");
            printer.terminate location;
        | Some '\\' ->
            if escaping then (
                Buffer.add_char buf '\\';
                read_loop location false
            ) else
                read_loop location true
        | Some c when c = ' ' || c = '\t' || c = '\n' ->
            let loc = if c = '\n' then (incr_loc location) else location in
            if escaping then (
                Buffer.add_char buf c;
                read_loop loc false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                Buffer.reset buf;
                read_loop loc false
            )
        | Some '}' ->
            if escaping then (
                Buffer.add_char buf '}';
                read_loop location false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                match List.rev !cmd with
                | [] ->
                    Buffer.add_char buf '}';
                    read_loop location false
                | c :: t when c = "{" || c = "}" || c = "#" ->
                    printer.print_text location c;
                    (* TODO add warning if (t <> []) *)
                    parse_text printer read_fun location
                | q :: t ->
                    printer.enter_cmd location q t;
                    printer.leave_cmd location;
                    parse_text printer read_fun location
            )
        | Some '|' ->
            if escaping then (
                Buffer.add_char buf '|';
                read_loop location false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                match List.rev !cmd with
                | [] ->
                    err printer location (`unknown_command "EMPTY CMD!!!");
                    parse_text printer read_fun location
                | q :: t ->
                    printer.enter_cmd location q t;
                    parse_text printer read_fun location
            )
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location false
    in
    read_loop location false
)


let do_transformation printer read_fun filename = (
    parse_text printer read_fun (loc 1 filename)
)
