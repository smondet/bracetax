module Sig = Signatures

let (~%) = Printf.sprintf

module FunctorMake =
functor (Printer: Sig.PRINTER) -> struct
    type t = {
        t_printer: Printer.t;
        t_read: unit -> string option;
        t_write: string -> unit;
    }
    let create ~read ~write = {
        t_printer = Printer.create ~write; t_read = read; t_write = write;
    }

    let make_loc l c = {Sig.s_line = l; s_char = c;}

    type meta_state =
        | Parsing
        | BeganVerbatim of string * string list

    type parser_state =
        | ReadText of int
        | ReadCommand of int * string option (* the current read name *)
        | ReadArgs of int * string * string list * string option
        (* ReadArgs (since, cmd_name, arg_list, current_arg *)

    let string_of_state =
        let sl l = (~% "L%d" (List.length l)) in
        let so = function None -> "_" | Some s -> s in
        function
        | ReadText o -> ~% "ReadText:%d" o
        | ReadCommand (i, o) -> ~% "ReadCommand:%d:%s" i (so o)
        | ReadArgs (i,c,l,Some o) -> ~% "ReadArgs:%d:%s:%s:%s" i (sl l) c o
        | ReadArgs (i,c,l, None) -> ~% "ReadArgs:%d:%s:%s:_" i (sl l) c


    module Str = String (* to be able to swicth easily *)

    (* Substring with indexes *)
    let sub_i s i j = Str.sub s i (j - i + 1)
    (* Substring from 'since' to the end *)
    let sub_end s since =
        let l = Str.length s in Str.sub s since (l - since)


    let debug s i state  = (
        if true then (
            let l = Str.length s in
            try
                Printf.eprintf "---[State: %s] \"%s[%s]%s\"    (%d)\n%!"
                     (string_of_state state)
                     (Str.sub s 0 i)
                     (Str.sub s i 1)
                     (Str.sub s (i + 1) (l - i - 1))
                     i
            with
            e -> ()
        );
    )

    let parse_line t line number state = (
        (* let i = ref 0 in *)
        let l = Str.length line in
        let opt_from_to ?(add_space=false) ?(opt=None) str i_from i_to =
            let substr = Str.sub str i_from (1 + i_to - i_from) in
            match opt with
            | None -> substr ^ (if add_space then " " else "")
            | Some s -> s ^ substr ^ (if add_space then " " else "")
        in
        let flush_text since last =
            Printer.handle_text t.t_printer
                (make_loc number last)
                (opt_from_to line since last)
        in
        let rec loop (i, state) =
            if i < l then (
                debug line i state;
                loop begin match Str.get line i with
                | '#' ->
                        let nstate =
                            match state with
                            | ReadText since ->
                                    (* TODO pop command *)
                                    flush_text since (i - 1);
                                    Printer.handle_comment_line t.t_printer
                                        (make_loc number i)
                                        (sub_end line (i+1));
                                    ReadText l
                            | s -> s
                        in
                        (l, nstate) (* i.e. go to 'EOL'  *)
                | '{' ->
                        let ni = i + 1 in
                        let nstate =
                            match state with
                            | ReadText since ->
                                    flush_text since (i - 1);
                                    ReadCommand (ni, None)
                            | ReadCommand (since, opt) ->
                                    ReadArgs (
                                        ni, opt_from_to ~opt line since (i-1),
                                        [], None)
                            | ReadArgs (_) as ra -> ra
                        in
                        (ni, nstate)
                | ':' ->
                        let ni = i + 1 in
                        let nstate =
                            match state with
                            | ReadCommand (since, opt) ->
                                    Printer.start_command t.t_printer
                                        (make_loc number i)
                                        (opt_from_to ~opt line since (i-1))
                                        [];
                                    ReadText ni
                            | s -> s
                        in
                        (ni, nstate)
                         
                | '}' ->
                        let ni = i + 1 in
                        let nstate =
                            match state with
                            | ReadText since ->
                                    flush_text since (i - 1);
                                    Printer.stop_command t.t_printer
                                        (make_loc number i);
                                    ReadText ni
                            | ReadCommand (since, opt) ->
                                    Printer.start_command t.t_printer
                                        (make_loc number i)
                                        (opt_from_to ~opt line since (i-1))
                                        [];
                                    Printer.stop_command t.t_printer
                                        (make_loc number i);
                                    ReadText ni
                            | ReadArgs (since, cmd, args, opt) ->
                                    let the_args =
                                        (opt_from_to ~opt line since (i-1))
                                            :: args
                                    in
                                    let another_arg =
                                        if ni <> l then
                                            Str.get line ni = '{' else false in
                                    if another_arg then (
                                        ReadArgs (
                                            ni + 1, cmd,
                                            the_args, None)
                                    ) else (
                                        Printer.start_command t.t_printer
                                            (make_loc number i) cmd the_args;
                                        ReadText ni
                                    )
                        in
                        (ni, nstate)
                        (* end command *)
                | _ ->
                        (* characters *)
                        (i + 1, state)
                end
            ) else (
                (* EOL *)
                let next_state =
                    let add_space = 
                        true (* '\n' is a white space => we put ' ' *) in
                    match state with
                    | ReadText since ->
                            if since <> i then (
                                flush_text since (i - 1);
                            ) else (
                                Printer.handle_text t.t_printer
                                    (make_loc number i)
                                    (" ")
                            );
                            ReadText 0
                    | ReadCommand (since, opt) ->
                            ReadCommand (0,
                                Some (opt_from_to
                                    ~add_space ~opt line since (l-1))) 
                    | ReadArgs (since, cmd_name, arg_list, opt) ->
                            ReadArgs (0, cmd_name, arg_list,
                                Some (opt_from_to
                                    ~add_space ~opt line since (l-1))) 
                in
                next_state
            )
        in loop (0, state)
    )

    let verb_pattern = "{verbatim"
    let verb_default_end = "{endverbatim}"

    let is_begin_verb line = (
        let l_pattern = Str.length verb_pattern in
        let l_line = (Str.length line) in
        if not (l_line >= (l_pattern + 1)) then (
            None
        ) else if not ((Str.sub line 0 l_pattern) = verb_pattern) then (
            None
        ) else (
            match Str.get line l_pattern with
            | '}' ->
                    (* start with defaults *)
                    (* warning if more data after *)
                    Some (verb_default_end, []) 
            | '{' ->
                    begin try
                        let next_cbra =
                            Str.index_from line (l_pattern + 1) '}' in
                        let end_token =
                            if next_cbra = l_pattern + 1 then
                                verb_default_end
                            else 
                                ~% "{%s}" (
                                    sub_i line (l_pattern + 1) (next_cbra - 1))
                        in
                        let args =
                            let rec parse_args cur_char acc = 
                                let another_arg =
                                    if cur_char <> l_line then
                                        Str.get line cur_char = '{'
                                    else
                                        false
                                in
                                if another_arg then (
                                    let next =
                                        Str.index_from line cur_char '}' in
                                    let arg = 
                                        sub_i line (cur_char + 1) (next - 1) in
                                    (* print_string (~% "Arg: %s\n" arg); *)
                                    parse_args (next + 1) (arg :: acc)
                                ) else
                                    acc
                            in
                            parse_args (next_cbra + 1) []
                        in
                        (* print_string (~% "End tok: %s\n" end_token); *)
                        Some (end_token, args)
                    with
                    Not_found -> None
                    end
            | _ ->
                    (* warning ? *)
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
                                begin match is_begin_verb s with
                                | None ->
                                        (parse_line t s lineno state, Parsing)
                                | Some (endtok, opts) ->
                                        (ReadText 0,
                                            BeganVerbatim (endtok, opts))
                                end
                        | BeganVerbatim (end_token, opts) ->
                                if (
                                    ((Str.length s) >=
                                        (Str.length end_token))
                                    && (* assumption on evaluation order... *)
                                    (Str.sub s 0 (Str.length end_token) =
                                        end_token)
                                ) then (
                                    (ReadText 0, Parsing)
                                ) else (
                                    Printer.handle_verbatim_line t.t_printer
                                        (make_loc lineno 0) s opts;
                                    (state, meta_state)
                                )
                    in
                    while_loop (lineno + 1) new_state new_metastate
            | None -> lineno,state
        in
        let last_line, last_state = while_loop 1 (ReadText 0) (Parsing) in
        (* call Printer.this_is_the_end *)
        Printer.terminate t.t_printer (make_loc last_line 0);
    )
end

module Make =
    (FunctorMake: functor (Printer: Sig.PRINTER) -> (Sig.TRANSFORMER))
    (* with type t = FunctorMake.t)) *)
