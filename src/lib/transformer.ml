module Sig = Signatures

let (~%) = Printf.sprintf

module FunctorMake =
functor (Printer: Sig.PRINTER) -> struct
    type t = {
        t_printer: Printer.t;
        t_read: unit -> string option;
        t_write: string -> unit;
        t_warn: string -> unit;
    }
    let create ~read ~writer = {
        t_printer = Printer.create ~writer;
        t_read = read;
        t_write = writer.Sig.w_write;
        t_warn = writer.Sig.w_warn;
    }

    let make_loc l c = {Sig.s_line = l; s_char = c;}

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
                t.t_warn (~%
                    "---[State: %s] \"%s[%s]%s\"    (%d)\n%!"
                    (string_of_state state)
                    (S.sub s 0 i)
                    (S.sub s i 1)
                    (S.sub s (i + 1) (l - i - 1))
                    i)
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
                (make_loc number last)
                (opt_from_to ?add_space line since last)
        in
        let handle_read_text i since = function
            | '#' ->
                flush_text since (i - 1);
                Printer.handle_comment_line t.t_printer
                    (make_loc number i) (sub_end line (i+1));
                (l, ReadText l)
            | '{' ->
                flush_text since (i - 1);
                (i + 1, ReadCommand (i + 1, None))
            | '}' ->
                flush_text since (i - 1);
                Printer.stop_command t.t_printer (make_loc number i);
                (i + 1, ReadText (i + 1))
            | '\n' ->
                if since <> i then (
                    flush_text since (i - 1);
                ) else (
                    Printer.handle_text t.t_printer (make_loc number i) " ";
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
                    split_and_start t (make_loc number i)
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
                            let loc = (make_loc number i) in
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

    let is_begin_verb line = (
        let l_pattern = S.length verb_pattern in
        let l_line = (S.length line) in
        if not (l_line >= (l_pattern + 1)) then (
            None
        ) else if not ((S.sub line 0 l_pattern) = verb_pattern) then (
            None
        ) else (
            match S.get line l_pattern with
            | '}' ->
                (* start with defaults *)
                (* warning if more data after *)
                Some (verb_default_end, []) 
            | ' ' | '\t' ->
                begin try
                    let next_cbra = S.index_from line (l_pattern + 1) '}' in
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
                with Not_found -> None
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
                            Printer.enter_verbatim
                                t.t_printer (make_loc lineno 0) opts;
                            (ReadText 0, BeganVerbatim (endtok, opts))
                        end
                    | BeganVerbatim (end_token, opts) ->
                        if (
                            ((S.length s) >= (S.length end_token))
                            && (* assumption on evaluation order... *)
                            (S.sub s 0 (S.length end_token) = end_token)
                        ) then (
                            Printer.exit_verbatim
                                t.t_printer (make_loc lineno 0);
                            (ReadText 0, Parsing)
                        ) else (
                            Printer.handle_verbatim_line t.t_printer
                                (make_loc lineno 0) s;
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
