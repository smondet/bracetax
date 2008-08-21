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

    let make_state l c s =
        {Sig.s_line = l; s_char = c; s_stack = s;}

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
        let sub s since = Str.sub s since (l - since) in
        let opt_from_to ?(add_space=false) opt str i_from i_to =
            let substr = Str.sub str i_from (1 + i_to - i_from) in
            match opt with
            | None -> substr ^ (if add_space then " " else "")
            | Some s -> s ^ substr ^ (if add_space then " " else "")
        in
        let rec loop (i, state) =
            if i < l then (
                debug line i state;
                loop begin match Str.get line i with
                | '#' ->
                        Printer.handle_comment_line t.t_printer
                            (make_state number i []) (sub line i);
                        (l, state) (* i.e. go to 'EOL'  *)
                | '{' ->
                        let ni = i + 1 in
                        let nstate =
                            match state with
                            | ReadText i ->
                                    (* TODO flush text *)
                                    ReadCommand (ni, None)
                            | ReadCommand (since, opt) ->
                                    ReadArgs (
                                        ni, opt_from_to opt line since (i-1),
                                        [], None)
                            | ReadArgs (_) as ra -> ra
                        in
                        (ni, nstate)
                | ':' ->
                        let ni = i + 1 in
                        let nstate =
                            match state with
                            | ReadCommand (since, opt) ->
                                    (* TODO call handler with *)
                                    (* opt_from_to opt line since (i-1) *)
                                    ReadText ni
                            | s -> s
                        in
                        (ni, nstate)
                         
                | '}' ->
                        let ni = i + 1 in
                        let nstate =
                            match state with
                            | ReadText i ->
                                    (* TODO flush text, pop command *)
                                    ReadText ni
                            | ReadCommand (since, opt) ->
                                    (* TODO push and pop command *)
                                    ReadText ni
                            | ReadArgs (since, cmd, args, opt) ->
                                    let another_arg =
                                        ni <> l && Str.get line ni = '{' in
                                    if another_arg then (
                                        ReadArgs (
                                            ni + 1, cmd,
                                            (opt_from_to opt line since (i-1))
                                                :: args, None)
                                    ) else (
                                        (* TODO push command *)
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
                    | ReadText i -> (* TODO flush text *) ReadText 0
                    | ReadCommand (since, opt) ->
                            ReadCommand (0,
                                Some (opt_from_to
                                    ~add_space opt line since (l-1))) 
                    | ReadArgs (since, cmd_name, arg_list, opt) ->
                            ReadArgs (0, cmd_name, arg_list,
                                Some (opt_from_to
                                    ~add_space opt line since (l-1))) 
                in
                next_state
            )
        in loop (0, state)
    )

    let do_transformation t = (
        let rec while_loop lineno state = 
            match t.t_read () with
            | Some s ->
                    let new_state =
                        parse_line t s lineno state in
                    while_loop (lineno + 1) new_state
            | None -> ()
        in
        while_loop 1 (ReadText 0);
        (* call Printer.this_is_the_end *)
    )
end

module Make =
    (FunctorMake: functor (Printer: Sig.PRINTER) -> (Sig.TRANSFORMER))
    (* with type t = FunctorMake.t)) *)
