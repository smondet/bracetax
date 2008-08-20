module Sig = Signatures

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
        | Undef (* should not be used at the end *)
        | Terminated
        | TextFrom of int

    let get_arguments s index = (
        let rec get_args prev i acc =
            match String.get s i with
            | '}' -> 
                    let l = ((prev, (i-1)) :: acc) in
                    if try String.get s (i+1) = '{' with e -> false then (
                        (* continue for another arg *)
                        get_args (i+2) (i+2) l
                    ) else (
                        (* finish *)
                        l
                    )
            | _ -> 
                    get_args prev (i+1) acc
        in
        if String.get s index = '{' then (
            (* there are arguments *)
            get_args (index + 1) (index + 1) []
        ) else (
            []
        )
    )

    let get_command_name s index = (
        let rec parse i =
            match String.get s i with
            | ':' | '{' -> i
            | ' ' | '\n' -> failwith "White space in command name"
            | _ -> parse (i+1)
        in
        parse index
    )

    let parse_line t line number = (
        let module S = String in
        (* let i = ref 0 in *)
        let l = S.length line in
        let rec loop (i, state) =
            if i < l then
                loop begin match S.get line i with
                | '#' ->
                        Printer.handle_comment_line t.t_printer
                            (make_state number i [])
                            (S.sub line i (l - i));
                        (l, Terminated)
                | '{' ->
                        (* begin read command *)
                        (* TODO read command and change state *)
                        let after_name = get_command_name line i in
                        let args = get_arguments line after_name in
                        let after_command =
                            match args with
                            | [] -> after_name
                            | (b,e) :: t -> e + 1
                        in
                        (after_command, TextFrom after_name)
                | '}' ->
                        (* end command *)
                        (* TODO must flush text, pop command *)
                        (i + 1, TextFrom (i + 1))
                | ' ' | '\n' | '\r' | '\t' ->
                        (* white space *)
                        (i + 1, Undef)
                | _ ->
                        (* characters *)
                        (i + 1, Undef)
                end
        in loop (0, TextFrom 0);
    )

    let do_transformation t = (
        let rec while_loop lineno = 
            match t.t_read () with
            | Some s ->
                    parse_line t s lineno;
                    while_loop (lineno + 1)
            | None -> ()
        in
        while_loop 1;
    )
end

module Make =
    (FunctorMake: functor (Printer: Sig.PRINTER) -> (Sig.TRANSFORMER))
    (* with type t = FunctorMake.t)) *)
