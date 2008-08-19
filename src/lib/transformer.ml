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
        | CommandFrom of int


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
                        (i + 1, CommandFrom i)
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
