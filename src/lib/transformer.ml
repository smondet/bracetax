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

    let parse_line t line number = (
        let module S = String in
        (* let i = ref 0 in *)
        let l = S.length line in
        let rec loop i =
            if i < l then
                loop begin match String.get line i with
                | '#' ->
                        Printer.handle_comment_line t.t_printer
                            (make_state number i [])
                            (String.sub line i (l - i));
                        l
                | _ -> (i + 1)
                end
        in loop 0;
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
