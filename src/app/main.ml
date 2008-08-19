let (~%) = Printf.sprintf
let p = print_string

module DummyPrinter = struct
    type t = int

    let create ~write = 0
    let print t f s = output_string f (~% "[%d]%s\n" t s)

    let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
    
    let handle_comment_line t state line =
        p (~% "%s{comment} %s\n" (strstat state) line);

end

let () = (
    let module DummyTransformer = Transformer.Make(DummyPrinter) in
    let t =
        DummyTransformer.create
            ~write:(fun s -> ())
            ~read:(fun () -> 
                try Some (read_line ()) 
                with e -> None
            ) in
    DummyTransformer.do_transformation t;
    p (~% "Done;\n")
)

