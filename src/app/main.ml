let (~%) = Printf.sprintf
let p = print_string

module DummyPrinter = struct
    type t = int

    let create ~write = 0
    let print t f s = output_string f (~% "[%d]%s\n" t s)

    let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
    let head = "####"
    
    let handle_comment_line t state line =
        p (~% "%s%s[comment] \"%s\"\n" head (strstat state) line)
    let handle_text t state line =
        p (~% "%s%s[text] \"%s\"\n" head (strstat state) line)

end

let () = (
    let module DummyTransformer = Transformer.Make(DummyPrinter) in
    let o = open_in Sys.argv.(1) in
    let t =
        DummyTransformer.create
            ~write:(fun s -> ())
            ~read:(fun () -> 
                try Some (input_line o) 
                with e -> None
            ) in
    DummyTransformer.do_transformation t;
    close_in o;
    p (~% "Done;\n")
)

