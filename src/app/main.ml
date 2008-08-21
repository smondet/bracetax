let (~%) = Printf.sprintf
let p = print_string

module DummyPrinter = struct
    type t = int

    let create ~write = 0
    let print t f s = output_string f (~% "[%d]%s\n" t s)

    let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
    let head = "####"
    
    let handle_comment_line t location line =
        p (~% "%s%s[comment] \"%s\"\n" head (strstat location) line)
    let handle_text t location line =
        p (~% "%s%s[text] \"%s\"\n" head (strstat location) line)

    let start_command t location name args =
        p (~% "%s%s[start %s(%s)]\n" head (strstat location)
            name (String.concat ", " args))

    let stop_command t location = 
        p (~% "%s%s[stop]\n" head (strstat location))
        
    let terminate t location = 
        p (~% "%s%s[This is the end...]\n" head (strstat location))
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

