let (~%) = Printf.sprintf
let p = print_string

module DummyPrinter = struct
    type t = int

    let create ~write = 0

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

    let handle_verbatim_line t location line =
        p (~% "%s%s[verbatim] %s\n" head (strstat location) line)

    let enter_verbatim t location args = ()
    let exit_verbatim t location = ()
end

let () = (
    if Sys.argv.(1) = "-debug" then (
        let module DummyTransformer = Transformer.Make(DummyPrinter) in
        let o = open_in Sys.argv.(2) in
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
    ) else (
        let module HtmlTransformer = Transformer.Make(HtmlPrinter) in
        let o = open_in Sys.argv.(1) in
        let t =
            HtmlTransformer.create
                ~write:(print_string)
                ~read:(fun () -> try Some (input_line o) with e -> None)
        in
        HtmlTransformer.do_transformation t;
        close_in o;
    )
)

