
type plugout = {
    tag: string;
    match_line_begin: string;
    match_line_end: string;
    match_verb_begin: string;
    match_verb_end: string;
    begin_handler: unit -> string;
    line_handler: string -> string;
    end_handler: unit -> string;
}

let line_matches_begin tag line = (
    let latex = "%%verbatimbegin:" ^ tag in
    let html = "<!--verbatimbegin:" ^ tag ^ " -->" in
    (line = latex) || (line = html)
)
let line_matches_end tag line = (
    let latex = "%%verbatimend:" ^ tag in
    let html = "<!--verbatimend:" ^ tag ^ " -->" in
    (line = latex) || (line = html)
)
let line_is_verbatim_begin line = (
    let latex = "\\begin{verbatim}" in
    let html = "<pre>" in
    (line = latex) || (line = html)
)
let line_is_verbatim_end line = (
    let latex = "\\end{verbatim}" in
    let html = "</pre>" in
    (line = latex) || (line = html)
)

let begins tags line1 line2 = (
    None
)
let ends tag line1 line2 = (
    false
)

type currrent_state = [ `writing | `read_begin | `read_verbend ]

let process plugouts readline writeline = (

    let rec look_for_begin line1 line2 = 
        match begins plugouts line1 line2 with
        | Some po ->
            writeline (po.begin_handler ());
            let line3, line4 = 
                "", "" 
            in
            look_for_end po line3 line4
        | None ->
            writeline line1;
            begin match readline () with
            | Some s -> 
                look_for_begin line2 s;
            | None -> 
                writeline line2;
            end;
        and
    look_for_end po line1 line2 = 
        match ends po line1 line2 with
        | true ->
            writeline (po.end_handler ());
            let line3, line4 = 
                "", "" 
            in
            look_for_begin line3 line4
        | false ->
            writeline (po.line_handler line1);
            begin match readline () with
            | Some s -> 
                look_for_end po line2 s;
            | None -> 
                writeline (po.line_handler line2);
            end;
    in
    look_for_begin "" ""

)

