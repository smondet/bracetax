
let (~%) = Printf.sprintf
let p = print_string
let string_of_char = String.make 1

let replace_string ~src ~find ~replace_with = (
    let l_src = String.length src in
    let l_find = String.length find in
    let res_buf = Buffer.create (2 * l_src) in
    let i = ref  0  in
    while !i < l_src  do
        if (!i <= l_src - l_find) && (String.sub src !i l_find = find) then  (
            Buffer.add_string res_buf replace_with;
            i := !i + l_find;
        ) else (
            Buffer.add_char res_buf src.[!i];
            incr i;
        )
    done;
    (* Buffer.add_substring res_buf src (l_src - l_find + 1) (l_find - 1); *)
    Buffer.contents res_buf
)

let replace_chars ~src ~patterns = (
    let l_src = String.length src in
    let res_buf = Buffer.create (2 * l_src) in
    let rec parse_list cur = function
        | [] -> string_of_char cur
        | (c, replace) :: t ->
            if c = cur then replace else parse_list cur t
    in
    for i = 0 to l_src - 1 do
        Buffer.add_string res_buf (parse_list src.[i] patterns);
    done;
    Buffer.contents res_buf
)
(* 

# let src = "azerty<uiop>qsdf&ghj<>&;jklm";;
val src : string = "azerty<uiop>qsdf&ghj<>&;jklm"
# let patterns = [ ('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;")];;
val patterns : (char * string) list =
      [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;")]
      # replace_chars ~src ~patterns;;
- : string = "azerty&lt;uiop&gt;qsdf&amp;ghj&lt;&gt;&amp;;jklm"
#

 *)

let is_white_space str = (
    try 
        String.iter (fun c ->
            if c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r'  then
                raise Exit
        ) str;
        true
    with 
    Exit -> false
)



