(******************************************************************************)
(*      Copyright (c) 2009, Sebastien MONDET                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

(** Implements {!type:GenericPrinter.output_t} for tables of contents. *)

(**/**)


let spr = Printf.sprintf

type inside = {
  mutable current: (int * string option) option;
  title_buffer: Buffer.t;
  mutable sections: (int * string option * string) list;
}

let store_opt me str () =
  if me.current <> None then (
    Buffer.add_string me.title_buffer str;
  );
  ""

let store_opt_nonunit me str =
  if me.current <> None then (
    Buffer.add_string me.title_buffer str;
  );
  ""

let start_section me level label =
  me.current <- Some (level, match label with "" -> None | s -> Some s);
  ""

let stop_section me level label =
  let lv, lb =
    match me.current with None -> failwith "Gni ??" | Some (a,b) -> (a,b) in
  me.sections <- (lv, lb, Buffer.contents me.title_buffer) :: me.sections;
  Buffer.reset me.title_buffer;
  me.current <- None;
  ""

let termination me () =
  let string_of_section label title =
    match label with
    | None -> title
    | Some link -> spr "{link local:%s|%s}" link title in
  let spaces_of_level level = String.make (level * 2) ' ' in
  let adjust_level_before curlv lv =
    let res = ref "" in
    for i = curlv to lv - 1 do
      res := !res ^ (spr "%s{list enum|\n" (spaces_of_level i));
    done;
    !res in
  let adjust_level_after curlv lv =
    if curlv > lv then
      (spr "%s%s\n" (spaces_of_level lv) (String.make (curlv - lv) '}'))
    else "" in
  let rec transform_list current_level acc = function
    | [] -> (String.make current_level '}') :: acc
    | (level, label, title) :: t ->
        let str =
          spr "%s%s%s{*} %s\n"
            (adjust_level_before current_level level)
            (adjust_level_after current_level level)
            (spaces_of_level level) (string_of_section label title)
        in
        transform_list level (str :: acc) t
  in
  (String.concat "" (List.rev (transform_list 0 [] (List.rev me.sections))))

(**/**)

(** Creation of the {!type:GenericPrinter.output_t}. *)
let create () =
  let me = 
    { current = None; title_buffer = Buffer.create 42; sections = [] } in
  {
    GenericPrinter.

    start_text = (fun _ -> "");
    terminate = termination me;
    start_raw = (fun _ _ -> "");
    raw_line = (fun _ _ -> "");
    stop_raw = (fun _ _ -> "");

    line = store_opt_nonunit me;
    comment_line = (fun _ -> "");

    quotation_open = (fun style ->
                        store_opt me (spr "{q %s|" style) ());
    quotation_close = (fun style ->
                         store_opt me "}" ());

    start_italic = store_opt me "{i|";
    start_bold = store_opt me "{b|";
    start_type = store_opt me "{t|";
    start_sup = store_opt me "{sup|";
    start_sub = store_opt me "{sub|";
    stop_italic = store_opt me "}";
    stop_bold = store_opt me "}";
    stop_type = store_opt me "}";
    stop_sup = store_opt me "}";
    stop_sub = store_opt me "}";

    list_start = (fun _ -> "");
    list_first_item = (fun _ -> "");
    list_item = (fun _ -> "");
    list_stop = (fun _ -> "");

    section_start = start_section me;
    section_stop  = stop_section me;

    paragraph  = store_opt me "{p}";
    new_line  = store_opt me "{br}";
    non_break_space  = store_opt me "{~}";
    horizontal_ellipsis  = store_opt me "{...}";
    en_dash = store_opt me "{--}";
    em_dash = store_opt me "{---}";

    open_brace  = store_opt me "{{}";
    close_brace  = store_opt me "{}}";
    sharp  = store_opt me "{#}";

    utf8_char = (fun i -> store_opt me (spr "{utf %d}" i) ());

    link = (fun _ _ _ -> "");

    start_header = (fun () -> "");
    start_title = (fun () -> "");
    start_authors = (fun () -> "");
    start_subtitle = (fun () -> "");
    stop_header = (fun () -> "");
    stop_title = (fun () -> "");
    stop_authors = (fun () -> "");
    stop_subtitle = (fun () -> "");

    start_image = (fun _ _ _ -> ""); 
    stop_image  = (fun _ _ _ -> ""); 

    print_table = (fun _ _ -> ());

    start_note = (fun () -> "");
    stop_note = (fun () -> "");

    start_quote = (fun () -> "");
    stop_quote = (fun () -> "");

  }

