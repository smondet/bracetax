(******************************************************************************)
(*      Copyright (c) 2008, Sebastien MONDET                                  *)
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

(** Utility functions for sanitization of strings. *)

(**/**)

let (~%) = Printf.sprintf
let p = print_string
let string_of_char = String.make 1

(**/**)

let replace_string ~src ~find ~replace_with =
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

(**
Replace characters by strings. For example:
{[
    let patterns = [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;")] in
    Escape.replace_chars ~src:line ~patterns ]}
*)
let replace_chars ~src ~patterns =
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

let is_white_space str =
  try 
    String.iter
      (fun c ->
         if c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r'  then
           raise Exit)
      str;
    true
  with 
    Exit -> false

(** Cleans a string to use only A-Z, a-z, 0-9, _ characters. *)
let clean_string s =
  let b = Bytes.of_string s in
  for i = 0 to String.length s - 1 do
    b.[i] <-
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> s.[i]
      | c -> '_';
  done;
  Bytes.unsafe_to_string b



