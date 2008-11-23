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

type write_fun = string -> unit

type output_t = {
    o_comment_line: write_fun;
}

type t = {
    stack: Commands.Stack.t;
    mutable write: write_fun;
    write_mem: write_fun Stack.t;
    mutable started_text: bool;
    mutable inside_header:bool;
    mutable current_table: Commands.Table.table option;
    error: Error.error -> unit;
    mutable loc: Error.location;
    mutable output: output_t;
}
module CS = Commands.Stack

let spr = Printf.sprintf

let create ~writer ~output =  (
    let module S = Signatures in
    let write = writer.S.w_write in
    {
        stack = CS.empty ();
        write = write;
        write_mem = Stack.create ();
        started_text = false;
        inside_header = false;
        current_table = None;
        error = writer.S.w_error;
        loc = {Error.l_line = -1; Error.l_char = -1;};
        output = output;
    }
)

