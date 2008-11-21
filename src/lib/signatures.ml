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


type location = {
    s_line: int;
    s_char: int;
}
type writer = {
    w_write: string -> unit;
    w_error: Error.error -> unit;
}
let make_writer ~write  ~error = (
    {
        w_write = write;
        w_error = error;
    }
)

module type PRINTER = sig
    type t
    val create: writer:writer -> t
    val handle_comment_line: t -> location -> string -> unit
    val handle_text: t -> location -> string -> unit
    val start_command: t -> location -> string -> string list -> unit
    val stop_command: t -> location -> unit
    val terminate: t -> location -> unit

    val enter_verbatim: t -> location -> string list -> unit
    val handle_verbatim_line: t -> location -> string -> unit
    val exit_verbatim: t -> location -> unit
end


module type TRANSFORMER = sig
    type t
    val create: read:(unit -> string option) -> writer:writer -> t
    val do_transformation: t -> unit
end



