
type command = [
    | `italic
    | `bold
    | `typeWriter
    | `verbatim of string list
]

type t = command list ref

let empty () = ref []

let push s cmd = s := cmd :: !s

let pop s = match !s with [] -> None | h :: t -> s:= t; Some h

let head s = match !s with [] -> None | h :: t -> Some h




