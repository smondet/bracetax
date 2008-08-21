
type command =
    | Italic
    | Bold
    | TypeWriter
    | Verbatim of string list


type stack = command list ref

let empty () = ref []

let push s cmd = s := cmd :: !s

let pop s = match !s with [] -> None | h :: t -> s:= t; Some h

let head s = match !s with [] -> None | h :: t -> Some h




