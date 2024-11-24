type t = ..

type rgba = (t -> int * int * int * int) -> t -> int * int * int * int

type rgba_chain = (t -> int * int * int * int) ref

let chain : rgba_chain = ref (fun _ -> failwith "Unhandled color")

let register (rgba : rgba) = chain := rgba !chain

let rgba c = !chain c
