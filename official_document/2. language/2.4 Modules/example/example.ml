module type Hello_type = sig
  val hello : unit -> unit
end
module Hello : Hello_type
=
struct
  let message = "Hello"
  let hello () = print_endline message
end

let goodbyte () = print_endline "Goodbye"

let hello_goodbyte () =
  Hello.hello ();
  goodbyte ()