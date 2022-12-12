type student = {
  name: string;
  age: int;
  height: float;
}
let hello () = print_endline "Hello"

module Hello = struct
  let message = "Hello"
  let hello () = print_endline message
end

let goodbyte () = print_endline "Goodbyte"

let hello_goodbyte () =
  Hello.hello();
  goodbyte