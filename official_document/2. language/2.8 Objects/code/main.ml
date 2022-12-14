class stack_of_ints = 
object(self)
  val mutable stack_of_ints = ([] : int list)
  method push x =
    stack_of_ints <- x :: stack_of_ints
  method pop =
    let result = List.hd stack_of_ints in
      stack_of_ints <- List.tl stack_of_ints;
      result
  method peek =
    List.hd stack_of_ints
  method size = 
    List.length stack_of_ints
  end

let soi = new stack_of_ints

(* Polymorphic classes *)

class ['a] stack = 
object (self)
  val mutable list = ([] : 'a list)
  method push x = x :: list
  method pop = 
    let result = List.hd list in
      list <- List.tl list;
      result
  method peek =
     List.hd list
  method size = 
    List.length list
end

class virtual widget (name : string) = 
object (self)
  method get_name = name
  method virtual repaint : unit
end

class virtual container name =
  object (self)
    inherit widget name
    val mutable widgets = ([] : widget list)
    method add w =
      widgets <- w :: widgets
    method get_widgets =
      widgets
    method repaint =
      List.iter (fun w -> w#repaint) widgets
  end

