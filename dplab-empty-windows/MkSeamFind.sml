functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq
 
  type 'a seq = 'a Seq.seq 

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  exception NotYetImplemented

  fun generateGradients ({width, height, data}:image):gradient seq seq = 
    raise NotYetImplemented

  fun findSeam (G:gradient seq seq) : int seq =
    raise NotYetImplemented

end