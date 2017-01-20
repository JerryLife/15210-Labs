functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
  let
    fun edgeMaker (out,inV) = map (fn i => (out,i)) inV
  in
    makeGraph (flatten (map edgeMaker S))
  end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    let
      val aspInfo = makeASP T w1
      val result = report aspInfo w2
    in
    result
    end

end
