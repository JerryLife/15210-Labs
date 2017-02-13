functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = (((Set.set) table)*int*int)
  type asp = (Set.set) table
  (*Graph is represented by adjacency table; the number of edges and of vertices are kept for fast lookup *)
  (*ASP is a graph consisting of only the edges on the shortest paths.*)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
  let
    val adjTbl = Table.map (Set.fromSeq) (Table.collect E)
    fun revPair (m,n) = (n,m)
    val revTbl = Table.map (Set.fromSeq) (Table.collect (map revPair E))
    fun first (i,_) = i
    val vertNum = Table.size (Table.merge (fn (i,j) => Set.empty()) (adjTbl,revTbl))
    in
    (adjTbl,vertNum,(Seq.length E))
    end

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
  #3(G)

  fun numVertices (G : graph) : int =
  #2(G)

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case (find (#1(G)) v) of
    NONE=>Seq.empty()
    |SOME s=>Set.toSeq s

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
  if(Seq.length(outNeighbors G v) = 0) then (Table.empty())
  else
    let
      val ivisited = Table.singleton(v,Set.empty())
      val itoVisit = Set.singleton(v)
      fun looper (visited,toVisit) = if(Set.size (toVisit) = 0) then (visited,toVisit)
      else
      let
      val newVisits = Seq.map (fn v =>(v,outNeighbors G v)) (Set.toSeq (toVisit))
      val patchNewVisits = Table.collect (Seq.flatten (Seq.map (fn (v,n) => (Seq.map (fn vert => (vert,v))) n) (newVisits)))
      val fixNewVisits = Table.map Set.fromSeq patchNewVisits
      val ntoVisit = Table.domain (Table.erase ((fixNewVisits),Table.domain(visited)))
      val nvisited = Table.merge (fn(i,j)=>i) ((visited),(fixNewVisits))
      in
      looper(nvisited,ntoVisit)
      end
in
#1(looper (ivisited,itoVisit))
end

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
 let fun reportList vert =
    case (find A vert) of
    NONE=>(Seq.empty())
    |SOME parentSet =>
    if(Set.size parentSet = 0) then Seq.singleton([vert])
    else let
        val parentSeq = Set.toSeq(parentSet)
        val pathsToParent = Seq.map reportList parentSeq
        fun parentFullPaths i = if (i >= Seq.length parentSeq) then Seq.empty()
          else Seq.map (fn param =>vert::param) (nth pathsToParent i)
     in Seq.flatten(Seq.tabulate parentFullPaths (Seq.length parentSeq)) end
    val seqres = Seq.map Seq.fromList (reportList v)
    in
    Seq.map Seq.rev seqres
    end

end
