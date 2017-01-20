functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = (point table) table

  fun makeCountTable (S : point seq) : countTable =
    let
    val horizonalSorted = Seq.sort (fn (a,b)=>compareKey (#1(a),#1(b))) S
    val insertion = fn (accm,nxt)=>insert (fn (a,b)=>b) nxt accm
    val tables = Seq.iterh insertion (empty()) (Seq.map (fn pt=>(#2pt,pt)) horizonalSorted)
    val t' = if (Seq.length S > 0) then Seq.append ((Seq.drop ((#1tables),1)),Seq.singleton(#2tables))
            else Seq.empty()
    in 
      fromSeq (Seq.zip (Seq.map #1 horizonalSorted) t')
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
   let
    val horIn = getRange T (xLeft,xRght)
    val greater = if (size horIn = 0) then 0 else size (getRange (#2(valOf (last horIn))) (yLo,yHi))
    val less = if (size horIn = 0) then 0 else size (getRange (#2(valOf(first horIn))) (yLo,yHi)) 
    in
      if (size horIn = 0) then 0 else (greater - less + 1)
    end
end