functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    if (length buildings = 0) then empty()
      else
        let
          fun skylineSortWithIndices (lower:int,higher:int):(int * int) seq =
            (*base cases*)
            if (higher - lower = 0) then empty()
            else if (higher - lower = 1) then
              let
                val elem = nth buildings lower
              in
                append (singleton ((#1elem),(#2elem)),singleton ((#3 elem),0))
              end
            else
              let 
                val (lowUnm,highUnm) =par(fn()=>skylineSortWithIndices(lower, (lower+higher) div 2),
                                          fn()=>skylineSortWithIndices((lower+higher) div 2,higher))
                (*zip the results with indicators of their source: ~1 means lower part,1 indicates higher*)
                val (low,high) = par(fn()=>zip lowUnm (tabulate (fn _=> ~1) (length lowUnm)),
                                      fn()=>zip highUnm (tabulate (fn _=> 1) (length highUnm)))
                (*sort*)
                fun compare (((a,_),_),((b,_),_)) = Int.compare (a,b)
                val beforeCombine = merge compare low high
                (*copy scan for two skylines*)
                fun Scanner c ((a:(int*int)*int),(b:(int*int)*int)) = if(#2 b = c) then b
                                                                      else ((#1(#1b),#2(#1a)),#2a)
                val (left,right) = par(fn()=>scani (Scanner ~1) ((0,0),1) beforeCombine,
                                       fn()=>scani (Scanner 1) ((0,0),~1) beforeCombine)
                (*pick out the higher skyline*)
                fun pick i = if(i<0 orelse i >= length beforeCombine) then (0,0)
                  else if (#2(nth left i) = 1 orelse #2(nth right i) = ~1) then (#1(nth beforeCombine i))
                  else (#1(#1(nth beforeCombine i)),Int.max((#2(#1(nth left i))),(#2(#1(nth right i)))))
                val withRepetitions = tabulate pick (length beforeCombine)
                (*remove repetitions*)
                fun noRepetition (0,a) = true
                  | noRepetition (i,a) = 
                    if(i<length withRepetitions) then 
                      (#2 (nth withRepetitions i) <> #2(nth withRepetitions (i-1)))
                    else 
                      false
              in
                filterIdx noRepetition withRepetitions
              end
        in
          skylineSortWithIndices (0,length buildings)
        end
end
