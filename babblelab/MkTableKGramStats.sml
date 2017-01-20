functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq
			
  (* You must define the abstract kgramstats type *)
  type kgramstats =(int*((string hist) T.table))

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
	val myTokens = append ((tokens (not o Char.isAlphaNum) corpus),singleton(""))
	fun genkgrams k = if(k=0) then zip (tabulate (fn i => empty()) (length myTokens)) myTokens
	  else 
	  let
	      fun genMaxGrams begin = if(begin + k > (length myTokens-1)) then (empty(),"") 
        			      else (subseq myTokens (begin,k),nth myTokens (begin+k))
	      val MaxGrams = tabulate genMaxGrams ((length myTokens)-k)		      
	  in 
	    append(genkgrams (k-1),MaxGrams) 
	  end
	val tblRaw = collect (collate String.compare) (genkgrams maxK)
	val rawGrams = map (fn (i,j)=>i) tblRaw
  	val filtered = map (filter (fn i => not (i = ""))) (map (fn(i,j)=>j) tblRaw)
	val rawHist = map (histogram String.compare) filtered
  in (maxK,Table.fromSeq(zip rawGrams rawHist)) end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq = 
	valOf (Table.find (#2(stats)) kgram)

  fun maxK (stats : kgramstats) : int =
      #1(stats)
end
