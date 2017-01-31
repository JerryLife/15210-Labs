functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq
	   
  exception NoData

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    if(KS.maxK(stats)=0) then
     raise NoData
     else
	let
	    val determineRandom = R.randomRealSeq seed NONE n
	    fun genKGram (gram,randomNum) = 
          let 
      		val toChoose = if((length gram) < KS.maxK(stats)) then (KS.lookupExts stats gram)
      		else(KS.lookupExts stats (drop (gram,(length gram)-KS.maxK(stats))))
      		val result = if(length toChoose = 0) then genKGram (gram,randomNum)
      		else append(gram,(singleton(Util.choose toChoose randomNum)))
      	  in
      	    result
      	  end
	    val result = iter genKGram (empty()) determineRandom
	    fun resultWithSpaces i = if(i mod 2 = 0) then (nth result (i div 2))
				                        else " "
	    val finalResult = tabulate resultWithSpaces (2* (length result) - 1)
	    val punctuatedFinal = append (finalResult, singleton("."))
	in
	    String.concatWith "" (toList(punctuatedFinal))
	end
	    		     
    fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    if (KS.maxK(stats)=0) then 
      raise NoData
    else
      let
	    val Seeds = R.randomIntSeq seed NONE n
        val wordLengths = R.randomIntSeq (R.fromInt(nth Seeds 0)) (SOME (5,10)) n
        val document = flatten (tabulate (fn i=> fromList[(randomSentence stats (nth wordLengths i+1) (R.fromInt (nth Seeds i))), " "]) n)
      	val finalDoc = take (document, (length document - 1))
      in
        String.concatWith "" (toList(finalDoc))
      end

end
