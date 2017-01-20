functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      let
(* calculate two’s complement for the number to be subtracted*)
        fun reverse ZERO = ONE
          | reverse ONE = ZERO
        val flipped = map reverse y
        fun tabulator i = if(i<(length flipped)) then (nth flipped i) else ONE
        val adder = tabulate tabulator (length x)
        (* add to the two’s complement*)
        val resultWithExtraBit = x ++ (adder ++ singleton(ONE))
      	val resultWithTrailingZero = subseq resultWithExtraBit (0,((length resultWithExtraBit)-1))
        (* remove trailing zeroes*)
	      fun id i = i
	      val resultsWithIndex = zip resultWithTrailingZero (tabulate id (length resultWithTrailingZero))
	      fun filterer (ZERO,_)= false
	        | filterer (ONE,_) =true
      	val filteringZero = filter filterer resultsWithIndex
	      val result = subseq resultWithTrailingZero (0,#2(nth filteringZero (length filteringZero-1))+1)
      in 
        result      
      end
      
  val sub = op--
end
