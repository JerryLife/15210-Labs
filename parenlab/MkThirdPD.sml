functor MkThirdPD(structure P: PAREN_PACKAGE) : PAREN_DIST =
struct
structure P = P
open P
open Seq
open Primitives
	 
fun parenDist(parens : paren seq) : int option =
  let
   fun parenMatch s =
	 let fun pm s = case showt s 
	 of EMPTY => (0,0) 
	 | ELT OPAREN => (0,1) 
	 | ELT CPAREN => (1,0) 
	 | NODE (l, r) => 
	 let val ((i,j),(k,l)) = par(fn () => pm l, fn () => pm r) 
	 in 
	 if j > k then (i, l+j-k) else (i+k-j, l) 
	 end 
	 in 
	 pm s = (0, 0) 
	 end
      fun mapper OPAREN = 1
	| mapper CPAREN = ~1
      fun id i = i
      val tmpSeq = map mapper parens
	  val numSeqPair = scan op+ 0 tmpSeq
	  val numSeq = append ((#1numSeqPair),(singleton(#2numSeqPair)))
      val posSeq = tabulate id (length numSeq)
      val pairSeq = zip numSeq posSeq
      fun cond (0,_) = true
	| cond (_,_) = false
      val filteredSeq = filter cond pairSeq
      val positSeq = map #2 filteredSeq
      fun diff 0 = 0
	| diff i = if (i <= length positSeq) then nth positSeq i - nth positSeq (i-1)
		   else 0
      val resSeq = tabulate diff (length positSeq)
      val result = nth resSeq (argmax Int.compare resSeq)
  in
      if((parenMatch parens = false) orelse (length parens = 0)) then NONE
      else SOME (result-2)
  end
end 