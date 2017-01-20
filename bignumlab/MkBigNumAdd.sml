functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let
      fun first (i,_) = i
      fun second (_,i) = i
      (*perform bitwise adding and produce carries*)
      fun mapToPair ZERO ZERO = (ZERO,STOP)
        | mapToPair ZERO ONE = (ONE,PROP)
        | mapToPair ONE ZERO = (ONE,PROP)
        | mapToPair ONE ONE = (ZERO,GEN)
      fun bitReverse ZERO = ONE
        | bitReverse ONE = ZERO
      fun addedDigit i = if(i < Int.min(length x, length y)) then mapToPair (nth x i) (nth y i)
         else if(i<length x) then mapToPair ZERO (nth x i)
         else if (i<length y) then mapToPair ZERO (nth y i)
         else (ZERO,STOP)
      val beforeCarrying = tabulate addedDigit (Int.max(length x, length y))
      val carriesBefore = map second beforeCarrying
      (* propagates carries*)
      fun performCarrying (_,STOP) = STOP
        | performCarrying (i,PROP) =i
        | performCarrying (_,GEN) =GEN
      val carriesAfter = scani performCarrying PROP carriesBefore
      fun mapper GEN = true
        | mapper _ = false
      val realCarries = append(singleton(false),map mapper carriesAfter)
      val valNotCarried=append((map first beforeCarrying),singleton(ZERO))
      (*perform carries on the bits*)
      fun digitCarrying i = if(i >= length valNotCarried) then ZERO
            else if(nth realCarries i = true) then bitReverse (nth valNotCarried i)
            else (nth valNotCarried i)
      val resultsWithDup = tabulate digitCarrying (length valNotCarried) 
    in
      (* remove duplicate zero at end*)
      if(nth resultsWithDup ((length resultsWithDup)-1) = ONE) then resultsWithDup
      else subseq resultsWithDup (0,(length resultsWithDup)-1)
    end
    
  val add = op++
end
