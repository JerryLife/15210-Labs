functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq
 
  type 'a seq = 'a Seq.seq 

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun generateGradients ({width, height, data}:image):gradient seq seq = 
  let
    fun square x = x * x
    fun gradient x y = 
      if(x=width-1) then Real.posInf
      else if (y=height-1) then 0.0
      else
      let 
        val belowr = square(#r(nth (nth data y) x)- #r(nth (nth data (y+1)) x))
        val belowg = square(#g(nth (nth data y) x)- #g(nth (nth data (y+1)) x))
        val belowb = square(#b(nth (nth data y) x)- #b(nth (nth data (y+1)) x))
        val rightr = square(#r(nth (nth data y) x)- #r(nth (nth data y) (x+1)))
        val rightg = square(#g(nth (nth data y) x)- #g(nth (nth data y) (x+1)))
        val rightb = square(#b(nth (nth data y) x)- #b(nth (nth data y) (x+1)))
      in 
        Math.sqrt(belowb + belowr + belowg + rightb + rightg + rightr) 
      end
  in 
    tabulate (fn i => (tabulate (fn j => gradient j i) width)) height 
  end

  fun findSeam (G:gradient seq seq) : int seq =
  let
    fun equalWithinEps (a,b) = (a - b < 0.0000000001)
    fun calcCost (row:int) (currRes:gradient seq seq) = if(row = (length G - 1)) then currRes
      else 
      let
        val rowCost = nth G row
        val lastCosts = nth currRes (row - 1)
        fun findLastCost i = if(i=0) then Real.min(nth lastCosts 0,nth lastCosts 1)
                             else if(i=length rowCost - 1) then Real.posInf
                             else Real.min(nth lastCosts (i-1),Real.min(nth lastCosts i,nth lastCosts (i+1)))
        val costs = tabulate findLastCost (length lastCosts)
      in 
        (calcCost (row+1) (append(currRes,singleton(map2 op+ rowCost costs)))) 
      end
    val allCosts = calcCost 1 (singleton((nth G 0)))
    val finalMinCost = reduce (fn ((i,j),(k,l))=>if j < l then (i,j) else (k,l)) (0,Real.posInf) 
                        (enum((nth allCosts ((length allCosts)-1))))
    val startingSeq = fromList [#1finalMinCost,#1finalMinCost] 
    fun findSeq rowNum currSeq = if (rowNum < 0) then currSeq else
      let
      val middle = nth currSeq 0
      val toSub = nth (nth G (rowNum + 1)) middle
      val theCost =  nth (nth allCosts (rowNum + 1)) middle - toSub
      val toAppend = if (middle = 0) then (if (equalWithinEps((nth (nth allCosts rowNum) 0),theCost)) then 0 else 1)
      else if(equalWithinEps(nth (nth allCosts rowNum) middle,theCost)) then middle
      else if (middle < length (nth G 0)-2) then 
      (if (equalWithinEps(nth (nth allCosts rowNum) (middle+1),theCost)) then (middle+1) else (middle-1))
      else (middle-1)
      in
        findSeq (rowNum-1) (append (singleton(toAppend),currSeq))
      end
  in
    findSeq (length allCosts-2) startingSeq
  end
end