functor MkBruteForcePD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Primitives

fun parenDist (parens :paren seq) : int option =
  let
    (*Function parenMatch determines if the sequence is matched *)
    fun parenMatch s =
      let 
        fun pm s = case showt s of EMPTY => (0,0) 
                            | ELT OPAREN => (0,1) 
                            | ELT CPAREN => (1,0) 
                            | NODE (l, r) => 
          let 
            val ((i,j),(k,l)) = par(fn () => pm l, fn () => pm r) 
          in 
            if j > k then (i, l+j-k) else (i+k-j, l) 
          end 
      in 
        (pm s = (0, 0)) andalso (length s > 0)
      end
  in
    (*if the string is unmatched, return NONE*)
    if(parenMatch parens = false) then NONE
      else
        let
          (*Function MatchDist finds the distance of the matching parenthesis with *)
          (* OPAREN at location i; if it’s a CPAREN, it returns 0*)
          fun MatchDist parens i =
          if((i >= length parens-1) orelse (nth parens i = CPAREN)) then 0 
            else 
              let 
                fun ending j = if (j > length parens-1) then 0 
                                else if(parenMatch (subseq parens (i,j-i+1))) then (j-i-1)
                                else (ending (j+1)) 
              in 
                ending (i+1) 
              end 
          val dists = tabulate (MatchDist parens) (length parens)
          (*result is the largest in distances*)
          val result = nth dists (argmax Int.compare dists)
        in
          SOME result
        end
    end
end
