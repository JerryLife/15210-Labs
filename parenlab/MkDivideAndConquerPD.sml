functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
    let
      (* the five-tuple (in,lmo,rmc,ounm,cunm) has the following meanings:*)
      (*  in: the largest matching distance within the string *)
      (* lmo: the distance from the leftmost unmatched OPAREN to the end *)
      (* rmc: the distance from the rightmost unmatched CPAREN to the end *)
      (*ounm: the number of unmatched OPARENs*)
      (*cunm: the number of unmatched CPARENs*)
      fun allInfo substr =
        case showt substr of 
                   EMPTY =>(0,0,0,0,0)
            | ELT OPAREN =>(0,1,0,1,0)
            | ELT RPAREN => (0,0,1,0,1)
            | NODE (l,r) =>
                let 
                  val ((lin,llmo,lrmc,lounm,lcunm),(rin,rlmo,rrmc,rounm,rcunm)) = 
                    par(fn()=>allInfo l, fn()=>allInfo r)
                  val aounm = if(lounm > rcunm) then (lounm-rcunm) else 0;
                  val acunm = if(rcunm > lounm) then (rcunm-lounm) else 0;
                  val ounm = rounm + aounm
                  val cunm = lcunm + acunm
                  val lmo = if(lounm > rcunm) then (llmo + length r) else rlmo;
                  val rmc = if(rcunm > lounm) then (rrmc + length l) else lrmc;
                  val nin = Int.max(Int.max(lin,rin),llmo+rrmc);
                in 
                  (nin,lmo,rmc,ounm,cunm)
                end
      val (res,_,_,reounm,recunm) = allInfo(parens)
    in
      if(reounm=0 andalso recunm=0 andalso res-2>=0) then SOME (res-2)
      else NONE
    end
end
