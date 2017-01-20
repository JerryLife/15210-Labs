functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
fun MST (E : edge seq, n : int) : edge seq = 
  let
    val sorted = rev (sort (fn ((s1,d1,w1),(s2,d2,w2))=>Int.compare (w1,w2)) E)
    val labeled = map (fn (s,d,w)=>(s,(d,w,(s,d,w)))) sorted
    val verts = tabulate (fn i => i) n
    fun helper (Vs,Es,T) s =
    let
      val coins = Rand.flip s n
      fun hToT (s,(d,w,l)) = (nth coins s = 0) andalso (nth coins d = 1)
      val minEs = filter (fn (_,(a,_,_))=>a >= 0) (enum (inject Es (tabulate (fn _ => (~1,~1,(~1,~1,~1))) n)))
      val contractions = filter hToT minEs
      val conts = map (fn (s,(d,_,_))=>(s,d)) contractions
      val P = inject conts Vs
      val T' = append ((map (fn (_,(_,_,l)) => l) contractions),T)
      val E' = map (fn (s,(d,w,l)) => ((nth P s),(nth P d,w,l))) Es
      val updE = filter (fn (s,(d,_,_))=> s <> d) E'
    in 
      if (length Es = 0) then T else helper (P,updE,T') (Rand.next s)
    end
  in
    helper (verts,labeled,empty()) (Rand.fromInt 2017)
  end

end
