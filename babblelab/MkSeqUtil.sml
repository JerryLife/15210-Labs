functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  exception ChooseParamException

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    if(length hist = 0 orelse p >1.0 orelse p < 0.0) then raise ChooseParamException
    else let 
      val keys = map (fn (i,j)=>i) hist
      val freqs = map (fn (i,j)=>j) hist
      val sums = scani op+ 0 freqs
      val itoReal = Real.fromInt
      val dist = map (fn x =>(itoReal x)/(itoReal(nth sums ((length sums)-1)))) sums
      val dists = zip keys dist
      fun scanner ((a:'a*real),(b:'a*real)) = if ((#2a)<p) then b else a 
    in
      #1(reduce scanner ((nth keys 0),0.0) dists)
    end

end
