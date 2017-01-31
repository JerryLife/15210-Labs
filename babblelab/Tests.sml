structure Tests =
struct

  (* Do not remove the following line! *)
  val corpus = TextIO.inputAll (TextIO.openIn "corpus.txt")

  val testsChoose : (((string * int) list) * real) list  = [
    ([("test", 10)], 0.5),
    ([("test", 2), ("awesome", 2)], 0.5),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.47),
    ([("hello",1),("goodbye",999)],0.0),
    ([("awesome",4),("wow",888)],1.0)
  ]

  (* You must add test kgrams for the corpus in corpus.txt as part of task 5.5
   * You may edit corpus.txt -- it will be handed in.
   *
   * You may also add other tests, which use other corpi (corpuses?), but those
   * corpuses will not be submitted. *)
  val testsKGramStats : ((string * int) * (string list)) list = [
    ((corpus, 50),
        ["direction",
         "time",
         "direction of time",
         "would write",
         "What Eddington says about",
         "British Museum",
         "write all the books in the British Museum",
         (*Ending phrase*)"through his head",
         (*Unique*)"Olaf",
         "imperially slim",
         (*Non-existent*)"what",
         (*A Number of apperances*)"What",
         (* Long and ending *)"So on we worked and waited for the light  And went without the meat and cursed the bread  And Richard Cory one calm summer night  Went home and put a bullet through his head"
         ])
  ]


end
