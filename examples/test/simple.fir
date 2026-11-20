IRSystem(
  {"input"}, {"output"},
  {
    IRActor("actor_1", Actor22, "add", {"s_in", "s_2"}, {"s_out", "s_1"}, ),
    IRDelay("delay_1", {LInt 0}, "s_1", "s_2", )
  },
  {
    IRSignal("s_in", TInt ("input", 1), ("actor_1", 1), ),
    IRSignal("s_1", TInt ("actor_1", 1), ("delay_1", 1), ),
    IRSignal("s_2", TInt ("delay_1", 1), ("actor_1", 1), ),
    IRSignal("s_out", TInt ("actor_1", 1), ("output", 1), )
  },
  {
    IRFunction("add", , ([TInt],[TInt]), )
  }
)
