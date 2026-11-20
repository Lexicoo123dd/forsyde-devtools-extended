GFuncDef(TData(IntType), "main", {}, ([]->[]),
  SScope({
    SVarDecl(TIdent("token"), "input"),
    SVarDecl(TIdent("token"), "output"),
    SVarDecl(TData(IntType), "i"),
    SVarDecl(TData(IntType), "j"),
    SVarDef(TPointer(TIdent("channel")), "s_in", ECall("create_buffer_nonblocking", {ELit (LInt 4)})),
    SVarDef(TPointer(TIdent("channel")), "s_out", ECall("create_buffer_nonblocking", {ELit (LInt 2)})),
    SVarDef(TPointer(TIdent("channel")), "s_1", ECall("create_buffer_nonblocking", {ELit (LInt 1)})),
    SVarDef(TPointer(TIdent("channel")), "s_1_delay", ECall("create_buffer_nonblocking", {ELit (LInt 1)})),
    SExpr(ECall("writeToken", {EVar("s_1"), ELit (LInt 0)})),
    SWhile(ELit (LInt 1), SScope({
      SExpr(ECall("actor11SDF", {ELit (LInt 2), ELit (LInt 1), EVar("s_in"), EVar("s_1"), EVar("f_1")}))
    })),
    SArrayAssign("x", ELit (LInt 0), , ELit (LInt 3)),
    SArrayAssign("x", ELit (LInt 1), "foo", ELit (LInt 2)),
    SIf(EBinOp(<, EVar("i"), EVar("n")), SReturn, ),
    SIf(EBinOp(>, EVar("i"), ELit (LInt 0)), SReturn(ELit (LInt 1)), SReturn(ELit (LInt 0))),
    SReturn(ELit (LInt 0)),
    SReturn,
    SFor(SVarDef(TData(IntType), "i", ELit (LInt 0)), EBinOp(<, EVar("i"), EVar("n")), SExpr(EUnOp(++, EVar("i"))), SScope({
      SExpr(ECall("printf", {EString("%d\n"), EVar("i")}))
    })),
    SVarDef(TReference(TIdent("fifo")), "fifo", EVar("f")),
    SExpr(ECallExpr(EVar("printf"), {EString("Hi")})),
    SExpr(ECallExpr(EPointerAccess(EVar("a"), "funca"), {})),
    SExpr(ECallExpr(EMemberAccess(ECallExpr(EMemberAccess(EVar("a"), "getB"), {}), "printB"), {})),
    SExpr(EPointerAccess(EArrayAccess(EVar("p"), EVar("i")), "value")),
    SExpr(ECall("pthread_mutex_lock", {EReference(EPointerAccess(EVar("fifo"), "lock"))})),
    SVarDef(TData(IntType), "value", EDereference(EVar("p"))),
    SAssign(EDereference(EVar("p")), ELit (LInt 10)),
    SArrayDecl(TData(IntType), "input", {ELit (LInt 2)})
  })
)

GFuncDef(static, TVoid, "actor11SDF", {(TData(IntType), "consum"), (TData(IntType), "prod"), (TPointer(TIdent("channel")), "ch_in"), (TPointer(TIdent("channel")), "ch_out"), (TFunctionPointer(TVoid, {TPointer(TIdent("token")), TPointer(TIdent("token"))}), "f")}, ([IntType] -> [IntType]),
  SScope({})
)
