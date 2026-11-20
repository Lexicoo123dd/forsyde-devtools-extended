module ProceduralIRToCSpec (spec) where

import ArgumentsMain
import Compiler.ProceduralIR
import Compiler.ProceduralIRToC
import Compiler.Types
import Data.Char (isSpace)
import Test.Hspec

exampleProceduralIR :: Program
exampleProceduralIR =
  Prog
    [ GFuncDef
        Nothing
        (TData TInt)
        "main"
        []
        ([], [])
        ( SScope
            [ SVarDecl (TIdent "token") "input",
              SVarDecl (TIdent "token") "output",
              SVarDecl (TData TInt) "i",
              SVarDecl (TData TInt) "j",
              SVarDef
                (TPointer (TIdent "channel"))
                "s_in"
                ( ECall
                    "create_buffer_nonblocking"
                    ([ELit (LInt 4)])
                ),
              SVarDef
                (TPointer (TIdent "channel"))
                "s_out"
                ( ECall
                    "create_buffer_nonblocking"
                    ([ELit (LInt 2)])
                ),
              SVarDef
                (TPointer (TIdent "channel"))
                "s_1"
                ( ECall
                    "create_buffer_nonblocking"
                    ([ELit (LInt 1)])
                ),
              SVarDef
                (TPointer (TIdent "channel"))
                "s_1_delay"
                ( ECall
                    "create_buffer_nonblocking"
                    ([ELit (LInt 1)])
                ),
              SExpr
                ( ECall
                    "writeToken"
                    ( [ EVar "s_1",
                        ELit (LInt 0)
                      ]
                    )
                ),
              SWhile
                (ELit (LInt 1))
                ( SScope
                    [ SExpr
                        ( ECall
                            "actor11SDF"
                            [ ELit (LInt 2),
                              ELit (LInt 1),
                              EVar "s_in",
                              EVar "s_1",
                              EVar "f_1"
                            ]
                        )
                    ]
                ),
              SArrayAssign "x" (ELit (LInt 0)) Nothing (ELit (LInt 3)),
              SArrayAssign "x" (ELit (LInt 1)) (Just "foo") (ELit (LInt 2)),
              SIf
                (EBinOp Less (EVar "i") (EVar "n"))
                ((SScope [(SReturn Nothing)]))
                Nothing,
              SIf
                (EBinOp Greater (EVar "i") (ELit (LInt 0)))
                ((SScope [SReturn (Just (ELit (LInt 1)))]))
                (Just (SScope [(SReturn (Just (ELit (LInt 0))))])),
              SReturn (Just (ELit (LInt 0))),
              SReturn Nothing,
              SFor
                (SVarDef (TData TInt) "i" (ELit (LInt 0)))
                (EBinOp Less (EVar "i") (EVar "n"))
                (SExpr (EUnOp Increment (EVar "i")))
                (SScope [SExpr (ECall "printf" [EString "%d\n", EVar "i"])]),
              SVarDef
                (TReference (TIdent "fifo"))
                "fifo"
                (EVar "f"),
              SExpr (ECallExpr (EVar "printf") [EString "Hi"]),
              SExpr (ECallExpr (EPointerAccess (EVar "a") "funca") []),
              SExpr
                ( ECallExpr
                    ( EMemberAccess
                        (ECallExpr (EMemberAccess (EVar "a") "getB") [])
                        "printB"
                    )
                    []
                ),
              SExpr
                ( EPointerAccess
                    (EArrayAccess (EVar "p") (EVar "i"))
                    "value"
                ),
              SExpr
                ( ECall
                    "pthread_mutex_lock"
                    [ EReference
                        (EPointerAccess (EVar "fifo") "lock")
                    ]
                ),
              SVarDef
                (TData TInt)
                "value"
                (EDereference (EVar "p")),
              SAssign
                (EDereference (EVar "p"))
                (ELit (LInt 10)),
              SArrayDecl
                (TData TInt)
                "input"
                [ELit (LInt 2)]
            ]
        ),
      GFuncDef
        (Just Static)
        TVoid
        "actor11SDF"
        [ ((TData TInt), "consum"),
          ((TData TInt), "prod"),
          (TPointer (TIdent "channel"), "ch_in"),
          (TPointer (TIdent "channel"), "ch_out"),
          ( TFunctionPointer
              TVoid
              [ (TPointer (TIdent "token")),
                (TPointer (TIdent "token"))
              ],
            "f"
          )
        ]
        ([TInt], [TInt])
        (SScope [])
    ]

spec :: Spec
spec = beforeAll readExpectedCode $ do
  describe "Procedural IR To C Codegen" $ do
    it "Test hand-crafted Procedural IR" $ \simpleCString -> do
      let cString = translateProgram exampleProceduralIR PC StdIn False
      normalize cString `shouldBe` normalize simpleCString
  where
    normalize = filter (not . isSpace)
    readExpectedCode = readFile "examples/test/simple.c"
