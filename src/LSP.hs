{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import ArgumentsLSP
import Colog.Core ((<&))
import qualified Colog.Core as L
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable as F
import Data.Function
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import ForSyDeIR (IRSystem)
import qualified Language.LSP.Logging as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import LSPUri (filePathToUriText, uriTextToFilePath)
import Network.Socket
import Options.Applicative
import Prettyprinter
import SDFSchedule (Schedule (..))
import System.IO
import Visualiser.KLighD (forSyDeIRToGraph)
import API

diagramAcceptMethod :: LSP.SMethod (LSP.Method_CustomMethod "diagram/accept")
diagramAcceptMethod = (LSP.SMethod_CustomMethod (Proxy @"diagram/accept"))

diagramOpenInTextEditor :: LSP.SMethod (LSP.Method_CustomMethod "diagram/openInTextEditor")
diagramOpenInTextEditor = (LSP.SMethod_CustomMethod (Proxy @"diagram/openInTextEditor"))

setPreferencesMethod :: LSP.SMethod (LSP.Method_CustomMethod "keith/preferences/setPreferences")
setPreferencesMethod = (LSP.SMethod_CustomMethod (Proxy @"keith/preferences/setPreferences"))

-- | Send our supported syntheses to the LSP client (KLighD-VSCode)
setSynthesis :: T.Text -> A.Value
setSynthesis _clientId =
  A.object
    [ "clientId" .= _clientId,
      "action"
        .= A.object
          [ "kind" .= ("setSyntheses" :: T.Text),
            "syntheses"
              .= Seq.fromList
                [ A.object
                    [ "id" .= ("se.kth.forsyde-devtools.dummyid" :: T.Text),
                      "displayName" .= ("ForSyDe Shallow" :: T.Text)
                    ]
                ]
          ]
    ]

-- | Send the supported options for the file
updateOptions :: FilePath -> T.Text -> A.Value
updateOptions f _clientId =
  A.object
    [ "clientId" .= _clientId,
      "action"
        .= A.object
          [ "kind" .= ("updateOptions" :: T.Text),
            "valuedSynthesisOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "layoutOptions" .= (Seq.empty :: Seq.Seq A.Object),
            "actions" .= (Seq.empty :: Seq.Seq A.Object),
            "modelUri" .= filePathToUriText f
          ]
    ]

-- | Send the graph for layout and display to the LSP client (KLighD-VSCode)
requestBounds :: FilePath -> T.Text -> IRSystem -> Maybe Schedule -> A.Value
requestBounds f _clientId ir sched =
  A.object
    [ "clientId" .= _clientId,
      "action"
        .= A.object
          [ "kind" .= ("requestBounds" :: T.Text),
            "newRoot"
              .= forSyDeIRToGraph f ir sched
          ]
    ]

diagramOpenInTextEditorMessage :: String -> Int -> Int -> Int -> Int -> A.Value
diagramOpenInTextEditorMessage uri sline scol eline ecol =
  A.object
    [ "location"
        .= A.object
          [ "uri" .= filePathToUriText uri,
            "range"
              .= A.object
                [ "start"
                    .= A.object
                      [ "line" .= (sline - 1),
                        "character" .= (scol - 1)
                      ],
                  "end"
                    .= A.object
                      [ "line" .= (eline - 1),
                        "character" .= (ecol - 1)
                      ]
                ]
          ],
      "forceOpen" .= False
    ]

recomputeModel :: LSP.LspT API.Config IO ()
recomputeModel = do
  config <- LSP.getConfig
  case file config of
    Nothing -> pure ()
    Just sourceFile -> do
      let options = API.CompileOptions { forSyDePath = forSyDePkg config }
      result <- liftIO $ API.compileFile options sourceFile
      case result of
        Left err -> do
          dualLogger <& ("Compilation failed: " <> T.show err) `L.WithSeverity` L.Error
        Right compiled -> do
          LSP.setConfig config
            { system = Just (resultIR compiled),
              schedule = Just (resultSchedule compiled) }


-- | The static notification and request handlers we support
handlers :: LSP.Handlers (LSP.LspM API.Config)
handlers =
  mconcat
    [ LSP.notificationHandler LSP.SMethod_Initialized $ \_not -> pure (),
      LSP.notificationHandler setPreferencesMethod $ \_not -> pure (),
      LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_not -> pure (),
      LSP.requestHandler LSP.SMethod_Initialize $ \_req _resp -> do
        _resp
          ( Right $
              LSP.InitializeResult
                { _capabilities =
                    LSP.ServerCapabilities
                      { _positionEncoding = Nothing,
                        _textDocumentSync = Nothing,
                        _notebookDocumentSync = Nothing,
                        _completionProvider = Nothing,
                        _hoverProvider = Nothing,
                        _signatureHelpProvider = Nothing,
                        _declarationProvider = Nothing,
                        _definitionProvider = Nothing,
                        _typeDefinitionProvider = Nothing,
                        _implementationProvider = Nothing,
                        _referencesProvider = Nothing,
                        _documentHighlightProvider = Nothing,
                        _documentSymbolProvider = Nothing,
                        _codeActionProvider = Nothing,
                        _codeLensProvider = Nothing,
                        _documentLinkProvider = Nothing,
                        _colorProvider = Nothing,
                        _workspaceSymbolProvider = Nothing,
                        _documentFormattingProvider = Nothing,
                        _documentRangeFormattingProvider = Nothing,
                        _documentOnTypeFormattingProvider = Nothing,
                        _renameProvider = Nothing,
                        _foldingRangeProvider = Nothing,
                        _selectionRangeProvider = Nothing,
                        _executeCommandProvider = Nothing,
                        _callHierarchyProvider = Nothing,
                        _linkedEditingRangeProvider = Nothing,
                        _semanticTokensProvider = Nothing,
                        _monikerProvider = Nothing,
                        _typeHierarchyProvider = Nothing,
                        _inlineValueProvider = Nothing,
                        _inlayHintProvider = Nothing,
                        _diagnosticProvider = Nothing,
                        _workspace = Nothing,
                        _experimental = Nothing
                      },
                  _serverInfo = Nothing
                }
          )
        pure (),
      LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeWatchedFiles $ \_not -> do
        recomputeModel
        sendModel,
      LSP.notificationHandler LSP.SMethod_TextDocumentDidSave $ \_not -> do
        recomputeModel
        sendModel,
      LSP.notificationHandler diagramAcceptMethod $ \LSP.TNotificationMessage {_params = p} -> do
        initialConfig <- LSP.getConfig
        -- What file should we use?
        let newfile = maybe (maybe "" id $ file initialConfig) id (getFilePathFromClient p)
        -- What clientId should we use?
        let newId = maybe (maybe ("sprotty" :: T.Text) id $ clientId initialConfig) id (getClientId p)
        -- Update config with new data
        LSP.setConfig
          initialConfig
            { file = Just newfile,
              clientId = Just newId
            }

        -- Recompute model and send if the client wants it
        if shouldUpdate p
          then recomputeModel >> sendModel
          else pure ()

        -- Get location information on selected object
        config <- LSP.getConfig
        let sel = getSelected p & map (T.split (\_c -> _c == '$')) & map last & map T.unpack
        let spans = API.getSelectedSpans sel config
        if length spans > 0
          then liftIO $ stderrLogger <& ("Selected: " <> T.show spans) `L.WithSeverity` L.Debug
          else pure ()

        -- Send selection messages for all of the currently selected elements
        _ <-
          traverse
            ( \(fname, sl, sc, el, ec) ->
                LSP.sendNotification diagramOpenInTextEditor $
                  diagramOpenInTextEditorMessage fname sl sc el ec
            )
            spans

        pure ()
    ]
  where
    sendModel :: LSP.LspT API.Config IO ()
    sendModel = do
      config@API.Config {file = f, clientId = c, system = s, schedule = curSched} <- LSP.getConfig
      case (f, c, s) of
        (Just curFile, Just curId, Just curSystem) ->
          LSP.sendNotification diagramAcceptMethod (setSynthesis curId)
            >> LSP.sendNotification diagramAcceptMethod (updateOptions curFile curId)
            >> LSP.sendNotification diagramAcceptMethod (requestBounds curFile curId curSystem curSched)
        _ -> dualLogger <& ("does not have enough information to send diagram: " <> T.show config) `L.WithSeverity` L.Error
    getKey key = \case
      A.Object o -> o !? key
      _ -> Nothing
    getKind params =
      Just params
        >>= getKey "action"
        >>= getKey "kind"
        >>= \case
          A.String _a -> Just $ T.unpack _a
          _ -> Nothing
    shouldUpdate params =
      case getKind params of
        Just "requestModel" -> True
        Just "refreshDiagram" -> True
        _ -> False
    getSelected params =
      Just params
        >>= getKey "action"
        >>= getKey "selectedElementsIDs"
        >>= \case
          A.Array _a -> Just $ F.toList _a
          _ -> Nothing
        & maybeToList
        & mconcat
        & foldr
          ( \j l ->
              case j of
                A.String s -> s : l
                _ -> l
          )
          []
    getFilePathFromClient params =
      Just params
        >>= getKey "action"
        >>= getKey "options"
        >>= getKey "sourceUri"
        >>= \case
          A.String uri -> uriTextToFilePath uri
          _ -> Nothing
    getClientId params =
      Just params
        >>= getKey "clientId"
        >>= \case
          A.String _a -> Just _a
          _ -> Nothing

logToText :: LSP.LspServerLog -> T.Text
logToText = T.show . pretty

formatOut :: L.WithSeverity T.Text -> String
formatOut (L.WithSeverity m s) =
  show s <> ": " <> T.unpack m

stderrLogger :: L.LogAction IO (L.WithSeverity T.Text)
stderrLogger = L.cmap formatOut L.logStringStderr

clientLogger :: L.LogAction (LSP.LspM API.Config) (L.WithSeverity T.Text)
clientLogger = LSP.defaultClientLogger

dualLogger :: L.LogAction (LSP.LspM API.Config) (L.WithSeverity T.Text)
dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

runServerC :: Handle -> Handle -> LSP.ServerDefinition API.Config -> IO Int
runServerC =
  LSP.runServerWithHandles
    (L.cmap (fmap logToText) stderrLogger)
    (L.cmap (fmap logToText) dualLogger)

-- | Process arguments for the LSP and run it
main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (arguments <**> helper)
        ( fullDesc
            <> progDesc "Run a ForSyDe LSP"
            <> header "ForSyDe DevTools"
        )

-- | Start the LSP
run :: Arguments -> IO ()
run (Arguments comm (Host ip) (TCP p) i_f pkgPath) =
  case (i_f, comm) of
    (FromClient, CommTcp) ->
      runTCPServer (Just ip) p lsp
      where
        lsp s = do
          handle <- socketToHandle s ReadWriteMode
          void $ runServerC handle handle serverDef
    (FromClient, CommStdio) ->
      void $ runServerC stdin stdout serverDef
    (InputFile f, _) -> do
      let options = API.CompileOptions { forSyDePath = pkgPath }
      result <- liftIO $ API.compileFile options f
      case result of
        Left err -> error (show err)
        Right CompileResult{resultIR = forsydeIR, resultSchedule = sched} -> do
          let graphMessage = requestBounds f "sprotty" forsydeIR (Just sched)
          BSL8.putStrLn $ AP.encodePretty graphMessage
  where
    serverDef =
      LSP.ServerDefinition
        { parseConfig = const $ const $ Right API.defaultConfig {forSyDePkg = pkgPath},
          onConfigChange = const $ pure (),
          defaultConfig = API.defaultConfig {forSyDePkg = pkgPath},
          configSection = "demo",
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = \_caps -> handlers,
          interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO,
          options = LSP.defaultOptions
        }

-- | Listen on host and port, as well as accept and fork off connections
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a1) -> IO a2
runTCPServer host port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) host (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
