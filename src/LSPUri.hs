module LSPUri (filePathToUriText, uriTextToFilePath) where

import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

-- | Convert an LSP file URI to a native file path.
uriTextToFilePath :: T.Text -> Maybe FilePath
uriTextToFilePath = LSP.uriToFilePath . LSP.Uri

-- | Convert a native file path to an LSP file URI.
filePathToUriText :: FilePath -> T.Text
filePathToUriText = LSP.getUri . LSP.filePathToUri
