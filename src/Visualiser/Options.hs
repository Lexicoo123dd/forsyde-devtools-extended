{-# LANGUAGE OverloadedStrings #-}

-- | Display and navigation choices exposed through KLighD's Options panel.
module Visualiser.Options
  ( VisualiserOptions (..),
    defaultVisualiserOptions,
    valuedSynthesisOptions,
    applySynthesisOptions
  ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Aeson.KeyMap ((!?))
import qualified Data.Foldable as F
import qualified Data.Text as T

data VisualiserOptions = VisualiserOptions
  { showFiringOrder :: Bool,
    diagramToSource :: Bool,
    sourceToDiagram :: Bool
  } deriving (Eq, Show)

defaultVisualiserOptions :: VisualiserOptions
defaultVisualiserOptions = VisualiserOptions True True True

-- KLighD renders type 0 as its native checkbox; values are sent back by ID.
valuedSynthesisOptions :: VisualiserOptions -> [A.Value]
valuedSynthesisOptions options =
  [ option "forsyde.showFiringOrder" "Show firing order" "Show the actor firing order in the diagram." (showFiringOrder options),
    option "forsyde.diagramToSource" "Diagram to source" "Select source code when clicking a diagram element." (diagramToSource options),
    option "forsyde.sourceToDiagram" "Source to diagram" "Highlight diagram elements selected in source code." (sourceToDiagram options)
  ]
  where
    option :: T.Text -> T.Text -> T.Text -> Bool -> A.Value
    option ident title description value = A.object
      [ "synthesisOption" .= A.object
          [ "id" .= ident, "name" .= title, "description" .= description,
            "type" .= (0 :: Int), "sourceHash" .= ident,
            "initialValue" .= True, "values" .= ([] :: [T.Text])
          ],
        "currentValue" .= value
      ]

-- Version 0.8.1 sends changed options as flat objects with id and currentValue.
applySynthesisOptions :: A.Value -> VisualiserOptions -> VisualiserOptions
applySynthesisOptions (A.Object params) options =
  case params !? "synthesisOptions" of
    Just (A.Array values) -> foldl' apply options (F.toList values)
    _ -> options
  where
    apply current (A.Object value) = case (value !? "id", value !? "currentValue") of
      (Just (A.String "forsyde.showFiringOrder"), Just (A.Bool enabled)) -> current {showFiringOrder = enabled}
      (Just (A.String "forsyde.diagramToSource"), Just (A.Bool enabled)) -> current {diagramToSource = enabled}
      (Just (A.String "forsyde.sourceToDiagram"), Just (A.Bool enabled)) -> current {sourceToDiagram = enabled}
      _ -> current
    apply current _ = current
applySynthesisOptions _ options = options
