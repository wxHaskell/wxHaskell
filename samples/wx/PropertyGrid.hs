{--------------------------------------------------------------------------------
   List control demo.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore 

import Data.Traversable as Traversable

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- main gui elements
       f       <- frame [text := "Property Grid Sample"]
       -- panel: just for the nice grey color
       p       <- panel f []
       textlog <- textCtrl p [enabled := False, wrap := WrapLine]

       -- use text control as logger
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              

       -- propertyGrid
       pg  <- propertyGrid p [on propertyGridEvent := onPropertyGridEvent]

       -- add test data
       _ <- propertyCategoryCreate "alpha" >>= propertyGridAppend pg
       _ <- stringPropertyCreate "Name" "name" "Bob" >>= propertyGridAppend pg 
       _ <- propertyGridDisableProperty pg "name"

       _ <- intPropertyCreate "Age" "age" 32 >>= propertyGridAppend pg 
       _ <- boolPropertyCreate "Is member?" "bool" True >>= propertyGridAppend pg 

       _ <- propertyCategoryCreate "beta" >>= propertyGridAppend pg
       _ <- floatPropertyCreate "Score" "float" 3.14 >>= propertyGridAppend pg 
       _ <- dateTimeCreate >>= \d -> dateTimeNow d >> datePropertyCreate "Join date" "date" d >>= propertyGridAppend pg 
       _ <- filePropertyCreate "Data file" "file" "/home/" >>= propertyGridAppend pg 

       -- specify layout
       set f [layout     := container p $ margin 10 $ 
                            column 5 [ fill  $ widget pg
                                     , hfill $ widget textlog
                                     ]
             ,clientSize := sz 600 400
             ]

  where
    onPropertyGridEvent eventPropertyGrid
      = case eventPropertyGrid of
          PropertyGridChanged prop -> 
            showProp prop >>= logMessage . (++) "PropertyGrid changed "

          PropertyGridHighlighted maybeProp -> 
            let propStr = show `fmap` (showProp `Traversable.mapM` maybeProp)
              in propStr >>= logMessage . (++) "PropertyGrid highlighted "

          _other -> 
              logMessage ("Other propertyGrid event.")

    showProp p = do
      label_      <- pGPropertyGetLabel p
      typeString  <- pGPropertyGetValueType p
      valueString <- pGPropertyGetValueAsString p
      return $ label_ ++ " " ++ show (readAny typeString valueString)

data Any
  = IsString String
  | IsInteger Integer
  | IsBool Bool
  | IsDouble Double
  | IsDateTime String
  | IsUndefined
      deriving (Eq, Show)

readAny :: String -> String -> Any
readAny typeStr valueStr = maybe IsUndefined ($ valueStr) (lookup typeStr typeMap)
    where typeMap = 
            [ ("string", IsString)
            , ("long", IsInteger . read)
            , ("bool", IsBool . read)
            , ("double", IsDouble . read)
            , ("datetime", IsDateTime) ]

