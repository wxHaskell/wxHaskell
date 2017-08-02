{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
{-|
Module      :  Attributes
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable


Widgets @w@ can have attributes of type @a@ represented by the type 'Attr' @w a@.
An example of an attribute is 'Graphics.UI.WX.Classes.text' with type:

> text :: Attr (Window a) String

This means that any object derived from 'Window' has a 'Graphics.UI.WX.Classes.text' attribute of type 'String'.
An attribute can be read with the 'get' function:

> get w title           :: IO String

When an attribute is associated with a value, we call it a /property/ of type 'Prop' @w@.
Properties are constructed by assigning a value to an attribute with the (':=') constructor:

> text := "hello world"      :: Prop (Window a)

A list of properties can be set with the 'set' function:

> set w [text := "Hi"]   :: IO ()

The (':~') constructor is used to transform an attribute value with an update function.
For example, the 'interval' on a timer can be doubled with the following expression.

> set timer [interval :~ (*2)]

The functions 'get', 'set', (':='), and (':~') are polymorphic and work for all widgets, but
the @text@ attribute just works for windows. Many attributes work for different kind
of objects and are organised into type classes. Actually, the real type of the
'Graphics.UI.WX.Classes.text' attribute is:

> Textual w => Attr w String

and 'Window' derived objects are part of this class:

> instance Textual (Window a)

But also menus and status fields:

> instance Textual (Menu a)
> instance Textual (StatusField)

Sometimes, it is convenient to also get a reference to the object itself when setting
a property. The operators ('::=') and ('::~') provide this reference.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Attributes
    (
    -- * Attributes
      Attr, Prop((:=),(:~),(::=),(::~)), ReadAttr, WriteAttr, CreateAttr
    , get, set, swap
    , mapAttr, mapAttrW


    -- * Internal

    -- ** Attributes
    , newAttr, readAttr, writeAttr, nullAttr, constAttr, makeAttr
    
    -- ** Reflection
    , attrName, propName, containsProperty
    
    -- ** Reflective attributes
    , reflectiveAttr, createAttr, withProperty, findProperty
    , withStyleProperty, withStylePropertyNot

    -- *** Filter
    , PropValue(..)
    , filterProperty 

    -- ** Cast
    , castAttr, castProp, castProps
    ) where

import Graphics.UI.WX.Types
import Control.Monad (void)
import Data.Dynamic

infixr 0 :=,:~,::=,::~

-- | A property of a widget @w@ is an attribute that
-- is already associated with a value. .
data Prop w     = forall a. Attr w a := a         -- ^ Assign a value to an attribute.
                | forall a. Attr w a :~ (a -> a)  -- ^ Apply an update function to an attribute.
                | forall a. Attr w a ::= (w -> a) -- ^ Assign a value to an attribute with the widget as argument.
                | forall a. Attr w a ::~ (w -> a -> a) -- ^ Apply an update function to an attribute with the widget as an argument.

-- For debugging purposes
instance Show a => Show (Prop a) where
  show ((Attr string _ _ _ _) :=  _) = "Attr \"" ++ string ++ "\" _ _ _ _ :=  _"
  show ((Attr string _ _ _ _) :~  _) = "Attr \"" ++ string ++ "\" _ _ _ _ :~  _"
  show ((Attr string _ _ _ _) ::= _) = "Attr \"" ++ string ++ "\" _ _ _ _ ::= _"
  show ((Attr string _ _ _ _) ::~ _) = "Attr \"" ++ string ++ "\" _ _ _ _ ::~ _"


-- | An attribute that should be specified at creation time. Just for documentation purposes.
type CreateAttr w a = Attr w a

-- | A read-only attribute. Just for documentation purposes.
type ReadAttr w a = Attr w a

-- | A write-only attribute. Just for documentation purposes.
type WriteAttr w a = Attr w a

-- | Widgets @w@ can have attributes of type @a@.
data Attr w a   = Attr String (Maybe (a -> Dynamic, Dynamic -> Maybe a))  -- name, dynamic conversion
                              (w -> IO a) (w -> a -> IO ())               -- getter setter 
                              (w -> (a -> a) -> IO a)                     -- updater      

-- For debugging purposes
instance Show a => Show (Attr w a) where
  show (Attr string _ _ _ _) =
    "Attr \"" ++ string ++ "\" _ _ _ _"


-- | Cast attributes.
castAttr :: (v -> w) -> Attr w a -> Attr v a
castAttr coerce (Attr name mbdyn getter setter upd)
  = Attr name mbdyn (\v -> getter (coerce v)) (\v x -> (setter (coerce v) x))
                    (\v f -> upd (coerce v) f) 

-- | Cast properties
castProp :: (v -> w) -> Prop w -> Prop v
castProp coerce prop
  = case prop of
      (attr := x)   -> (castAttr coerce attr) := x
      (attr :~ f)   -> (castAttr coerce attr) :~ f
      (attr ::= f)  -> (castAttr coerce attr) ::= (\v -> f (coerce v))
      (attr ::~ f)  -> (castAttr coerce attr) ::~ (\v x -> f (coerce v) x)

-- | Cast a list of properties.
castProps :: (v -> w) -> [Prop w] -> [Prop v]
castProps coerce props
  = map (castProp coerce) props

-- | Create a /reflective/ attribute with a specified name: value can possibly be
-- retrieved using 'getPropValue'. Note: the use of this function is discouraged
-- as it leads to non-compositional code.
reflectiveAttr :: Typeable a => String -> (w -> IO a) -> (w -> a -> IO ()) -> Attr w a
reflectiveAttr name getter setter
  = Attr name (Just (toDyn, fromDynamic)) getter setter updater 
  where
    updater w f   = do x <- getter w; setter w (f x); return x

-- | Create a /reflective/ attribute with a specified name: value can possibly be
-- retrieved using 'getPropValue'. Note: the use of this function is discouraged
-- as it leads to non-compositional code.
createAttr :: Typeable a => String -> (w -> IO a) -> (w -> a -> IO ()) -> CreateAttr w a
createAttr name getter setter
  = reflectiveAttr name getter setter

-- | Create a new attribute with a specified name, getter, setter, and updater function.
makeAttr :: String -> (w -> IO a) -> (w -> a -> IO ()) -> (w -> (a -> a) -> IO a) -> Attr w a
makeAttr name getter setter updater
  = Attr name Nothing getter setter updater 


-- | Create a new attribute with a specified name, getter and setter function.
newAttr :: String -> (w -> IO a) -> (w -> a -> IO ()) -> Attr w a
newAttr name getter setter
  = makeAttr name getter setter updater 
  where
    updater w f   = do x <- getter w; setter w (f x); return x


-- | Define a read-only attribute.
readAttr :: String -> (w -> IO a) -> ReadAttr w a
readAttr name getter
  = newAttr name getter (\_w _x -> ioError (userError ("attribute '" ++ name ++ "' is read-only.")))

-- | Define a write-only attribute.
writeAttr :: String -> (w -> a -> IO ()) -> WriteAttr w a
writeAttr name setter
  = newAttr name (\_w -> ioError (userError ("attribute '" ++ name ++ "' is write-only."))) setter

-- | A dummy attribute.
nullAttr :: String -> WriteAttr w a
nullAttr name
  = writeAttr name (\_w _x -> return ())

-- | A constant attribute.
constAttr :: Typeable a => String -> a -> Attr w a
constAttr name x
  = newAttr name (\_w -> return x) (\_w _x -> return ())


-- | (@mapAttr get set attr@) maps an attribute of @Attr w a@ to
-- @Attr w b@ where (@get :: a -> b@) is used when the attribute is
-- requested and (@set :: a -> b -> a@) is applied to current
-- value when the attribute is set.
mapAttr :: (a -> b) -> (a -> b -> a) -> Attr w a -> Attr w b
mapAttr get' set' (Attr name _reflect getter setter updater)
    = Attr name Nothing
                (\w   -> do a <- getter w; return (get' a))
                (\w b -> do a <- getter w; setter w (set' a b))
                (\w f -> do a <- updater w (\a -> set' a (f (get' a))); return (get' a)) 

-- | (@mapAttrW conv attr@) maps an attribute of @Attr w a@ to
-- @Attr v a@ where (@conv :: v -> w@) is used to convert a widget
-- @v@ into a widget of type @w@.
mapAttrW :: (v -> w) -> Attr w a -> Attr v a
mapAttrW f attr
  = castAttr f attr

-- | Get the value of an attribute
--
-- > t <- get w text
--
get :: w -> Attr w a -> IO a
get w (Attr _name _reflect getter _setter _updater)
  = getter w

-- | Set a list of properties.
--
-- > set w [text := "Hi"]
--
set :: w -> [Prop w] -> IO ()
set w props
  = mapM_ setprop props
  where
    setprop ((Attr _name _reflect _getter  setter _updater) := x)
      = setter w x
    setprop ((Attr _name _reflect _getter _setter  updater) :~ f)
      = void $ updater w f
    setprop ((Attr _name _reflect _getter  setter _updater) ::= f)
      = setter w (f w)
    setprop ((Attr _name _reflect _getter _setter  updater) ::~ f)
      = void $ updater w (f w)

-- | Set the value of an attribute and return the old value.
swap :: w -> Attr w a -> a -> IO a
swap w (Attr _name _reflect _getter _setter updater) x
  = updater w (const x)

-- | Retrieve the name of an attribute
attrName :: Attr w a -> String
attrName (Attr name _ _ _ _)
  = name

-- | Retrieve the name of a property.
propName :: Prop w -> String
propName (attr :=  _x)   = attrName attr
propName (attr :~  _f)   = attrName attr
propName (attr ::= _f)   = attrName attr
propName (attr ::~ _f)   = attrName attr


-- | Is a certain property in a list of properties?
containsProperty :: Attr w a -> [Prop w] -> Bool
containsProperty attr props
  = containsPropName (attrName attr) props

-- | Is a certain property in a list of properties?
containsPropName :: String -> [Prop w] -> Bool
containsPropName name props
  = any (\p -> propName p == name) props


-- | Property value: used when retrieving a property from a list.
data PropValue a  = PropValue  a
                  | PropModify (a -> a)
                  | PropNone

instance Show a => Show (PropValue a) where
  show (PropValue x)   = "PropValue " ++ show x
  show (PropModify _f) = "PropModify"
  show (PropNone)      = "PropNone"

-- | Retrieve the value of a property and the list with the property removed.
filterProperty :: Typeable a => Attr w a -> [Prop w] -> (PropValue a, [Prop w])
filterProperty (Attr name _ _ _ _) props
  = walk [] PropNone props
  where
    -- Daan: oh, how a simple thing like properties can result into this... ;-)
    walk :: Typeable a => [Prop w] -> PropValue a -> [Prop w] -> (PropValue a, [Prop w])
    walk acc res props'
      = case props' of
          -- Property setter found.
          (((Attr attr (Just (todyn, _fromdyn)) _ _ _) := x):rest)  | name == attr
              -> case fromDynamic (todyn x) of
                   Just y  -> walk acc (PropValue y) rest
                   Nothing -> walk acc res props'
                   
          -- Property modifier found.
          (((Attr attr (Just (todyn,fromdyn)) _ _ _) :~ f):rest)  | name == attr
              -> let dynf x = case fromdyn (toDyn x) of
                                Just xx -> case fromDynamic (todyn (f xx)) of
                                             Just y  -> y
                                             Nothing -> x  -- identity
                                Nothing -> x -- identity
                 in case res of
                      PropValue x  -> walk acc (PropValue (dynf x)) rest
                      PropModify g -> walk acc (PropModify (dynf . g)) rest
                      PropNone     -> walk acc (PropModify dynf) rest

          -- Property found, but with the wrong arguments
          (((Attr attr _ _ _ _) :=  _) : _rest) | name == attr  -> stop
          (((Attr attr _ _ _ _) :~  _) : _rest) | name == attr  -> stop
          (((Attr attr _ _ _ _) ::= _) : _rest) | name == attr  -> stop
          (((Attr attr _ _ _ _) ::~ _) : _rest) | name == attr  -> stop

          -- Defaults
          (prop:rest)
              -> walk (prop:acc) res rest
          []  -> stop
       where
        stop  = (res, reverse acc ++ props')
  
               
-- | Try to find a property value and call the contination function with that value
-- and the property list witht the searched property removed. If the property is not
-- found, use the default value and the unchanged property list.
withProperty :: Typeable a => Attr w a -> a -> (a -> [Prop w] -> b) -> [Prop w] -> b
withProperty attr def cont props
  = case filterProperty attr props of
      (PropValue x, ps)  -> cont x ps
      (PropModify f, ps) -> cont (f def) ps
      (PropNone, ps)     -> cont def ps

-- | Try to find a property value. Return @Nothing@ if not found at all.
findProperty :: Typeable a => Attr w a -> a -> [Prop w] -> Maybe (a,[Prop w])
findProperty attr def props
  = case filterProperty attr props of
      (PropValue x, ps)  -> Just (x,ps)
      (PropModify f, ps) -> Just (f def,ps)
      (PropNone, _ps)    -> Nothing



-- | Transform the properties based on a style property.
withStyleProperty :: Attr w Bool -> Style -> ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
withStyleProperty prop flag 
  = withStylePropertyEx prop (bitsSet flag) (\isSet style -> if isSet then (style .+. flag) else (style .-. flag)) 

-- | Transform the properties based on a style property. The flag is interpreted negatively, i.e. @True@
-- removes the bit instead of setting it.
withStylePropertyNot :: Attr w Bool -> Style -> ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
withStylePropertyNot prop flag 
  = withStylePropertyEx prop (not . bitsSet flag) (\isSet style -> if isSet then (style .-. flag) else (style .+. flag)) 


-- | Transform the properties based on a style property.
withStylePropertyEx :: Attr w Bool -> (Style -> Bool) -> (Bool -> Style -> Style) -> ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
withStylePropertyEx prop def transform cont props style
  = case filterProperty prop props of
      (PropValue x, ps)  -> cont ps (transform x style) 
      (PropModify f, ps) -> cont ps (transform (f (def style)) style)
      (PropNone, ps)     -> cont ps style
