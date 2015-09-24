{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

{-
  JQuery bindings, loosely based on fay-jquery
-}

module JavaScript.JQuery ( JQuery(..)
                         , Event(..)
                         , EventType
                         , Selector
                         , Method(..)
                         , AjaxSettings(..)
                         , AjaxResult(..)
                         , ajax
                         , HandlerSettings(..)
                         , addClass
                         , animate
                         , getAttr
                         , setAttr
                         , hasClass
                         , getHtml
                         , setHtml
                         , getProp
                         , setProp
                         , removeAttr
                         , removeClass
                         , removeProp
                         , getVal
                         , setVal
                         , getText
                         , setText
                         , holdReady
                         , selectElement
                         , selectObject
                         , select
                         , selectEmpty
                         , selectWithContext
                         , getCss
                         , setCss
                         , getHeight
                         , setHeight
                         , getWidth
                         , setWidth
                         , getInnerHeight
                         , getOuterHeight
                         , getInnerWidth
                         , getOuterWidth
                         , getScrollLeft
                         , setScrollLeft
                         , getScrollTop
                         , setScrollTop
                         , click
                         , dblclick
                         , focusin
                         , focusout
                         , hover
                         , mousedown
                         , mouseenter
                         , mouseleave
                         , mousemove
                         , mouseup
                         , on
                         , one
                         , triggerHandler
                         , delegateTarget
                         , isDefaultPrevented
                         , isImmediatePropagationStopped
                         , isPropagationStopped
                         , namespace
                         , pageX
                         , pageY
                         , preventDefault
                         , stopPropagation
                         , stopImmediatePropagation
                         , target
                         , timeStamp
                         , eventType
                         , which
                         , blur
                         , change
                         , onFocus
                         , focus
                         , onSelect
                         , keydown
                         , keyup
                         , keypress
                         , after
                         , afterJQuery
                         , afterElem
                         , append
                         , appendJQuery
                         , appendElem
                         , appendTo
                         , appendToJQuery
                         , appendToElem
                         , before
                         , beforeJQuery
                         , beforeElem
                         , CloneType(..)
                         , clone
                         , detach
                         , detachSelector
                         , empty
                         , insertAfter
                         , insertAfterJQuery
                         , insertAfterElem
                         , insertBefore
                         , insertBeforeJQuery
                         , insertBeforeElem
                         , prepend
                         , prependJQuery
                         , prependElem
                         , prependTo
                         , prependToJQuery
                         , prependToElem
                         , remove
                         , removeSelector
                         , replaceAll
                         , replaceAllJQuery
                         , replaceAllElem
                         , replaceWith
                         , replaceWithJQuery
                         , replaceWithElem
                         , unwrap
                         , wrap
                         , wrapJQuery
                         , wrapElem
                         , wrapAll
                         , wrapAllJQuery
                         , wrapAllElem
                         , wrapInner
                         , wrapInnerJQuery
                         , wrapInnerElem
                         , addSelector
                         , addElement
                         , addHtml
                         , add
                         , andSelf
                         , children
                         , childrenMatching
                         , closestSelector
                         , closest
                         , closestElement
                         , contents
                         , end
                         , eq
                         , filter
                         , filterElement
                         , filterJQuery
                         , find
                         , findJQuery
                         , findElement
                         , first
                         , has
                         , hasElement
                         , is
                         , isJQuery
                         , isElement
                         , last
                         , next
                         , nextSelector
                         , nextAll
                         , nextAllSelector
                         , nextUntil
                         , nextUntilElement
                         , not
                         , notElement
                         , notJQuery
                         , offsetParent
                         , parent
                         , parentSelector
                         , parents
                         , parentsSelector
                         , parentsUntil
                         , parentsUntilElement
                         , prev
                         , prevSelector
                         , prevAll
                         , prevAllSelector
                         , prevUntil
                         , prevUntilElement
                         , siblings
                         , siblingsSelector
                         , slice
                         , sliceFromTo
                         , stop
                         ) where

import           Prelude hiding (filter, not, empty, last)

import           GHCJS.Marshal
import           GHCJS.Foreign (toJSBool, jsNull, jsFalse, jsTrue)
import           GHCJS.Types
import           GHCJS.DOM.Types ( ToJSString(..), FromJSString(..), toJSString, fromJSString
                                 , Element(..), IsElement(..), toElement, unElement)
import qualified GHCJS.Foreign as F
import qualified GHCJS.Foreign.Callback as F
import           GHCJS.Nullable
import qualified JavaScript.Object as F
import           JavaScript.JQuery.Internal

import           Control.Applicative hiding (empty)
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad

import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import           Data.Typeable
import           Data.Coerce

import           System.IO (fixIO)

type EventType = Text
type Selector  = Text

data Method = GET | POST | PUT | DELETE deriving (Eq, Ord, Enum, Show)

data AjaxSettings = AjaxSettings { asContentType :: Text
                                 , asCache       :: Bool
                                 , asIfModified  :: Bool
                                 , asMethod      :: Method
                                 } deriving (Ord, Eq, Show, Typeable)

data AjaxResult = AjaxResult { arStatus :: Int
                             , arData   :: Maybe Text
                             } deriving (Ord, Eq, Show, Typeable)

instance Default AjaxSettings where
  def = AjaxSettings "application/x-www-form-urlencoded; charset=UTF-8" True False GET

instance ToJSRef AjaxSettings where
  toJSRef (AjaxSettings ct cache ifMod method) = do
    o <- F.create
    let (.=) :: Text -> JSRef -> IO ()
        p .= v = F.setProp p v o
    "method"      .= toJSString method
    "ifModified"  .= toJSBool ifMod
    "cache"       .= toJSBool cache
    "contentType" .= toJSString ct
    "dataType"    .= ("text" :: JSString)
    return o

instance ToJSString Method
instance ToJSRef Method where
  toJSRef m = (toJSRef :: JSString -> JSRef) $ case m of
    GET    -> "GET"
    POST   -> "POST"
    PUT    -> "PUT"
    DELETE -> "DELETE"

ajax :: Text -> [(Text,Text)] -> AjaxSettings -> IO AjaxResult
ajax url d s = do
  o <- F.create
  forM_ d (\(k,v) -> F.setProp k (toJSString v) o)
  os <- toJSRef s
  F.setProp ("data"::Text) o os
  arr <- jq_ajax (toJSString url) os
  dat <- F.getProp ("data"::Text) arr
  let d = if isNull dat then Nothing else Just (fromJSString dat)
  status <- fromMaybe 0 <$> (fromJSRef =<< F.getProp ("status"::Text) arr)
  return (AjaxResult status d)

data HandlerSettings = HandlerSettings { hsPreventDefault           :: Bool
                                       , hsStopPropagation          :: Bool
                                       , hsStopImmediatePropagation :: Bool
                                       , hsSynchronous              :: Bool
                                       , hsDescendantFilter         :: Maybe Selector
                                       , hsHandlerData              :: Maybe JSRef
                                       }

convertHandlerSettings :: HandlerSettings -> (Bool, Bool, Bool, JSString, JSRef)
convertHandlerSettings (HandlerSettings pd sp sip _ ds hd) =
  (pd, sp, sip, maybe jsNull toJSString ds, fromMaybe jsNull hd)

instance Default HandlerSettings where
  def = HandlerSettings False False False True Nothing Nothing

addClass :: Text -> JQuery -> IO JQuery
addClass c = jq_addClass (toJSString c)

animate :: F.Object -> F.Object -> JQuery -> IO JQuery
animate = jq_animate

getAttr :: Text -> JQuery -> IO Text
getAttr a jq = fromJSString <$> jq_getAttr (toJSString a) jq

setAttr :: Text -> Text -> JQuery -> IO JQuery
setAttr a v = jq_setAttr (toJSString a) (toJSString v)

hasClass :: Text -> JQuery -> IO Bool
hasClass c jq = jq_hasClass (toJSString c) jq

getHtml :: JQuery -> IO Text
getHtml jq = fromJSString <$> jq_getHtml jq

setHtml :: Text -> JQuery -> IO JQuery
setHtml t = jq_setHtml (toJSString t)

getProp :: Text -> JQuery -> IO Text
getProp p jq = fromJSString <$> jq_getProp (toJSString p) jq

-- fixme value can be Boolean or Number
setProp :: Text -> Text -> JQuery -> IO JQuery
setProp p v = jq_setProp (toJSString p) (toJSString v)

removeAttr :: Text -> JQuery -> IO JQuery
removeAttr a = jq_removeAttr (toJSString a)

removeClass :: Text -> JQuery -> IO JQuery
removeClass c = jq_removeClass (toJSString c)

removeProp :: Text -> JQuery -> IO JQuery
removeProp p = jq_removeProp (toJSString p)

-- toggleClass :: Text -> JQuery -> IO JQuery
-- toggleClass c = jq_toggleClass (toJSString c)

getVal :: JQuery -> IO Text
getVal jq = fromJSString <$> jq_getVal jq

setVal :: Text -> JQuery -> IO JQuery
setVal v = jq_setVal (toJSString v)

getText :: JQuery -> IO Text
getText jq = fromJSString <$> jq_getText jq

setText :: Text -> JQuery -> IO JQuery
setText t = jq_setText (toJSString t)

holdReady :: Bool -> IO ()
holdReady b = jq_holdReady b

selectElement :: IsElement e => e -> IO JQuery
selectElement e = jq_selectElement (unElement (toElement e))

selectObject :: F.Object -> IO JQuery
selectObject a = jq_selectObject (coerce a)

select :: Text -> IO JQuery
select q = jq_select (toJSString q)

selectEmpty :: IO JQuery
selectEmpty = jq_selectEmpty

-- :: Text -> Either JQuery F.Object -> IO JQuery ?
selectWithContext :: Text -> F.Object -> IO JQuery
selectWithContext t o = jq_selectWithContext (toJSString t) (coerce o)

getCss :: Text -> JQuery -> IO Text
getCss t jq = fromJSString <$> jq_getCss (toJSString t) jq

setCss :: Text -> Text -> JQuery -> IO JQuery
setCss k v = jq_setCss (toJSString k) (toJSString v)

getHeight :: JQuery -> IO Double
getHeight = jq_getHeight

setHeight :: Double -> JQuery -> IO JQuery
setHeight = jq_setHeight

getWidth :: JQuery -> IO Double
getWidth = jq_getWidth

setWidth :: Double -> JQuery -> IO JQuery
setWidth = jq_setWidth

getInnerHeight :: JQuery -> IO Double
getInnerHeight = jq_getInnerHeight

getInnerWidth :: JQuery -> IO Double
getInnerWidth = jq_getInnerWidth

getOuterHeight :: Bool   -- ^ include margin?
               -> JQuery
               -> IO Double
getOuterHeight b = jq_getOuterHeight b

getOuterWidth :: Bool    -- ^ include margin?
              -> JQuery
              -> IO Double
getOuterWidth b = jq_getOuterWidth b

getScrollLeft :: JQuery -> IO Double
getScrollLeft = jq_getScrollLeft

setScrollLeft :: Double -> JQuery -> IO JQuery
setScrollLeft = jq_setScrollLeft

getScrollTop :: JQuery -> IO Double
getScrollTop = jq_getScrollTop

setScrollTop :: Double -> JQuery -> IO JQuery
setScrollTop = jq_setScrollTop

click :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
click a = on a "click"

dblclick :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
dblclick a = on a "dblclick"

focusin :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
focusin a = on a "focusin"

focusout :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
focusout a = on a "focusout"

hover :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
hover a = on a "hover"

mousedown :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mousedown a = on a "mousedown"

mouseenter :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseenter a = on a "mouseenter"

mouseleave :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseleave a = on a "mouseleave"

mousemove :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mousemove a = on a "mousemove"

mouseout :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseout a = on a "mouseout"

mouseover :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseover a = on a "mouseover"

mouseup :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseup a = on a "mouseup"

{- |  Register an event handler. Use the returned IO action to remove the
      handler.

      Note that the handler will stay in memory until the returned IO action is
      executed, even if the DOM nodes are removed.
 -}
on :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
on a et hs jq = do
  cb <- if hsSynchronous hs
          then F.syncCallback1 F.ContinueAsync a
          else F.asyncCallback1 a
  jq_on cb et' ds hd sp sip pd jq
  return (jq_off cb et' ds jq >> F.releaseCallback cb)
    where
      et'                   = toJSString et
      (pd, sp, sip, ds, hd) = convertHandlerSettings hs

one :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
one a et hs jq = do
  cb <- fixIO $ \cb ->
      let a' = \e -> F.releaseCallback cb >> a e
      in if hsSynchronous hs
            then F.syncCallback1 F.ContinueAsync a
            else F.asyncCallback1 a
  jq_one cb et' ds hd sp sip pd jq
  return (jq_off cb et' ds jq >> F.releaseCallback cb)
    where
      et'                   = toJSString et
      (pd, sp, sip, ds, hd) = convertHandlerSettings hs

trigger :: EventType -> JQuery -> IO ()
trigger et jq = jq_trigger (toJSString et) jq

triggerHandler :: EventType -> JQuery -> IO ()
triggerHandler et jq = jq_triggerHandler (toJSString et) jq

delegateTarget :: Event -> IO Element
delegateTarget ev = Element <$> jq_delegateTarget ev

isDefaultPrevented :: Event -> IO Bool
isDefaultPrevented e = jq_isDefaultPrevented e

isImmediatePropagationStopped :: Event -> IO Bool
isImmediatePropagationStopped e = jq_isImmediatePropagationStopped e

isPropagationStopped :: Event -> IO Bool
isPropagationStopped e = jq_isPropagationStopped e

namespace :: Event -> IO Text
namespace e = fromJSString <$> jq_namespace e

pageX :: Event -> IO Double
pageX = jq_pageX

pageY :: Event -> IO Double
pageY = jq_pageY

preventDefault :: Event -> IO ()
preventDefault = jq_preventDefault

stopPropagation :: Event -> IO ()
stopPropagation = jq_stopPropagation

stopImmediatePropagation :: Event -> IO ()
stopImmediatePropagation = jq_stopImmediatePropagation

target :: Event -> IO Element
target ev = Element <$> jq_target ev

timeStamp :: Event -> IO Double
timeStamp = jq_timeStamp

eventType :: Event -> IO Text
eventType e = fromJSString <$> jq_eventType e

which :: Event -> IO Int
which = jq_eventWhich

blur :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
blur a = on a "blur"

change :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
change a = on a "change"

onFocus :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
onFocus a = on a "focus"

focus :: JQuery -> IO JQuery
focus = jq_focus

onSelect :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
onSelect a = on a "select"

submit :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
submit a = on a "submit"

keydown :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
keydown a = on a "keydown"

keyup :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
keyup a = on a "keyup"

keypress :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
keypress a = on a "keypress"

after :: Text -> JQuery -> IO JQuery
after h jq = jq_after (coerce $ toJSString h) jq

afterJQuery :: JQuery -> JQuery -> IO JQuery
afterJQuery j jq = jq_after (coerce j) jq

afterElem :: IsElement e => e -> JQuery -> IO JQuery
afterElem e jq = jq_after (coerce . unElement $ toElement e) jq

append :: Text -> JQuery -> IO JQuery
append h jq = jq_append (coerce $ toJSString h) jq

appendJQuery :: JQuery -> JQuery -> IO JQuery
appendJQuery j jq = jq_append (coerce j) jq

appendElem :: IsElement e => e -> JQuery -> IO JQuery
appendElem e jq = jq_append (coerce . unElement $ toElement e) jq

appendTo :: Text -> JQuery -> IO JQuery
appendTo h jq = jq_appendTo (coerce $ toJSString h) jq

appendToJQuery :: JQuery -> JQuery -> IO JQuery
appendToJQuery j jq = jq_appendTo (coerce j) jq

appendToElem :: IsElement e => e -> JQuery -> IO JQuery
appendToElem e jq = jq_appendTo (coerce . unElement $ toElement e) jq

before :: Text -> JQuery -> IO JQuery
before h jq = jq_before (coerce $ toJSString h) jq

beforeJQuery :: JQuery -> JQuery -> IO JQuery
beforeJQuery j jq = jq_before (coerce j) jq

beforeElem :: IsElement e => e -> JQuery -> IO JQuery
beforeElem e jq = jq_before (coerce . unElement $ toElement e) jq

data CloneType = WithoutDataAndEvents
               | WithDataAndEvents
               | DeepWithDataAndEvents

clone :: CloneType -> JQuery -> IO JQuery
clone WithoutDataAndEvents  = jq_clone False False
clone WithDataAndEvents     = jq_clone True  False
clone DeepWithDataAndEvents = jq_clone True  True

detach :: JQuery -> IO JQuery
detach = jq_detach jsNull

detachSelector :: Selector -> JQuery -> IO JQuery
detachSelector s = jq_detach (toJSString s)

empty :: JQuery -> IO JQuery
empty = jq_empty

insertAfter :: Text -> JQuery -> IO JQuery
insertAfter h jq = jq_insertAfter (coerce $ toJSString h) jq

insertAfterJQuery :: JQuery -> JQuery -> IO JQuery
insertAfterJQuery j jq = jq_insertAfter (coerce j) jq

insertAfterElem :: IsElement e => e -> JQuery -> IO JQuery
insertAfterElem e jq = jq_insertAfter (coerce . unElement $ toElement e) jq

insertBefore :: Text -> JQuery -> IO JQuery
insertBefore h jq = jq_insertBefore (coerce $ toJSString h) jq

insertBeforeJQuery :: JQuery -> JQuery -> IO JQuery
insertBeforeJQuery j jq = jq_insertBefore (coerce j) jq

insertBeforeElem :: IsElement e => e -> JQuery -> IO JQuery
insertBeforeElem e jq = jq_insertBefore (coerce . unElement $ toElement e) jq

prepend :: Text -> JQuery -> IO JQuery
prepend h jq = jq_prepend (coerce $ toJSString h) jq

prependJQuery :: JQuery -> JQuery -> IO JQuery
prependJQuery j jq = jq_prepend (coerce j) jq

prependElem :: IsElement e => e -> JQuery -> IO JQuery
prependElem e jq = jq_prepend (coerce . unElement $ toElement e) jq

prependTo :: Text -> JQuery -> IO JQuery
prependTo h jq = jq_prependTo (coerce $ toJSString h) jq

prependToJQuery :: JQuery -> JQuery -> IO JQuery
prependToJQuery j jq = jq_prependTo (coerce j) jq

prependToElem :: IsElement e => e -> JQuery -> IO JQuery
prependToElem e jq = jq_prependTo (coerce . unElement $ toElement e) jq

remove :: JQuery -> IO JQuery
remove = jq_remove jsNull

removeSelector :: Selector -> JQuery -> IO JQuery
removeSelector s = jq_remove (toJSString s)

replaceAll :: Text -> JQuery -> IO JQuery
replaceAll h jq = jq_replaceAll (coerce $ toJSString h) jq

replaceAllJQuery :: JQuery -> JQuery -> IO JQuery
replaceAllJQuery j jq = jq_replaceAll (coerce j) jq

replaceAllElem :: IsElement e => e -> JQuery -> IO JQuery
replaceAllElem e jq = jq_replaceAll (coerce . unElement $ toElement e) jq

replaceWith :: Text -> JQuery -> IO JQuery
replaceWith h jq = jq_replaceWith (coerce $ toJSString h) jq

replaceWithJQuery :: JQuery -> JQuery -> IO JQuery
replaceWithJQuery j jq = jq_replaceWith (coerce j) jq

replaceWithElem :: IsElement e => e -> JQuery -> IO JQuery
replaceWithElem e jq = jq_replaceWith (coerce . unElement $ toElement e) jq

unwrap :: JQuery -> IO JQuery
unwrap = jq_unwrap

wrap :: Text -> JQuery -> IO JQuery
wrap h jq = jq_wrap (coerce $ toJSString h) jq

wrapJQuery :: JQuery -> JQuery -> IO JQuery
wrapJQuery j jq = jq_wrap (coerce j) jq

wrapElem :: IsElement e => e -> JQuery -> IO JQuery
wrapElem e jq = jq_wrap (coerce . unElement $ toElement e) jq

wrapAll :: Text -> JQuery -> IO JQuery
wrapAll h jq = jq_wrapAll (coerce $ toJSString h) jq

wrapAllJQuery :: JQuery -> JQuery -> IO JQuery
wrapAllJQuery j jq = jq_wrapAll (coerce j) jq

wrapAllElem :: IsElement e => e -> JQuery -> IO JQuery
wrapAllElem e jq = jq_wrapAll (coerce . unElement $ toElement e) jq

wrapInner :: Text -> JQuery -> IO JQuery
wrapInner h jq = jq_wrapInner (coerce $ toJSString h) jq

wrapInnerJQuery :: JQuery -> JQuery -> IO JQuery
wrapInnerJQuery j jq = jq_wrapInner (coerce j) jq

wrapInnerElem :: IsElement e => e -> JQuery -> IO JQuery
wrapInnerElem e jq = jq_wrapInner (coerce . unElement $ toElement e) jq

addSelector :: Selector -> JQuery -> IO JQuery
addSelector s jq = jq_add (coerce $ toJSString s) jq

addElement :: IsElement e => e -> JQuery -> IO JQuery
addElement e jq = jq_add (coerce . unElement $ toElement e) jq

addHtml :: Text -> JQuery -> IO JQuery
addHtml h jq = jq_add (coerce $ toJSString h) jq

add :: JQuery -> JQuery -> IO JQuery
add j jq = jq_add (coerce j) jq

-- addSelectorWithContext :: Selector -> JQuery -> JQuery -> IO JQuery
-- addSelectorWithContext = undefined

andSelf :: JQuery -> IO JQuery
andSelf = jq_andSelf

children :: JQuery -> IO JQuery
children = jq_children jsNull

childrenMatching :: Selector -> JQuery -> IO JQuery
childrenMatching s = jq_children (toJSString s)

closestSelector :: Selector -> JQuery -> IO JQuery
closestSelector s jq = jq_closest (coerce $ toJSString s) jq

-- closestWithContext :: Selector -> Selector -> JQuery -> IO JQuery
-- closestWithContext = undefined

closest :: JQuery -> JQuery -> IO JQuery
closest j jq = jq_closest (coerce j) jq

closestElement :: IsElement e => e -> JQuery -> IO JQuery
closestElement e jq = jq_closest (coerce . unElement $ toElement e) jq

contents :: JQuery -> IO JQuery
contents = jq_contents

-- This just isn't cool[' Can']'t we all just use map?
-- each :: (Double -> Element -> Fay Bool) -> JQuery -> Fay JQuery
-- each = ffi "%2['each'](%1)"

end :: JQuery -> IO JQuery
end = jq_end

eq :: Int -> JQuery -> IO JQuery
eq = jq_eq

filter :: Selector -> JQuery -> IO JQuery
filter s = jq_filter (coerce $ toJSString s)

filterElement :: IsElement e => e -> JQuery -> IO JQuery
filterElement e = jq_filter (coerce . unElement $ toElement e)

filterJQuery :: JQuery -> JQuery -> IO JQuery
filterJQuery j = jq_filter (coerce j)

find :: Selector -> JQuery -> IO JQuery
find s = jq_find (coerce $ toJSString s)

findJQuery :: JQuery -> JQuery -> IO JQuery
findJQuery j = jq_find (coerce j)

findElement :: IsElement e => e -> JQuery -> IO JQuery
findElement e = jq_find (coerce . unElement $ toElement e)

first :: JQuery -> IO JQuery
first = jq_first

has :: Selector -> JQuery -> IO JQuery
has s = jq_has (coerce $ toJSString s)

hasElement :: IsElement e => e -> JQuery -> IO JQuery
hasElement e = jq_has (coerce . unElement $ toElement e)

is :: Selector -> JQuery -> IO Bool
is s = jq_is (coerce $ toJSString s)

isJQuery :: JQuery -> JQuery -> IO Bool
isJQuery j = jq_is (coerce j)

isElement :: IsElement e => e -> JQuery -> IO Bool
isElement e = jq_is (coerce . unElement $ toElement e)

last :: JQuery -> IO JQuery
last = jq_last

next :: JQuery -> IO JQuery
next = jq_next jsNull

nextSelector :: Selector -> JQuery -> IO JQuery
nextSelector s = jq_next (toJSString s)

nextAll :: JQuery -> IO JQuery
nextAll = jq_nextAll jsNull

nextAllSelector :: Selector -> JQuery -> IO JQuery
nextAllSelector s = jq_nextAll (toJSString s)

nextUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
nextUntil s mf = jq_nextUntil (coerce $ toJSString s) (maybe jsNull toJSString mf)

nextUntilElement :: IsElement e => e -> Maybe Selector -> JQuery -> IO JQuery
nextUntilElement e mf = jq_nextUntil (coerce . unElement $ toElement e) (maybe jsNull toJSString mf)

not :: Selector -> JQuery -> IO JQuery
not s = jq_not (coerce $ toJSString s)

notElement :: IsElement e => e -> JQuery -> IO JQuery
notElement e = jq_not (coerce . unElement $ toElement e)

-- notElements :: [Element] -> JQuery -> IO JQuery
-- notElements = jq_notElements

notJQuery :: JQuery -> JQuery -> IO JQuery
notJQuery j = jq_not (coerce j)

offsetParent :: JQuery -> IO JQuery
offsetParent = jq_offsetParent

parent :: JQuery -> IO JQuery
parent = jq_parent jsNull

parentSelector :: String -> JQuery -> IO JQuery
parentSelector s = jq_parent (toJSString s)

parents :: JQuery -> IO JQuery
parents = jq_parents jsNull

parentsSelector :: Selector -> JQuery -> IO JQuery
parentsSelector s = jq_parents (toJSString s)

parentsUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
parentsUntil s mf = jq_parentsUntil (coerce $ toJSString s) (maybe jsNull (coerce . toJSString) mf)

parentsUntilElement :: IsElement e => e -> Maybe Selector -> JQuery -> IO JQuery
parentsUntilElement e mf = jq_parentsUntil (coerce . unElement $ toElement e) (maybe jsNull (coerce . toJSString) mf)

prev :: JQuery -> IO JQuery
prev = jq_prev jsNull

prevSelector :: Selector -> JQuery -> IO JQuery
prevSelector s = jq_prev (toJSString s)

prevAll :: JQuery -> IO JQuery
prevAll = jq_prevAll jsNull

prevAllSelector :: String -> JQuery -> IO JQuery
prevAllSelector s = jq_prevAll (toJSString s)

prevUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
prevUntil s mf = jq_prevUntil (coerce $ toJSString s) (maybe jsNull toJSString mf)

prevUntilElement :: IsElement e => e -> Maybe Selector -> JQuery -> IO JQuery
prevUntilElement e mf = jq_prevUntil (coerce . unElement $ toElement e) (maybe jsNull toJSString mf)

siblings :: JQuery -> IO JQuery
siblings = jq_siblings (maybeToNullable Nothing)

siblingsSelector :: Selector -> JQuery -> IO JQuery
siblingsSelector s = jq_siblings (maybeToNullable (Just (toJSString s)))

slice :: Int -> JQuery -> IO JQuery
slice = jq_slice

sliceFromTo :: Int -> Int -> JQuery -> IO JQuery
sliceFromTo = jq_sliceFromTo

stop :: Bool -> JQuery -> IO JQuery
stop = jq_stop
