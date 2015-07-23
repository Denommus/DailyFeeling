import Control.Arrow
import Control.Monad.IO.Class
import Data.Time.Clock
import Reflex.Dom
import DailyFeeling.Common.Types
import Control.Monad
import qualified Data.Map as M
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BS

moodMap :: M.Map Mood String
moodMap = foldr (uncurry M.insert) M.empty $ (id &&& show) <$> [Happy, Normal, Sad]

headers :: M.Map String String
headers = M.insert "Content-type" "application/json" $
          M.singleton "Accepts" "application/json"

buildFeelingRequest :: Entry -> XhrRequest
buildFeelingRequest entry = XhrRequest
                              {
                                _xhrRequest_method = "POST"
                              , _xhrRequest_url = "/entries"
                              , _xhrRequest_config = config
                              }
  where config = XhrRequestConfig
                 {
                   _xhrRequestConfig_headers = headers
                 , _xhrRequestConfig_user = _xhrRequestConfig_user def
                 , _xhrRequestConfig_password = _xhrRequestConfig_password def
                 , _xhrRequestConfig_responseType = _xhrRequestConfig_responseType
                                                    def
                 , _xhrRequestConfig_sendData = Just . BS.toString . encode $ entry
                 }

feelingInput :: MonadWidget t m => m ()
feelingInput = do
  nameIn <- el "div" $ text "Name (optional): " >> textInput def
  moodIn <- el "div" $ text "Mood: " >> dropdown Happy (constDyn moodMap) def
  descIn <- el "div" $ text "Reason: " >> textArea def
  buttonEvent <- button "Save"
  let n = _textInput_value nameIn
  let m = _dropdown_value moodIn
  let d = _textArea_value descIn
  partialEntry <- combineDyn Entry m n
  entry <- combineDyn ($) partialEntry d
  let entryE = tag (current entry) buttonEvent
  performRequestAsync $ buildFeelingRequest <$> entryE
  return ()

feelingTable :: MonadWidget t m => [Entry] -> m ()
feelingTable feelings = do
  el "table" $ do
    el "tr" $ do
      el "th" $ text "Name"
      el "th" $ text "Mood"
      el "th" $ text "Reason"

    forM_ feelings $ \feeling -> do
      el "tr" $ do
        el "td" $ text $ name feeling
        el "td" $ text $ show . mood $ feeling
        el "td" $ text $ description feeling

getFeelings :: (MonadWidget t m) => m (Event t [Entry])
getFeelings = do
  currTime <- liftIO getCurrentTime
  t <- tickLossy 1 currTime
  entries <- getAndDecode $ const "/entries" <$> t
  return $ maybe [] id <$> entries

main :: IO ()
main = do
  mainWidget $ do
    feelingInput
    entriesEvent <- getFeelings
    entries <- holdDyn [] entriesEvent
    dyn =<< mapDyn feelingTable entries
    return ()
