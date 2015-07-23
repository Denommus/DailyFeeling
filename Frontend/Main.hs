import Control.Monad.IO.Class
import Data.Time.Clock
import Reflex.Dom
import DailyFeeling.Common.Types
import Control.Monad

feelingInput :: MonadWidget t m => m ()
feelingInput = do
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
