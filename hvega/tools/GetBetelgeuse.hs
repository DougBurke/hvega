{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Grab Betelgeuse data from the AAVSO site. This is based on
code in https://github.com/hippke/betelbot which is
used to generate the tweets from https://twitter.com/betelbot

The idea is to download a bunch of Vis-band observations
and grab the data from the HTML, convert to a very-simple
JSON format and write to the screen. There is limited
error checking.

-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as NHC

import Data.Aeson (ToJSON(..), encode, defaultOptions, genericToEncoding)

import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text.Encoding (decodeUtf8)

import GHC.Generics

import Text.HTML.TagSoup (Tag(..)
                         , isTagOpenName
                         , parseTags
                         , partitions)
import Text.Read (readMaybe)

import Numeric.Natural (Natural)


-- | The given page of results from AAVSO for Betelgeuse
--
--   When ran there were ~3900 results, so at 200 per page that's
--   20 pages
--
pageURL :: Natural -> String
pageURL p = "https://www.aavso.org/apps/webobs/results/?start=2017-06-01&num_results=200&obs_types=vis+ccd&star=betelgeuse&page=" <> show p

nPages :: Natural
nPages = 20


-- | Return the given page.
--
getPage :: Natural -> IO T.Text
getPage p = decodeUtf8 . L.toStrict <$> NHC.simpleHttp (pageURL p)


data Row = Row { jd :: Double
               , date :: T.Text
               , magnitude :: Double
               , magErr :: Maybe Double
               , filterName :: T.Text
               , observer :: T.Text
               } deriving (Generic, Show)

instance ToJSON Row where
    toEncoding = genericToEncoding defaultOptions

-- Extract rows
--
-- The data structure is regular, but complicated. I am going
-- to ignore the "extra" data for each observation.
--
extractRows :: [Tag T.Text] -> [Row]
extractRows tgs =
  let rows = partitions (isTagOpenName "tr") tgs
  in mapMaybe extractRow rows 


isObs :: [(T.Text, T.Text)] -> Bool
isObs xs =
  let hasObs cls = "obs" `elem` T.splitOn " " cls
      act = hasObs <$> lookup "class" xs
  in fromMaybe False act

  
-- Is this a row we care about? The choice is made by
-- only looking for rows with a class name of 'obs'.
--
extractRow :: [Tag T.Text] -> Maybe Row
extractRow (TagOpen "tr" attrs : ts) | isObs attrs = toRow ts
extractRow _ = Nothing
  

-- extract the text contents of the td element; highly specialised
-- to this use case
getText :: [Tag T.Text] -> T.Text
getText (TagText txt:_) = txt
getText (_:ts) = getText ts
getText [] = error "unexpected data"


-- probably not enough data checks
toRow :: [Tag T.Text] -> Maybe Row
toRow tgs =
  let tds = partitions (isTagOpenName "td") tgs
      jdTag = getText (tds !! 2)
      dateTag = getText (tds !! 3)
      magTag = getText (tds !! 4)
      magETag = getText (tds !! 5)
      filterTag = getText (tds !! 6)
      obsTag = getText (tds !! 7)

      magE = readMaybe (T.unpack magETag)

  in if length tds /= 9
     then Nothing
     else Row <$> readMaybe (T.unpack jdTag)
              <*> pure dateTag
              <*> readMaybe (T.unpack magTag)
              <*> pure magE
              <*> pure filterTag
              <*> pure obsTag


processPage :: T.Text -> [Row]
processPage = extractRows . parseTags


main :: IO ()
main = do
  rs <- mapM (fmap processPage . getPage) [1..nPages]

  -- assuming no chance of getting repeated rows from this query
  let rows = concat rs
      srows = sortBy (compare `on` jd) rows

      js = encode srows

  L8.putStrLn js
  
