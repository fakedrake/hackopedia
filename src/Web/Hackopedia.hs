{-# LANGUAGE OverloadedStrings #-}

module Web.Hackopedia (urlBody) where
import Data.Either
import Text.Regex.TDFA
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Text.XML.HaXml.SAX as XML
import qualified Text.XML.HaXml.Types as XT
import Control.Lens
import Network.Wreq

type Url = String;
instance Show XML.SaxElement where
  show (XML.SaxElementOpen n atts) = "<" ++ n ++ showAttrs atts ++  ">"
  show (XML.SaxElementTag n atts) = "<" ++ n ++ " " ++ showAttrs atts ++  "/>"
  show (XML.SaxElementClose n) = "</" ++ n ++ ">"
  show (XML.SaxCharData str) = str
  show _ = ""

showAttrs [] = ""
showAttrs ((XT.N name, vals):as) = " " ++ name ++ "=\"" ++ show vals ++ "\""
  ++ showAttrs as
showAttrs (a:as) = " " ++ show a ++ showAttrs as

-- |Get the body of a url.
urlBody :: Url -> IO BCL.ByteString
urlBody index = do
  r <- get index
  return $  r ^. responseBody

dumpsUrl :: Url -> IO (Either String [Url])
dumpsUrl url = do
  dumps <- dumpsHtml <$> urlBody url
  let unrel = (url ++)
  -- XXX: we just assume they are relative
  either (return . Left) (return . Right . (map unrel)) dumps

dumpsHtml :: BL.ByteString -> Either String [Url]
dumpsHtml html =
  let (sax, err) = XML.saxParse "" $ BCL.unpack html
      r = "pages-articles[0-9].*\\.bz2$" :: BCL.ByteString
  in maybe (Right $ filter (=~ r) $ getLinks sax) Left err

getLinks :: [XML.SaxElement] -> [Url]
getLinks [] = []
getLinks ((XML.SaxElementOpen "a" attrs):as) = getHrefs attrs ++ getLinks as where
  getHrefs :: [XT.Attribute] -> [String]
  getHrefs [] = []
  getHrefs ((XT.N "href", (XT.AttValue hrAttrs)):xs) =
    lefts hrAttrs ++ getHrefs xs
  getHrefs (_:xs) = getHrefs xs
getLinks (_:xs) = getLinks xs
