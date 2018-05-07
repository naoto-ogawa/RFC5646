{-# LANGUAGE
    OverloadedStrings
#-}
module RFC.SpecRFC5646 where

import Test.Tasty
import Test.Tasty.Hspec
--
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Internal.Types as T
--
import RFC.RFC5646

--

-- main :: IO ()
-- main = hspec spec_sample

getContext = return "dummy" 

testRange = [
    ("2-3", (range 2 3 (char '*')), "**", "**")
  , ("2-3", (range 2 3 (char '*')), "**A", "**")
  , ("2-3", (range 2 3 (char '*')), "****", "***")
  , ("2-3", (range 2 3 (char '*')), "***A", "***")
  , ("2-3", (range 2 3 (char '*')), "*****", "***")
  , ("2-3", (range 2 3 (char '*')), "****A", "***")
  , ("2-3", (range 2 3 (char '*')), "*", "error")
  , ("2-3", (range 2 3 (char '*')), "A", "error")
  ]

spec_range = do
  before getContext $ do
    describe "test description" $ makeIts testRange testStr

testPrivateuse_ = [
    ("_pPrivateuse error", _pPrivateuse, "-", "error")
  , ("_pPrivateuse 1", _pPrivateuse, "-1", "1")
  , ("_pPrivateuse 2", _pPrivateuse, "-12",  "12")
  , ("_pPrivateuse 3", _pPrivateuse, "-123", "123")
  , ("_pPrivateuse 4", _pPrivateuse, "-1234", "1234")
  , ("_pPrivateuse 5", _pPrivateuse, "-12345", "12345")
  , ("_pPrivateuse 6", _pPrivateuse, "-123456", "123456")
  , ("_pPrivateuse 7", _pPrivateuse, "-1234567", "1234567")
  , ("_pPrivateuse 8-1", _pPrivateuse, "-12345678", "12345678")
  , ("_pPrivateuse 8-2", _pPrivateuse, "-123456789", "12345678")
  ]

spec_privateuse_ = do
  before getContext $ do
    describe "test description" $ makeIts testPrivateuse_ testStr

testPrivateuse = [
    ("pPrivateuse error", pPrivateuse, "X-", ["error"])
  , ("pPrivateuse 1", pPrivateuse, "x-1", ["1"])
  , ("pPrivateuse 2", pPrivateuse, "x-12",  ["12"])
  , ("pPrivateuse 3", pPrivateuse, "x-123", ["123"])
  , ("pPrivateuse 4", pPrivateuse, "x-1234", ["1234"])
  , ("pPrivateuse 5", pPrivateuse, "x-12345", ["12345"])
  , ("pPrivateuse 6", pPrivateuse, "x-123456", ["123456"])
  , ("pPrivateuse 7", pPrivateuse, "x-1234567", ["1234567"])
  , ("pPrivateuse 8-1", pPrivateuse, "x-12345678", ["12345678"])
  , ("pPrivateuse 8-2", pPrivateuse, "x-123456789", ["12345678"])
  , ("pPrivateuse TODO ", pPrivateuse, "x-123-", ["123"])  -- TODO correct ?
  , ("pPrivateuse 2 parts", pPrivateuse, "x-123-abcd", ["123","abcd"])
  , ("pPrivateuse 3 parts", pPrivateuse, "x-1-ab-1ab", ["1","ab", "1ab"])
  ]

spec_privateuse = do
  before getContext $ do
    describe "test description" $ makeIts testPrivateuse testStrList

testSingleton = [
    ("pSingleton error 1",  pSingleton, "-", '@')
  , ("pSingleton error 2",  pSingleton, "x", '@')
  , ("pSingleton 1", pSingleton, "a", 'a')
  , ("pSingleton 2", pSingleton, "1", '1')
  ]

spec_singleton = do
  before getContext $ do
    describe "singleton" $ makeIts testSingleton testChar

testExtension = [
    ("pExtension error 1",  pExtension, "x-aa", "error" )
  , ("pExtension error 2",  pExtension, "x-", "error" )
  , ("pExtension error 3",  pExtension, "a-a", "error")
  , ("pExtension 1", pExtension, "b-123-456", "123")
  , ("pExtension 2", pExtension, "b-123-456-xyz", "123")
  ]

spec_extension = do
  before getContext $ do
    describe "extension" $ makeIts testExtension testStr

pExtension2 = option [] (many' (char '-' >> pExtension))
testExtension2 = [
    ("pExtension2 1", pExtension2, "-b-123-c-456", ["123", "456"])
  , ("pExtension2 2", pExtension2, "-b-123-c-456-d-xyz", ["123", "456", "xyz"])
  ]

spec_extension2 = do
  before getContext $ do
    describe "extension" $ makeIts testExtension2 testStrList

testVariant = [
    ("pVariant error 1",  pVariant, "", "error" )
  , ("pVariant",  pVariant, "aaaaa", "aaaaa" )
  , ("pVariant",  pVariant, "bbbbbbbb", "bbbbbbbb" )
  , ("pVariant error 2",  pVariant, "ccccccccc", "error" )
  , ("pVariant",  pVariant, "rozaj", "rozaj" )
  ]

spec_variant = do
  before getContext $ do
    describe "extension" $ makeIts testVariant testStr

testLangtag = [
    ("language",  pLangtag, "xx"
    ,Langtag (ISO639 "xx" []) Nothing Nothing [] [] [])
  , ("language ext 1",  pLangtag, "xx-yyy"
    ,Langtag (ISO639 "xx" ["yyy"]) Nothing Nothing [] [] [])
  , ("language ext 2",  pLangtag, "xx-yyy-zzz"
    ,Langtag (ISO639 "xx" ["yyy", "zzz"]) Nothing Nothing [] [] [])
  , ("script",  pLangtag, "xx-yyyy"
    ,Langtag (ISO639 "xx" []) (Just "yyyy") Nothing [] [] [])
  , ("region chars",  pLangtag, "xx-yy"
    ,Langtag (ISO639 "xx" []) Nothing (Just "yy") [] [] [])
  , ("region number",  pLangtag, "xx-123"
    ,Langtag (ISO639 "xx" []) Nothing (Just "123") [] [] [])
  -- Appendix A.  Examples of Language Tags (Informative)
  , ("Chinese written using the Traditional Chinese script",  
     pLangtag, "zh-Hant"
    ,Langtag (ISO639 "zh" []) (Just "Hant") Nothing [] [] [])
  , ("Chinese written using the Simplified Chinese script",  
     pLangtag, "zh-Hans"
    ,Langtag (ISO639 "zh" []) (Just "Hans") Nothing [] [] [])
  , ("Serbian written using the Cyrillic script",  
     pLangtag, "sr-Cyrl"
    ,Langtag (ISO639 "sr" []) (Just "Cyrl") Nothing [] [] [])
  , ("Serbian written using the Latin script",  
     pLangtag, "sr-Latn"
    ,Langtag (ISO639 "sr" []) (Just "Latn") Nothing [] [] [])
  , ("Chinese, Mandarin, Simplified script, as used in China",  
     pLangtag, "zh-cmn-Hans-CN"
    ,Langtag (ISO639 "zh" ["cmn"]) (Just "Hans") (Just "CN") [] [] [])
 , ("Mandarin Chinese, Simplified script, as used in China",  
    pLangtag, "cmn-Hans-CN"
   ,Langtag (ISO639 "cmn" []) (Just "Hans") (Just "CN") [] []  [])
  , ("Chinese, Cantonese, as used in Hong Kong SAR",  
     pLangtag, "zh-yue-HK"
    ,Langtag (ISO639 "zh" ["yue"]) Nothing (Just "HK") [] [] [])
  , ("Cantonese Chinese, as used in Hong Kong SAR",  
     pLangtag, "yue-HK"
    ,Langtag (ISO639 "yue" []) Nothing (Just "HK") [] [] [])
  -- Language-Script-Region:
  , ("Chinese written using the Simplified script as used in mainland China",  
     pLangtag, "zh-Hans-CN"
    ,Langtag (ISO639 "zh" []) (Just "Hans") (Just "CN") [] [] [])
  , ("Serbian written using the Latin script as used in Serbia",  
     pLangtag, "sr-Latn-RS"
    ,Langtag (ISO639 "sr" []) (Just "Latn") (Just "RS") [] [] [])
  -- Language-Variant:
  , ("Resian dialect of Slovenian",  
     pLangtag, "sl-rozaj"
    ,Langtag (ISO639 "sl" []) Nothing  Nothing ["rozaj"] [] [])
  , ("San Giorgio dialect of Resian dialect of Slovenian",  
     pLangtag, "sl-rozaj-biske"
    ,Langtag (ISO639 "sl" []) Nothing Nothing ["rozaj","biske"] [] [])
  , ("Nadiza dialect of Slovenian",  
     pLangtag, "sl-nedis"
    ,Langtag (ISO639 "sl" []) Nothing Nothing ["nedis"] [] [])
  -- Language-Region-Variant:
  , ("German as used in Switzerland using the 1901 variant [orthography]",  
     pLangtag, "de-CH-1901"
    ,Langtag (ISO639 "de" []) Nothing (Just "CH") ["1901"] [] [])
  , ("Slovenian as used in Italy, Nadiza dialect",  
     pLangtag, "sl-IT-nedis"
    ,Langtag (ISO639 "sl" []) Nothing (Just "IT") ["nedis"] [] [])
  -- Language-Script-Region-Variant:
  , ("Eastern Armenian written in Latin script, as used in Italy",  
     pLangtag, "hy-Latn-IT-arevela"
    ,Langtag (ISO639 "hy" []) (Just "Latn") (Just "IT") ["arevela"] [] [])
  -- Language-Region:
  , ("German for Germany",  
     pLangtag, "de-DE"
    ,Langtag (ISO639 "de" []) Nothing (Just "DE") [] [] [])
  , ("English as used in the United States",  
     pLangtag, "en-US"
    ,Langtag (ISO639 "en" []) Nothing (Just "US") [] [] [])
  , ("Spanish appropriate for the Latin America and Caribbean region using the UN region code",  
     pLangtag, "es-419"
    ,Langtag (ISO639 "es" []) Nothing (Just "419") [] [] [])
  -- Private use subtags:
  , ("Private use subtags 1",  
     pLangtag, "de-CH-x-phonebk"
    ,Langtag (ISO639 "de" []) Nothing (Just "CH") [] [] ["phonebk"])
  , ("Private use subtags 2",
     pLangtag, "az-Arab-x-AZE-derbend"
    ,Langtag (ISO639 "az" []) (Just "Arab") Nothing [] [] ["AZE", "derbend"])
--  -- Private use registry values:
--  , ("private use using the singleton 'x'",  
--     pLangtag, "x-whatever"
--    ,Langtag (ISO639 "" []) Nothing Nothing [] [] ["whatever"])
  -- Tags that use extensions (examples ONLY -- extensions MUST be defined by revision or update to this document, or by RFC):
  , ("extention 1",  
     pLangtag, "en-US-u-islamcal"
    ,Langtag (ISO639 "en" []) Nothing (Just "US") [] ["islamcal"] [])
  , ("extention 2",  
     pLangtag, "zh-CN-a-myext-x-private"
    ,Langtag (ISO639 "zh" []) Nothing (Just "CN") [] ["myext"] ["private"])
  , ("extention 3",  
     pLangtag, "en-a-myext-b-another"
    ,Langtag (ISO639 "en" []) Nothing Nothing  [] ["myext","another"] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
  ]

spec_langtag = do
  before getContext $ do
    describe "langtag" $ makeIts testLangtag testWithError



makeIts dada check = foldl1 (>>) $ fmap it_ dada 
  where
    it_ (casedesc, myparser, input , expected)
      = it casedesc $ \ctc -> check myparser input
                         `shouldBe`
                       expected

testStrList p inp = either (\x -> ["error"]) id (parseOnly p inp)
testStr p inp = either (\x -> "error") id (parseOnly p inp)
testChar p inp = either (\x -> '@') id (parseOnly p inp)
testWithError p inp = either (\x -> error "error!!") id (parseOnly p inp)


