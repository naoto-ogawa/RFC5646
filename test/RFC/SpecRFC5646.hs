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
import RFC.Internal.RFC5646

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
      ("pPrivateuse error"   , pPrivateuse , "X-"          , ["error"])
    , ("pPrivateuse 1"       , pPrivateuse , "x-1"         , ["x","1"])
    , ("pPrivateuse 2"       , pPrivateuse , "x-12"        , ["x","12"])
    , ("pPrivateuse 3"       , pPrivateuse , "x-123"       , ["x","123"])
    , ("pPrivateuse 4"       , pPrivateuse , "x-1234"      , ["x","1234"])
    , ("pPrivateuse 5"       , pPrivateuse , "x-12345"     , ["x","12345"])
    , ("pPrivateuse 6"       , pPrivateuse , "x-123456"    , ["x","123456"])
    , ("pPrivateuse 7"       , pPrivateuse , "x-1234567"   , ["x","1234567"])
    , ("pPrivateuse 8-1"     , pPrivateuse , "x-12345678"  , ["x","12345678"])
    , ("pPrivateuse 8-2"     , pPrivateuse , "x-123456789" , ["x","12345678"])
    , ("pPrivateuse TODO "   , pPrivateuse , "x-123-"      , ["x","123"])  -- TODO correct ?
    , ("pPrivateuse 2 parts" , pPrivateuse , "x-123-abcd"  , ["x","123"                      , "abcd"])
    , ("pPrivateuse 3 parts" , pPrivateuse , "x-1-ab-1ab"  , ["x","1"                        , "ab"     , "1ab"])
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
      ("pExtension error 1" , pExtension , "x-aa"          , ('E', "error") )
    , ("pExtension error 2" , pExtension , "x-"            , ('E', "error") )
    , ("pExtension error 3" , pExtension , "a-a"           , ('E', "error") )
    , ("pExtension 1"       , pExtension , "b-123-456"     , ('b', "123"  ) )
    , ("pExtension 2"       , pExtension , "b-123-456-xyz" , ('b', "123"  ) )
  ]

spec_extension = do
  before getContext $ do
    describe "extension" $ makeIts testExtension testCharStr

pExtension2 = option [] (many' (char '-' >> pExtension))
testExtension2 = [
    ("pExtension2 1", pExtension2, "-b-123-c-456", [('b',"123"),('c',"456")])
  , ("pExtension2 2", pExtension2, "-b-123-c-456-d-xyz", [('b',"123"),('c',"456"),('d',"xyz")])
  ]

spec_extension2 = do
  before getContext $ do
    describe "extension" $ makeIts testExtension2 testCharStrList

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
    ,Langtag (ISO639 "de" []) Nothing (Just "CH") [] [] ["x","phonebk"])
  , ("Private use subtags 2",
     pLangtag, "az-Arab-x-AZE-derbend"
    ,Langtag (ISO639 "az" []) (Just "Arab") Nothing [] [] ["x","AZE", "derbend"])
  -- Tags that use extensions (examples ONLY -- extensions MUST be defined by revision or update to this document, or by RFC):
  , ("extention 1",  
     pLangtag, "en-US-u-islamcal"
    ,Langtag (ISO639 "en" []) Nothing (Just "US") [] [('u',"islamcal")] [])
  , ("extention 2",  
     pLangtag, "zh-CN-a-myext-x-private"
    ,Langtag (ISO639 "zh" []) Nothing (Just "CN") [] [('a',"myext")] ["x","private"])
  , ("extention 3",  
     pLangtag, "en-a-myext-b-another"
    ,Langtag (ISO639 "en" []) Nothing Nothing  [] [('a',"myext"),('b',"another")] [])
  -- example : 2.2.7.  Private Use Subtags 
  , ("all subtags following the singleton 'x' MUST be considered private use",  
     pLangtag, "en-x-US"
    ,Langtag (ISO639 "en" []) Nothing Nothing [] [] ["x","US"])
  -- Tags that use extensions
  , ("Tags that use extensions 1",  
     pLangtag, "en-US-u-islamcal"
    ,Langtag (ISO639 "en" []) Nothing (Just "US") [] [('u',"islamcal")] [])
  , ("Tags that use extensions 2",  
     pLangtag, "zh-CN-a-myext-x-private"
    ,Langtag (ISO639 "zh" []) Nothing (Just "CN") [] [('a',"myext")] ["x","private"])
  , ("Tags that use extensions 3",  
     pLangtag, "en-a-myext-b-another"
    ,Langtag (ISO639 "en" []) Nothing Nothing [] [('a',"myext"),('b',"another")] [])
  -- 
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
--  , ("",  
--     pLangtag, ""
--    ,Langtag (ISO639 "" []) (Just "") Nothing [] [] [])
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

testPLanguageTag = [
  -- Private use registry values:
    ("private use using the singleton 'x'",  
     pLanguageTag , "x-whatever"
    ,Private ["x", "whatever"])
  , ("all private tags",  
     pLanguageTag , "qaa-Qaaa-QM-x-southern"
    ,Normal $ Langtag (ISO639 "qaa" []) (Just "Qaaa") (Just "QM") [] [] ["x","southern"])
  , ("German, with a private script",  
     pLanguageTag , "de-Qaaa"
    ,Normal $ Langtag (ISO639 "de" []) (Just "Qaaa") Nothing [] [] [])
  , ("Serbian, Latin script, private region",  
     pLanguageTag , "sr-Latn-QM"
    ,Normal $ Langtag (ISO639 "sr" []) (Just "Latn") (Just "QM") [] [] [])
  , ("Serbian, private script, for Serbia",  
     pLanguageTag , "sr-Qaaa-RS"
    ,Normal $ Langtag (ISO639 "sr" []) (Just "Qaaa") (Just "RS") [] [] [])
  ] 

spec_languagetag= do
  before getContext $ do
    describe "languageTag" $ makeIts testPLanguageTag testWithError

testInvalid = [
  -- An extension MUST follow at least a primary language subtag.
    ("That is, a language tag cannot begin with an extension",  
     pLanguageTag , "a-value"
    ,"Failed reading: empty")
  -- well-formed
  , ("not well formed 1", pLanguageTag , "av*alue" ,"Failed reading: empty")
  , ("not well formed 2", pLanguageTag , "1" ,"Failed reading: empty")
  , ("not well formed 3", pLanguageTag , "abcdefghi" ,"Failed reading: empty")
  ]

spec_invalid = do
  before getContext $ do
    describe "languageTag" $ makeIts testInvalid testError


testPPrivateOfPrimaryLanguageSubtag = [
    ("", pPrivateOfPrimaryLanguageSubtag , "qaz", "qaz" )
  , ("", pPrivateOfPrimaryLanguageSubtag , "qaa", "qaa" )
  , ("", pPrivateOfPrimaryLanguageSubtag , "qtz", "qtz" )
  , ("", pPrivateOfPrimaryLanguageSubtag , "otz", "error" )
  , ("", pPrivateOfPrimaryLanguageSubtag , "quz", "error" )
  , ("", pPrivateOfPrimaryLanguageSubtag , "qa", "error" )
  , ("", pPrivateOfPrimaryLanguageSubtag , "qaaa", "error" )
--  , ("",  pVariant, "aaaaa", "aaaaa" )
  ]

spec_pPrivateOfPrimaryLanguageSubtag = do
  before getContext $ do
    describe "spec_pPrivateOfPrimaryLanguageSubtag" $ makeIts testPPrivateOfPrimaryLanguageSubtag testStr


spec_eqStringCI = do
  before getContext $ do
    describe "eqStringCI" $ do
     it "abc-abc" $ \_-> eqStringCI "abc" "abc" `shouldBe` True
     it "abc-ABC" $ \_-> eqStringCI "abc" "ABC" `shouldBe` True
     it "abc-Abc" $ \_-> eqStringCI "abc" "AbC" `shouldBe` True
     it "abc-zzz" $ \_-> eqStringCI "abc" "zzz" `shouldBe` False
     it "abc-" $ \_-> eqStringCI "abc" ""       `shouldBe` False

spec_eqStringListCI = do
  before getContext $ do
    describe "eqStringCI" $ do
     it "abc-abc" $ \_-> eqStringListCI ["abc"] ["abc"] `shouldBe` True
     it "abc-ABC" $ \_-> eqStringListCI ["abc"] ["ABC"] `shouldBe` True
     it "abc-Abc" $ \_-> eqStringListCI ["abc"] ["AbC"] `shouldBe` True
     it "abc-zzz" $ \_-> eqStringListCI ["abc"] ["zzz"] `shouldBe` False
     it "abc-efg ABC-EFG" $ \_-> eqStringListCI ["abc","efg"] ["ABC","EFG"] `shouldBe` True
     it "abc-efg ABC" $ \_-> eqStringListCI ["abc","efg"] ["ABC"] `shouldBe` False
     it "abc-efg ABC" $ \_-> eqStringListCI ["abc","efg"] [] `shouldBe` False 

spec_isDuplicate = do
  before getContext $ do
    describe "isDuplicate" $ do
     it "aa-bb-cc" $ \_-> isDuplicate ["aa", "bb", "cc"] `shouldBe` False
     it "empty" $ \_-> isDuplicate ([] :: [String]) `shouldBe` False
     it "aa" $ \_-> isDuplicate ["aa"] `shouldBe` False
     it "aa-bb-aa" $ \_-> isDuplicate ["aa", "bb", "aa"] `shouldBe` True


testformat = [
    ("country", "aa", "aa")
  , ("country", "BB", "bb")
  , ("country", "Cc", "cc")
  , ("country", "dD", "dd")
  , ("country", "aaa", "aaa")
  , ("country", "BBB", "bbb") -- https://www.loc.gov/standards/iso639-2/faq.html FAQ 21
  , ("script", "AA-xyyy", "aa-Xyyy") 
  , ("script", "AA-xYYY", "aa-Xyyy") 
  , ("region" , "AA-xx", "aa-XX") 
  , ("region" , "AA-xX", "aa-XX") 
  , ("region" , "AA-Xx", "aa-XX") 
  , ("region" , "AA-XX", "aa-XX") 
  , ("region" , "AA-xx", "aa-XX") 
  , ("region" , "AA-123", "aa-123") 
  , ("rule base" , "EN-ca-X-ca", "en-CA-x-ca") 
  , ("rule base" , "EN-ca-X-CA", "en-CA-x-CA") 
  , ("rule base" , "SGN-be-fr", "sgn-BE-FR") 
  , ("rule base" , "Az-lATN-x-latn", "az-Latn-x-latn") 
  , ("rule base" , "Az-lATN-x-LATN", "az-Latn-x-LATN") 
  ]

spec_format = do
  before getContext $ do
    describe "format" $ do
      makeIts testformat
  where 
    makeIts dada  = foldl1 (>>) $ fmap it_ dada 
    it_ (desc, input, exp)
      = it (desc ++ " : " ++ input ++ " -> " ++ exp) $ \ctc -> format (read input :: LanguageTag) `shouldBe` exp
 
-- spec_ isDuplicateVariant = do
--   before getContext $ do
--     describe "isDuplicateVariant" $ do
--      it "aa-bb-cc" $ \_-> isDuplicate ["aa", "bb", "cc"] `shouldBe` False
--      it "empty" $ \_-> isDuplicate ([] :: [String]) `shouldBe` False
--      it "aa" $ \_-> isDuplicate ["aa"] `shouldBe` False
--      it "aa-bb-aa" $ \_-> isDuplicate ["aa", "bb", "aa"] `shouldBe` True


{-
  , ("Tags that use extensions 2",  
     pLangtag, "zh-CN-a-myext-x-private"
    ,Langtag (ISO639 "zh" []) Nothing (Just "CN") [] ["myext"] ["private"])
-}

makeIts dada check = foldl1 (>>) $ fmap it_ dada 
  where
    it_ (casedesc, myparser, input , expected)
      = it casedesc $ \ctc -> check myparser input
                         `shouldBe`
                       expected
  
testStrList p inp = either (\x -> ["error"]) id (parseOnly p inp)
testStr p inp = either (\x -> "error") id (parseOnly p inp)
testStr' p inp = either (show) id (parseOnly p inp)
testChar p inp = either (\x -> '@') id (parseOnly p inp)
testWithError p inp = either (\x -> error "error!!") id (parseOnly p inp)
testError p inp = either id (\x -> error "error!!") (parseOnly p inp) 

testCharStr p inp = either (\x -> ('E',"error")) id (parseOnly p inp)
testCharStrList p inp = either (\x -> [('E',"error")]) id (parseOnly p inp)

