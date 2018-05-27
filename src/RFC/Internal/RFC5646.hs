{-|
Module      : RFC5646
Description : the parser of RFC5646
Copyright   : (c) Naoto 0gawa 2018-
License     : MIT 
Maintainer  : Naoto Ogawa 
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , ExplicitForAll 
#-}

module RFC.Internal.RFC5646
where 

import Control.Applicative
import Control.Lens
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Internal.Types as T
import Data.ByteString.Char8 as BC (pack, unpack)
import Data.List

-- TODO test validity check and doctest
--
-- equality
--

-- | case insensitive equality
eqCharCI x y = 
  if x == y
  then True
  else if 65 <= x' && x' <= 90 
       then
         x' + 32 == y' -- capital vs small
       else
         x' - 32 == y' -- small vs capital
  where 
   x' = fromEnum x
   y' = fromEnum y

eqStringCI x y = 
  if length x == length y
  then and $ zipWith eqCharCI x y
  else False

eqStringListCI x y = 
  if length x == length y 
  then and $ zipWith eqStringCI x y
  else False 

toLower :: Char -> Char
toLower x = if 65 <= x' && x' <= 90 then toEnum (x' + 32) else x where x' = fromEnum x

lowerStr = fmap toLower

toUpper :: Char -> Char
toUpper x = if 65 <= x' && x' <= 90 then x else toEnum (x' - 32) where x' = fromEnum x

toTitleCase :: String -> String
toTitleCase []      = []
toTitleCase (x:xs)  = toUpper x : lowerStr xs

sepByMinus :: [Char] -> [[Char]]
sepByMinus = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== '-') $ l

-- All subtags, including extension and private
-- use subtags, use lowercase letters with two exceptions: two-letter
-- and four-letter subtags that neither appear at the start of the tag
-- nor occur after singletons.
_formatAll input = intercalate "-" $ lowerStr x : (snd $ foldl foldFun ("",[]) xs)
  where 
    x:xs = sepByMinus input
    rule y = case length y of
               2 -> fmap toUpper y
               4 -> toTitleCase  y 
               _ -> lowerStr     y
    judge (b, c) = if length b == 1 then c else rule c
    foldFun = \( b     -- b   : a precedence value of the current value
               ,ret)   -- ret : an accumulated value
               c       -- c   : the current value
               -> (c   -- the crrent value becomes a precedence for the nexe process.
                  , ret ++         -- an accumulated value 
                    [judge (b, c)] -- converting the current value(c) depends on the precedence value(b)
                  )  

class Convention a where
  toString :: a -> String
  format   :: a -> String
  format   = _formatAll . toString

-- 
-- Types
--

{-
LanguageTag 
   | Normal Langtag
   |           + Language
   |           |   | ISO639
   |           |   |   |  _primary  -- [ISO639-1] recommends that language codes be written in lowercase.
   |           |   |   |  _extended
   |           |   | Resv  
   |           |   | LangSubtag
   |           + script             -- [ISO15924] recommends that script codes use lowercase with the initial letter capitalized.
   |           + region             -- [ISO3166-1] recommends that country codes be capitalized.
   |           + variant
   |           + extension
   |           + priateuse
   | Private
   | Grandfathered 
-}

data Language = 
    ISO639 {_primary :: String, _extended :: [String]} 
  | Resv String
  | LangSubtag String
  deriving (Show)

makePrisms ''Language
makeLenses ''Language

instance Convention Language where
  toString x = 
    case x of 
      Resv       x -> x
      LangSubtag x -> x
      x ->  intercalate "-" $ x ^. primary : x ^. extended

instance Eq Language where
  Resv x       == Resv y       = eqStringCI x y
  LangSubtag x == LangSubtag y = eqStringCI x y
  ISO639 x1 x2 == ISO639 y1 y2 = eqStringCI x1 y1 && eqStringListCI x2 y2
  _            == _            = False
 
data Langtag = Langtag {
    _language   :: Language
  , _script     :: Maybe String
  , _region     :: Maybe String
  , _variant    :: [String]
  , _extension  :: [(Char, String)]
  , _privateuse :: [String] 
  }
  deriving (Show)

makeLenses ''Langtag

instance Convention Langtag where
  toString x = 
    (concat . intersperse "-" ) $
    (x ^. language & format   ) :
    (x ^. script ^.. folded   ) ++
    (x ^. region ^.. folded   ) ++
    (x ^. variant             ) ++
    (x ^. (extension . to toStringExtension )          ) ++
    (x ^. privateuse          )
    where 
      toStringExtension = map (\(x, y) -> x : y)

instance Eq Langtag where
  (==) x y =
    x ^. language == y ^. language                     &&
    x ^. script   == y ^. script                       &&
    x ^. region   == y ^. region                       &&
    eqStringListCI (x ^. variant   ) (y ^. variant   ) &&
    eqExtensionList (x ^. extension ) (y ^. extension ) &&
    eqStringListCI (x ^. privateuse) (y ^. privateuse)
    where 
      eqExtension (a, b) (c, d) = eqCharCI a c && eqStringCI b d 
      eqExtensionList x y = 
        if length x == length y 
        then and $ zipWith eqExtension x y
        else False 

data LanguageTag =
    Normal Langtag
  | Private [String]
  | Grandfathered String
  deriving (Show)

makePrisms ''LanguageTag

instance Convention LanguageTag where
  toString (Normal x)        = toString x
  toString (Private xs)      = concat $ intersperse "-" xs 
  toString (Grandfathered x) = x

instance Eq LanguageTag where
  (==) (Normal x       ) (Normal y       ) = x == y
  (==) (Private xs     ) (Private ys     ) = eqStringListCI xs ys
  (==) (Grandfathered x) (Grandfathered y) = x == y
  (==) _                 _                 = False

-- |  Language-Tag  = langtag             ; normal language tags
-- |                / privateuse          ; private use tag
-- |                / grandfathered       ; grandfathered tags
pLanguageTag :: Parser LanguageTag 
pLanguageTag = choice [private, granpa, normal] 
  where 
    normal  = pLangtag       >>= return . Normal
    private = pPrivateuse    >>= return . Private
    granpa  = pGrandfathered >>= return .Grandfathered . BC.unpack

-- |  langtag       = language
-- |                  ["-" script]
-- |                  ["-" region]
-- |                  *("-" variant)
-- |                  *("-" extension)
-- |                  ["-" privateuse]
-- | 
pLangtag :: Parser Langtag
pLangtag = do
  lang <- pLanguage
  scri <- option "" (char '-' >> pScirpt    )
  regi <- option "" (char '-' >> pRegion    )
  vari <- many'     (char '-' >> pVariant   )
  extn <- option [] (many' (char '-' >> pExtension))
  priv <- option [] (char '-' >> pPrivateuse)
  return $ Langtag lang (may scri) (may regi) vari extn priv
  where may x = if null x then Nothing else Just x

-- |  language      = 2*3ALPHA            ; shortest ISO 639 code
-- |                  ["-" extlang]       ; sometimes followed by
-- |                                      ; extended language subtags
-- |                / 4ALPHA              ; or reserved for future use
-- |                / 5*8ALPHA            ; or registered language subtag
-- | 
pLanguage :: Parser Language
pLanguage = do
  w <- (iso639 <|> reserved <|> langsubtag)
  pCheckEnd "pLanguage" $ w 
  where 
    iso639 = do
      x <- range 2 3 _pAlpha 
      y <- option [] pExtlang 
      return $ ISO639 x y 
    reserved   = do
      x <- count 4 _pAlpha
      return $ Resv x
    langsubtag = do
      x <- range 5 8 _pAlpha
      return $ LangSubtag x

-- |  extlang       = 3ALPHA              ; selected ISO 639 codes
-- |                  *2("-" 3ALPHA)      ; permanently reserved
-- pExtlang = (char '-' >> count 3 _pAlpha >>= \x -> return [x]) --  <|> resv)
pExtlang = range 1 3 (char '-' >> iso639) --  <|> resv)
  where 
   iso639 = do
     x <- count 3 _pAlpha
     pCheckEnd "not pExtLang" x

-- |  script        = 4ALPHA              ; ISO 15924 code
pScirpt = do 
  x <- count 4 _pAlpha
  pCheckEnd "not pScript" x

-- |  region        = 2ALPHA              ; ISO 3166-1 code
-- |                / 3DIGIT              ; UN M.49 code
pRegion = do
  x <- count 2 _pAlpha <|> count 3 digit 
  pCheckEnd "not pRegion" x

-- |  variant       = 5*8alphanum         ; registered variants
-- |                / (DIGIT 3alphanum)
-- pVariant = anyChar >> return "aaaaa"
pVariant = do
  x <- (vari1  <|> vari2)
  pCheckEnd "not pVariant" x
  where 
    vari1 = range 5 8 pAlphanum 
    vari2 = do 
      d <- digit
      x <- count 3 pAlphanum
      return $ d : x

-- | extension     = singleton 1*("-" (2*8alphanum))
pExtension :: Parser (Char, String)
pExtension = do
  s  <- pSingleton
  char '-'
  ex <- range 2 8 pAlphanum
  return (s, ex)

-- |
-- |                                     ; Single alphanumerics
-- |                                     ; "x" reserved for private use
-- | singleton     = DIGIT               ; 0 - 9
-- |               / %x41-57             ; A - W
-- |               / %x59-5A             ; Y - Z
-- |               / %x61-77             ; a - w
-- |               / %x79-7A             ; y - z
pSingleton :: Parser Char
pSingleton = 
  digit                                <|>
  satisfy (\c -> c >= 'A' && c <= 'W') <|>
  satisfy (\c -> c >= 'Y' && c <= 'Z') <|>
  satisfy (\c -> c >= 'a' && c <= 'w') <|>
  satisfy (\c -> c >= 'y' && c <= 'z')

-- | alphanum      = (ALPHA / DIGIT)     ; letters and numbers
pAlphanum :: Parser Char
pAlphanum = _pAlpha <|> digit

_pAlpha = satisfy isAlpha_ascii 

-- | privateuse    = "x" 1*("-" (1*8alphanum))
pPrivateuse :: Parser [String]
pPrivateuse = do 
  x  <- char 'x' <|> char 'X' 
  xs <- many1 _pPrivateuse
  return $ [x] : xs

_pPrivateuse :: Parser [Char]
_pPrivateuse = do
  char '-' 
  x <- range 1 8 pAlphanum 
  return x

-- | grandfathered = irregular           ; non-redundant tags registered
-- |                / regular             ; during the RFC 3066 era
pGrandfathered = pIrregular <|> pRegular

-- | irregular     = "en-GB-oed"         ; irregular tags do not match
-- |               / "i-ami"             ; the 'langtag' production and
-- |               / "i-bnn"             ; would not otherwise be
-- |               / "i-default"         ; considered 'well-formed'
-- |               / "i-enochian"        ; These tags are all valid,
-- |               / "i-hak"             ; but most are deprecated
-- |               / "i-klingon"         ; in favor of more modern
-- |               / "i-lux"             ; subtags or subtag
-- |               / "i-mingo"           ; combination
-- |               / "i-navajo"
-- |               / "i-pwn"
-- |               / "i-tao"
-- |               / "i-tay"
-- |               / "i-tsu"
-- |               / "sgn-BE-FR"
-- |               / "sgn-BE-NL"
-- |               / "sgn-CH-DE"
pIrregular = 
  stringCI "en-GB-oed"  <|>
  stringCI "i-ami"      <|>
  stringCI "i-bnn"      <|>
  stringCI "i-default"  <|>
  stringCI "i-enochian" <|>
  stringCI "i-hak"      <|>
  stringCI "i-klingon"  <|>
  stringCI "i-lux"      <|>
  stringCI "i-mingo"    <|>
  stringCI "i-navajo"   <|>
  stringCI "i-pwn"      <|>
  stringCI "i-tao"      <|>
  stringCI "i-tay"      <|>
  stringCI "i-tsu"      <|>
  stringCI "sgn-BE-FR"  <|>
  stringCI "sgn-BE-NL"  <|>
  stringCI "sgn-CH-DE"

-- | regular       = "art-lojban"        ; these tags match the 'langtag'
-- |               / "cel-gaulish"       ; production, but their subtags
-- |               / "no-bok"            ; are not extended language
-- |               / "no-nyn"            ; or variant subtags: their meaning
-- |               / "zh-guoyu"          ; is defined by their registration
-- |               / "zh-hakka"          ; and all of these are deprecated
-- |               / "zh-min"            ; in favor of a more modern
-- |               / "zh-min-nan"        ; subtag or sequence of subtags
-- |               / "zh-xiang"<Paste>
pRegular = 
  stringCI "art-lojban"        <|>
  stringCI "cel-gaulish"       <|>
  stringCI "no-bok"            <|>
  stringCI "no-nyn"            <|>
  stringCI "zh-guoyu"          <|>
  stringCI "zh-hakka"          <|>
  stringCI "zh-min"            <|>
  stringCI "zh-min-nan"        <|>
  stringCI "zh-xiang"

pCheckEnd msg x = do
  z <- peekChar
  case z of
    Just y  -> if y == '-' then return x else fail msg 
    Nothing -> return x 

--
-- check privateness.
--

-- | The subtags in the range 'qaa' through 'qtz' are reserved for private use in language tags.
pPrivateOfPrimaryLanguageSubtag = pFromTo "isPrivateOfPrimaryLanguageSubtag" "qaa" "qtz"

isPrivateOfPrimaryLanguageSubtag :: Langtag -> Bool
isPrivateOfPrimaryLanguageSubtag langtag = langtag ^. (language . primary) & isPrivateOfPrimaryLanguageSubtag'

isPrivateOfPrimaryLanguageSubtag' :: String -> Bool
isPrivateOfPrimaryLanguageSubtag' = pBoolParse  pPrivateOfPrimaryLanguageSubtag

-- | The script subtags 'Qaaa' through 'Qabx' are reserved for private use in language tags.  
-- | These subtags correspond to codes reserved by ISO 15924 for private use.
pPrivateOfScript = pFromTo "isPrivateOfScript" "Qaaa" "Qabx"

isPrivateOfScript  :: Langtag -> Bool
isPrivateOfScript  langtag = 
  case langtag ^. script of
    Just x  -> isPrivateOfScript' x
    Nothing -> False

isPrivateOfScript'  :: String -> Bool
isPrivateOfScript'  = pBoolParse pPrivateOfScript

-- | The region subtags 'AA', 'QM'-'QZ', 'XA'-'XZ', and 'ZZ' are reserved for private use in language tags.
-- | These subtags correspond to codes reserved by ISO 3166 for private use
pPrivateOfRegion = 
  pFromTo "isPrivateOfRegion" "AA" "AA" <|>
  pFromTo "isPrivateOfRegion" "QM" "QZ" <|>
  pFromTo "isPrivateOfRegion" "XA" "XZ" <|> 
  pFromTo "isPrivateOfRegion" "ZZ" "ZZ"

isPrivateOfRegion  :: Langtag -> Bool
isPrivateOfRegion  langtag = 
  case langtag ^. region of
    Just x  -> isPrivateOfRegion' x
    Nothing -> False

isPrivateOfRegion'  :: String -> Bool
isPrivateOfRegion'  = pBoolParse pPrivateOfRegion

--
-- well-formedness
--
isValidString :: String -> Bool
isValidString = _either2Bool . readLanguageTagEither'

eitherValid :: LanguageTag -> Either String LanguageTag
eitherValid tag =  
  if isDuplicateVariant tag 
  then Left $ "Duplicata Variant : " ++ (show tag) 
  else if isDuplicateExtension tag 
       then Left $ "Duplicata Extension : " ++ (show tag) 
       else Right tag 

isValid :: LanguageTag -> Bool 
isValid = _either2Bool . eitherValid

isDuplicateVariant :: LanguageTag -> Bool
isDuplicateVariant lan = lan ^. (_Normal . variant) & isDuplicate 

isDuplicateExtension :: LanguageTag -> Bool
isDuplicateExtension lan = lan ^. (_Normal . extension) & isDuplicate 

--
-- useful functions for attoparsec
--
pBoolParse :: Parser a -> String -> Bool
pBoolParse myParser input =
  case parseOnly myParser $ BC.pack input of
    Left  _ -> False
    Right _ -> True

pFromTo msg from to = do
  x <- foldP $ zipWith (\a b -> (a,b)) from to 
  pCheckEnd msg x
  where 
    satisfyChar from to = satisfy (\c -> c >= from && c <= to)
    addP sc1 sc2 = do
      x <- sc1
      y <- sc2
      return $ x : y 
    foldP = foldr (\(a,b) acc -> (satisfyChar a b) `addP` acc) (return [])

-- | an equivalence of regrex {min, max}
range :: Int -> Int -> T.Parser i a -> T.Parser i [a]
range n m p = (++) 
              <$> count n p       -- mimimam
              <*> upto (m - n) p  -- rest

-- | until n times.
upto :: Int -> T.Parser i a -> T.Parser i [a]
upto 0 _ = return []
upto n p = (:) <$> 
               p                -- at least once
           <*> upto (n - 1) p   -- repeat
           <|> return []        -- in case of faliure

-- | check duplication
isDuplicate :: (Eq a) => [a] -> Bool
isDuplicate = any check . tails
  where 
    check []      = False
    check (x:xs) = x `elem` xs

instance Read Langtag where
  readsPrec _ input = _readEihter readLangtagEither input

instance Read Language where
  readsPrec _ input = _readEihter readLanguageEither input

instance Read LanguageTag where
  readsPrec _ input = _readEihter readLanguageTagEither input

readLanguageEither    = parseOnly pLanguage    . pack

readLangtagEither     = parseOnly pLangtag     . pack

readLanguageTagEither = parseOnly pLanguageTag . pack

readLanguageTagEither' input =
  case readLanguageTagEither input of
    Left  x -> Left x 
    Right x -> eitherValid x

_readEihter myReader input = 
  case myReader input of
    Left  _ -> [] 
    Right x -> [(x, "")]

_either2Bool x = case x of Left _ -> False; Right _ -> True


