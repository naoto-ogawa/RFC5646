{-# LANGUAGE
    OverloadedStrings
#-}

module RFC.RFC5646 (
    Language(..)
  , Langtag(..)
  , pLangtag
  , range
  , pVariant
  , pExtension 
  , pSingleton
  , pPrivateuse
  , _pPrivateuse
  , _pAlpha
) where 

import Control.Applicative
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Internal.Types as T

data Langtag = Langtag {
    _language   :: Language
  , _script     :: Maybe String
  , _region     :: Maybe String
  , _variant    :: [String]
  , _extension  :: [String]
  , _privateuse :: [String] 
  } deriving (Show, Eq)

data Language = 
  ISO639 String [String] |
  Resv String            |
  LangSubtag String
  deriving (Show, Eq)

-- |  Language-Tag  = langtag             ; normal language tags
-- |                / privateuse          ; private use tag
-- |                / grandfathered       ; grandfathered tags
-- pLanguageTag = pLangtag <|> pPrivateuse <|> pGrandfathered  TODO

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
--  return $ Langtag lang (may scri) (may regi) vari extn Nothing 
--  return $ Langtag lang (may scri) (may regi) vari [] Nothing 
--  return $ Langtag lang (may scri) (may regi) [] [] Nothing 
--  return $ Langtag lang (may scri) Nothing [] [] Nothing 
--  return $ Langtag lang Nothing  Nothing [] [] Nothing 
  where may x = if null x then Nothing else Just x

-- |  language      = 2*3ALPHA            ; shortest ISO 639 code
-- |                  ["-" extlang]       ; sometimes followed by
-- |                                      ; extended language subtags
-- |                / 4ALPHA              ; or reserved for future use
-- |                / 5*8ALPHA            ; or registered language subtag
-- | 
pLanguage :: Parser Language
pLanguage = iso639 <|> reserved <|> langsubtag 
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
     z <- peekChar
     case z of
       Just y  -> if y == '-' then return x else fail "not extlang"
       Nothing -> return x 

-- |  script        = 4ALPHA              ; ISO 15924 code
pScirpt = do 
  x <- count 4 _pAlpha
  z <- peekChar
  case z of
    Just y  -> if y == '-' then return x else fail "not pScript"
    Nothing -> return x 

-- |  region        = 2ALPHA              ; ISO 3166-1 code
-- |                / 3DIGIT              ; UN M.49 code
pRegion = do
  x <- count 2 _pAlpha <|> count 3 digit 
  z <- peekChar
  case z of
    Just y  -> if y == '-' then return x else fail "not pScript"
    Nothing -> return x 

-- |  variant       = 5*8alphanum         ; registered variants
-- |                / (DIGIT 3alphanum)
-- pVariant = anyChar >> return "aaaaa"
pVariant = do
  x <- (vari1  <|> vari2)
  z <- peekChar
  case z of
    Just y  -> if y == '-' then return x else fail "not pVariant"
    Nothing -> return x 
  where 
    vari1 = range 5 8 pAlphanum 
    vari2 = do 
      d <- digit
      x <- count 3 pAlphanum
      return $ d : x


-- | extension     = singleton 1*("-" (2*8alphanum))
pExtension :: Parser String
pExtension = pSingleton >> char '-' >> range 2 8 pAlphanum

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
  char 'x' 
  x <- many1 _pPrivateuse
  return x

_pPrivateuse :: Parser [Char]
_pPrivateuse = do
  char '-' 
  x <- range 1 8 pAlphanum 
  return x

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
               <|> return []    -- in case of faliure

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
  string "en-GB-oed" <|>
  string "i-ami"     <|>
  string "i-bnn"     <|>
  string "i-default" <|>
  string "i-enochian" <|>
  string "i-hak"     <|>
  string "i-klingon" <|>
  string "i-lux"     <|>
  string "i-mingo"   <|>
  string "i-navajo" <|>
  string "i-pwn" <|>
  string "i-tao" <|>
  string "i-tay" <|>
  string "i-tsu" <|>
  string "sgn-BE-FR" <|>
  string "sgn-BE-NL" <|>
  string "sgn-CH-DE"

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
  string "art-lojban"        <|>
  string "cel-gaulish"       <|>
  string "no-bok"            <|>
  string "no-nyn"            <|>
  string "zh-guoyu"          <|>
  string "zh-hakka"          <|>
  string "zh-min"            <|>
  string "zh-min-nan"        <|>
  string "zh-xiang"
