{-|
Module      : RFC5646
Description : the parser of RFC5646
Copyright   : (c) Naoto 0gawa 2018-
License     : MIT 
Maintainer  : Naoto Ogawa 
Stability   : experimental
Portability : POSIX

== a  parser of RFC5646

https://tools.ietf.org/html/rfc5646

== Features

  * Lens-enabled
  * instance of Read class

      * use readMaby

  * instance of Eq class

      * case insensitive equality

  * support for common convention formatting


  * check if a tag is valid


  * check if a tag is private

== Basic usage :

>>> read "de" :: LanguageTag
Normal (Langtag {_language = ISO639 {_primary = "de", _extended = []}, _script = Nothing, _region = Nothing, _variant = [], _extension = [], _privateuse = []})

>>> read "DE" :: LanguageTag
Normal (Langtag {_language = ISO639 {_primary = "DE", _extended = []}, _script = Nothing, _region = Nothing, _variant = [], _extension = [], _privateuse = []})

== Equality :

>>> let l1 =read "de" :: LanguageTag
>>> let l2 =read "DE" :: LanguageTag
>>> l1 == l2
True

== Tag Access : 

=== zh-Hant (Chinese written using the Traditional Chinese script)

>>> let c1 = read "zh-Hant" :: LanguageTag
>>> c1 ^. (_Normal . language . primary)
"zh"
>>> c1 ^. (_Normal . language . extended)
[]
>>> c1 ^. (_Normal . script)
Just "Hant"
>>> c1 ^. (_Normal . region)
Nothing
>>> c1 ^. (_Normal . variant)
[]
>>> c1 ^. (_Normal . extension)
[]
>>> c1 ^. (_Normal . privateuse )
[]

=== zh-cmn-Hans-CN (Chinese, Mandarin, Simplified script, as used in China)

>>> let c2 = read "zh-cmn-Hans-CN" :: LanguageTag
>>> c2 ^. (_Normal . language . primary)
"zh"
>>> c2 ^. (_Normal . language . extended)
["cmn"]
>>> c2 ^. (_Normal . script)
Just "Hans"
>>> c2 ^. (_Normal . region)
Just "CN"
>>> c2 ^. (_Normal . variant)
[]
>>> c2 ^. (_Normal . extension)
[]
>>> c2 ^. (_Normal . privateuse)
[]

=== sl-rozaj-biske (San Giorgio dialect of Resian dialect of Slovenian)

>>> let c3 = read "sl-rozaj-biske" :: LanguageTag
>>> c3 ^. (_Normal . language . primary)
"sl"
>>> c3 ^. (_Normal . language . extended)
[]
>>> c3 ^. (_Normal . script)
Nothing
>>> c3 ^. (_Normal . region)
Nothing
>>> c3 ^. (_Normal . variant)
["rozaj","biske"]
>>> c3 ^. (_Normal . extension)
[]
>>> c3 ^. (_Normal . privateuse)
[]

=== az-Arab-x-AZE-derbend (Private use subtags)

>>> let c4 = read "az-Arab-x-AZE-derbend" :: LanguageTag
>>> c4
Normal (Langtag {_language = ISO639 {_primary = "az", _extended = []}, _script = Just "Arab", _region = Nothing, _variant = [], _extension = [], _privateuse = ["x","AZE","derbend"]})
>>> c4 ^. (_Normal . language . primary)
"az"
>>> c4 ^. (_Normal . language . extended)
[]
>>> c4 ^. (_Normal . script)
Just "Arab"
>>> c4 ^. (_Normal . region)
Nothing
>>> c4 ^. (_Normal . variant)
[]
>>> c4 ^. (_Normal . extension)
[]
>>> c4 ^. (_Normal . privateuse)
["x","AZE","derbend"]

== Privateness : 

=== qaa-Qaaa-QM-x-southern (all private tags)

>>> let c5 = read "qaa-Qaaa-QM-x-southern" :: LanguageTag
>>> c5 & previews _Normal isPrivateOfPrimaryLanguageSubtag
Just True
>>> c5 & previews _Normal isPrivateOfScript
Just True
>>> c5 & previews _Normal isPrivateOfPrimaryLanguageSubtag
Just True

== Shortcut :

When you know that you don't use a grandfathered tag nor a private tag at the beginning, use Langtag type insted of LanguageTag.

>>> let c2' = read "zh-cmn-Hans-CN" :: Langtag
>>> c2'
Langtag {_language = ISO639 {_primary = "zh", _extended = ["cmn"]}, _script = Just "Hans", _region = Just "CN", _variant = [], _extension = [], _privateuse = []}
>>> c2' ^. (language . primary)
"zh"
>>> c2' ^. (language . extended)
["cmn"]
>>> c2' ^. script
Just "Hans"
>>> c2' ^. region
Just "CN"

== Safty :

The read function is not safe, so use readMaybe

>>> import Text.Read
>>> readMaybe "qaa-Qaaa-QM-x-southern" :: Maybe LanguageTag
Just (Normal (Langtag {_language = ISO639 {_primary = "qaa", _extended = []}, _script = Just "Qaaa", _region = Just "QM", _variant = [], _extension = [], _privateuse = ["x","southern"]}))

>>> readMaybe "abcdefg" :: Maybe LanguageTag
Nothing

== Validity : 

>>> eitherValid "a-b-c"
Left "parse error : a-b-c"

=== duplication check

>>> isValid $ read "xxx-xxxxx"
True
>>> isValid $ read "xxx-xxxxx-xxxxx"
False

>>> isValid $ read "xxx-a-xxxxx-b-xxxxx"
True
>>> isValid $ read "xxx-a-xxxxx-a-xxxxx"
False


-}

{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , ExplicitForAll 
#-}

module RFC.RFC5646 (
    LanguageTag(..)
  , Langtag(..)
  , Language(..)
  , isPrivateOfPrimaryLanguageSubtag 
  -- | uniquness
  , isDuplicateVariant
  , isDuplicateExtension
  -- | validity
  , isValidString
  , eitherValid
  , isValid
) where 

import RFC.Internal.RFC5646
import Control.Applicative
import Control.Lens

