# RFC5646

A parser of [RFC5646](https://tools.ietf.org/html/rfc5646) in Haskell. 

   <a>https://tools.ietf.org/html/rfc5646</a>
   
   <h2>Features</h2>
   
   <ul>
   <li>Lens-enabled</li>
   <li>instance of Read class<ul><li>use readMaby</li></ul></li>
   </ul>
   
   <ul>
   <li>instance of Eq class<ul><li>case insensitive
   equality</li></ul></li>
   <li>support for common convention formatting</li>
   <li>check if a tag is valid</li>
   <li>check if a tag is private</li>
   </ul>
   
   <h2>Basic usage :</h2>
   
   <pre>
   &gt;&gt;&gt; read "de" :: LanguageTag
   Normal (Langtag {_language = ISO639 {_primary = "de", _extended = []}, _script = Nothing, _region = Nothing, _variant = [], _extension = [], _privateuse = []})
   </pre>
   
   <pre>
   &gt;&gt;&gt; read "DE" :: LanguageTag
   Normal (Langtag {_language = ISO639 {_primary = "DE", _extended = []}, _script = Nothing, _region = Nothing, _variant = [], _extension = [], _privateuse = []})
   </pre>
   
   <h2>Equality :</h2>
   
   <pre>
   &gt;&gt;&gt; let l1 =read "de" :: LanguageTag
   
   &gt;&gt;&gt; let l2 =read "DE" :: LanguageTag
   
   &gt;&gt;&gt; l1 == l2
   True
   </pre>
   
   <h2>Tag Access : </h2>
   
   <h3>zh-Hant (Chinese written using the Traditional Chinese
   script)</h3>
   
   <pre>
   &gt;&gt;&gt; let c1 = read "zh-Hant" :: LanguageTag
   
   &gt;&gt;&gt; c1 ^. (_Normal . language . primary)
   "zh"
   
   &gt;&gt;&gt; c1 ^. (_Normal . language . extended)
   []
   
   &gt;&gt;&gt; c1 ^. (_Normal . script)
   Just "Hant"
   
   &gt;&gt;&gt; c1 ^. (_Normal . region)
   Nothing
   
   &gt;&gt;&gt; c1 ^. (_Normal . variant)
   []
   
   &gt;&gt;&gt; c1 ^. (_Normal . extension)
   []
   
   &gt;&gt;&gt; c1 ^. (_Normal . privateuse )
   []
   </pre>
   
   <h3>zh-cmn-Hans-CN (Chinese, Mandarin, Simplified script, as used in
   China)</h3>
   
   <pre>
   &gt;&gt;&gt; let c2 = read "zh-cmn-Hans-CN" :: LanguageTag
   
   &gt;&gt;&gt; c2 ^. (_Normal . language . primary)
   "zh"
   
   &gt;&gt;&gt; c2 ^. (_Normal . language . extended)
   ["cmn"]
   
   &gt;&gt;&gt; c2 ^. (_Normal . script)
   Just "Hans"
   
   &gt;&gt;&gt; c2 ^. (_Normal . region)
   Just "CN"
   
   &gt;&gt;&gt; c2 ^. (_Normal . variant)
   []
   
   &gt;&gt;&gt; c2 ^. (_Normal . extension)
   []
   
   &gt;&gt;&gt; c2 ^. (_Normal . privateuse)
   []
   </pre>
   
   <h3>sl-rozaj-biske (San Giorgio dialect of Resian dialect of
   Slovenian)</h3>
   
   <pre>
   &gt;&gt;&gt; let c3 = read "sl-rozaj-biske" :: LanguageTag
   
   &gt;&gt;&gt; c3 ^. (_Normal . language . primary)
   "sl"
   
   &gt;&gt;&gt; c3 ^. (_Normal . language . extended)
   []
   
   &gt;&gt;&gt; c3 ^. (_Normal . script)
   Nothing
   
   &gt;&gt;&gt; c3 ^. (_Normal . region)
   Nothing
   
   &gt;&gt;&gt; c3 ^. (_Normal . variant)
   ["rozaj","biske"]
   
   &gt;&gt;&gt; c3 ^. (_Normal . extension)
   []
   
   &gt;&gt;&gt; c3 ^. (_Normal . privateuse)
   []
   </pre>
   
   <h3>az-Arab-x-AZE-derbend (Private use subtags)</h3>
   
   <pre>
   &gt;&gt;&gt; let c4 = read "az-Arab-x-AZE-derbend" :: LanguageTag
   
   &gt;&gt;&gt; c4
   Normal (Langtag {_language = ISO639 {_primary = "az", _extended = []}, _script = Just "Arab", _region = Nothing, _variant = [], _extension = [], _privateuse = ["x","AZE","derbend"]})
   
   &gt;&gt;&gt; c4 ^. (_Normal . language . primary)
   "az"
   
   &gt;&gt;&gt; c4 ^. (_Normal . language . extended)
   []
   
   &gt;&gt;&gt; c4 ^. (_Normal . script)
   Just "Arab"
   
   &gt;&gt;&gt; c4 ^. (_Normal . region)
   Nothing
   
   &gt;&gt;&gt; c4 ^. (_Normal . variant)
   []
   
   &gt;&gt;&gt; c4 ^. (_Normal . extension)
   []
   
   &gt;&gt;&gt; c4 ^. (_Normal . privateuse)
   ["x","AZE","derbend"]
   </pre>
   
   <h2>Privateness : </h2>
   
   <h3>qaa-Qaaa-QM-x-southern (all private tags)</h3>
   
   <pre>
   &gt;&gt;&gt; let c5 = read "qaa-Qaaa-QM-x-southern" :: LanguageTag
   
   &gt;&gt;&gt; c5 &amp; previews _Normal isPrivateOfPrimaryLanguageSubtag
   Just True
   
   &gt;&gt;&gt; c5 &amp; previews _Normal isPrivateOfScript
   Just True
   
   &gt;&gt;&gt; c5 &amp; previews _Normal isPrivateOfPrimaryLanguageSubtag
   Just True
   </pre>
   
   <h2>Shortcut :</h2>
   
   When you know that you don't use a grandfathered tag nor a private tag
   at the beginning, use Langtag type insted of LanguageTag.
   
   <pre>
   &gt;&gt;&gt; let c2' = read "zh-cmn-Hans-CN" :: Langtag
   
   &gt;&gt;&gt; c2'
   Langtag {_language = ISO639 {_primary = "zh", _extended = ["cmn"]}, _script = Just "Hans", _region = Just "CN", _variant = [], _extension = [], _privateuse = []}
   
   &gt;&gt;&gt; c2' ^. (language . primary)
   "zh"
   
   &gt;&gt;&gt; c2' ^. (language . extended)
   ["cmn"]
   
   &gt;&gt;&gt; c2' ^. script
   Just "Hans"
   
   &gt;&gt;&gt; c2' ^. region
   Just "CN"
   </pre>
   
   <h2>Safty :</h2>
   
   The read function is not safe, so use readMaybe
   
   <pre>
   &gt;&gt;&gt; import Text.Read
   
   &gt;&gt;&gt; readMaybe "qaa-Qaaa-QM-x-southern" :: Maybe LanguageTag
   Just (Normal (Langtag {_language = ISO639 {_primary = "qaa", _extended = []}, _script = Just "Qaaa", _region = Just "QM", _variant = [], _extension = [], _privateuse = ["x","southern"]}))
   </pre>
   
   <pre>
   &gt;&gt;&gt; readMaybe "abcdefg" :: Maybe LanguageTag
   Nothing
   </pre>
   
   <h2>Validity : </h2>
   
   <pre>
   &gt;&gt;&gt; eitherValid "a-b-c"
   Left "parse error : a-b-c"
   </pre>
   
   <h3>duplication check</h3>
   
   <pre>
   &gt;&gt;&gt; isValid $ read "xxx-xxxxx"
   True
   
   &gt;&gt;&gt; isValid $ read "xxx-xxxxx-xxxxx"
   False
   </pre>
   
   <pre>
   &gt;&gt;&gt; isValid $ read "xxx-a-xxxxx-b-xxxxx"
   True
   
   &gt;&gt;&gt; isValid $ read "xxx-a-xxxxx-a-xxxxx"
   False
   </pre>


