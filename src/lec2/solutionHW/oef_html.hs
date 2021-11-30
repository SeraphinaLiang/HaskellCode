
module Template where

-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = MkAttr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [MkAttr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]
    
-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Apples"],HtmlTag "li" [] [HtmlString "Bananas"],HtmlTag "li" [] [HtmlString "Oranges"]]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link hrf txt) = HtmlTag "a" [MkAttr "href" hrf] [HtmlString txt]

ul :: HtmlElements -> HtmlElement
ul = HtmlTag "ul" []

li :: HtmlElement -> HtmlElement
li e = HtmlTag "li" [] [e]

-- Haskell list instance.
instance HTML a => HTML [a] where
  toHtml as = ul $ map (li . toHtml) as

-- Exercise model datatype for an address book.
data AddressBook = AddressBook
    String    -- Owner
    [Person]  -- Contacts

data Person = Person
    String    -- First name
    String    -- Last name
    String    -- Image
    [Email]   -- Email
    [Address] -- Address
  deriving (Eq,Show)

data Kind = Private | Work | Other
  deriving (Eq,Show)

data Email = Email Kind String
  deriving (Eq,Show)

data Address = Address
    Kind      -- Kind
    String    -- Street
    String    -- Code
    String    -- City
    String    -- Country
  deriving (Eq,Show)

myAddressBook :: AddressBook
myAddressBook =
  AddressBook "George"
  [ Person "George" "Karachalias"
      "https://people.cs.kuleuven.be/~george.karachalias/images/george_gent.jpg"
      [ Email Private "george.karachalias@gmail.com",
        Email Work    "george.karachalias@cs.kuleuven.be"
      ]
      [ Address Work
          "Celestijnenlaan 200A"
          "3001"
          "Leuven"
          "Belgium"
      ],
    Person "Tom" "Schrijvers"
      "https://people.cs.kuleuven.be/~tom.schrijvers/img/tom_schrijvers.png"
      [ Email Work "tom.schrijvers@cs.kuleuven.be" ]
      [ Address Work
          "Celestijnenlaan 200A"
          "3001"
          "Leuven"
          "Belgium"
      ]
  ]

-- Some HTML markup abbreviations.
br :: HtmlElement
br     =  HtmlTag "br" [] []

html, body, tr, td, h2, h3 :: HtmlElements -> HtmlElement
html   =  HtmlTag "html" []
body   =  HtmlTag "body" []
tr     =  HtmlTag "tr" []
td     =  HtmlTag "td" []
h2     =  HtmlTag "h2" []
h3     =  HtmlTag "h3" []

table :: [Attr] -> HtmlElements -> HtmlElement
table = HtmlTag "table"

border :: Int -> Attr
border = MkAttr "border" . show
width :: Int -> Attr
width = MkAttr "width"  . show

emptyRow :: HtmlElement
emptyRow = tr [ td [ br ] ]

-- HTML instances
instance HTML Kind where
  toHtml k = HtmlString $ show k

instance HTML Address where
  toHtml (Address kind street code city country) =
    tr [ td [ toHtml kind ],
         td [ HtmlString street , br,
              HtmlString (code ++ " " ++ city), br,
              HtmlString country]
       ]

instance HTML Email where
  toHtml (Email kind address) =
    tr [ td [ toHtml kind ],
         td [ toHtml (Link ("mailto:" ++ address) address) ]
       ]

instance HTML Person where
  toHtml (Person firstName lastName image emails addresses) =
    table [border 1, width 400] [ tr [ td [ innerTable ] ] ]
    where
      name :: HtmlElement
      name = h2 [ HtmlString (firstName ++ " " ++ lastName) ]
      img = HtmlTag "img" [MkAttr "src" image, width 80] []
      innerTable :: HtmlElement
      innerTable = table [] $
        [ tr [ td [ img ], td [ name] ],
          emptyRow,
          tr [ td [ h3 [ HtmlString "Email addresses:" ] ] ]
        ] ++
        map toHtml emails ++
        [ emptyRow,
          tr [ td [ h3 [ HtmlString "Addresses:" ] ] ]
        ] ++
        map toHtml addresses

instance HTML AddressBook where
  toHtml (AddressBook owner contacts) = html
    [ HtmlTag "head" [MkAttr "title" $ owner ++ "'s Address Book"] []
    , body $ map toHtml contacts
    ]


printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]

