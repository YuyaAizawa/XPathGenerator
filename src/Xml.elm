module Xml exposing
  ( Element
  , Attribute
  , Content(..)
  , parse
  , toString
  )

import Peg.Parser exposing (..)



type alias Element =
  { name : String
  , attributes : List Attribute
  , contents : List Content
  }

type alias Attribute =
  { name : String
  , value : String
  }

type Content
  = Child Element
  | Content String

parse : String -> Maybe Element
parse src =
  Peg.Parser.parse src parser



-- PARSER

parser = pElement

pDq =
  match "\""

pSp =
  char (charIn "\u{0020}\u{0009}\u{000D}\u{000A}")
    |> oneOrMore

pChar =
  char (always True)

pExclude str =
  pChar
    |> andThen (\c ->
      if charIn str c
      then fail
      else return c)

pCharData =
  zeroOrMore (pExclude "<&")
    |> map String.fromList
    |> andThen (\str ->
      if String.contains "]]>" str
      then fail
      else return str)

pName : Parser String
pName =
  seq2
  pNameStartChar
  (zeroOrMore pNameChar)
  (\hd tl -> String.fromList (hd :: tl) |> Debug.log "pName")

pNameStartChar =
  char (\c ->
    Char.isAlpha c ||
    Char.isDigit c ||
    charIn ":_-.\u{00B7}" c ||
    ('\u{00C0}'  <= c && c <= '\u{00D6}') ||
    ('\u{00D8}'  <= c && c <= '\u{00F6}'))

pNameChar =
  char (\c ->
    Char.isAlpha c ||
    c == ':' ||
    c == '_' ||
    ('\u{00C0}'  <= c && c <= '\u{00D6}') ||
    ('\u{00D8}'  <= c && c <= '\u{00F6}'))

pValue =
  seq3
  pDq
  (zeroOrMore (pExclude "<&\"")
    |> map String.fromList)
  pDq
  (\_ str _ -> str)

pAttribute =
  seq3
  pName
  (match "=")
  pValue
  (\name_ _ value -> { name = name_, value = value } |> Debug.log "pAttribute")

pSTag : Parser { name : String, attributes : List Attribute }
pSTag =
  seq5
  (match "<")
  pName
  (zeroOrMore (seq2 pSp pAttribute (\_ a -> a)))
  (option pSp)
  (match ">")
  (\_ name_ attributes_ _ _ -> { name = name_, attributes = attributes_ } |> Debug.log "pSTag")

pETag name_ =
  seq4
  (match "</")
  (match name_)
  (option pSp)
  (match ">")
  (\_ _ _ _ -> name_ |> Debug.log "pETag")

pContent : Parser (List Content)
pContent =
  choice
  [ \() -> (pElement |> map Child)
  , \() -> (pCharData |> andThen (\str -> if str == "" then fail else return str) |> map Content)
  ]
    |> zeroOrMore

pEmptyElementTag : Parser Element
pEmptyElementTag =
  seq5
  (match "<")
  pName
  (zeroOrMore
    (seq2
      pSp
      pAttribute
      (\_ a -> a)
    )
  )
  (option pSp)
  (match "/>")
  (\_ name_ attributes_ _ _ -> { name = name_, attributes = attributes_, contents = [] })

pElement : Parser Element
pElement =
  choice
  [ \_ ->
    pEmptyElementTag
  ,  \_ ->
    pSTag
      |> andThen (\tag ->
        seq2
        pContent (pETag tag.name)
        (\contents_ _ -> { name = tag.name, attributes = tag.attributes, contents = contents_ }))
  ]

charIn : String -> Char -> Bool
charIn str c =
  str
    |> String.toList
    |> List.member c



attributeToString this =
  this.name++" = \""++this.value++"\""

attributesToString attrs =
  "["++(attrs |> List.map attributeToString |> String.join ", ")++"]"

sp i =
  List.repeat i "  "
    |> String.concat

contentsToString indent contents_ =
  List.foldl (\content str -> str ++ "\n" ++ contentToString indent content) "" contents_


contentToString indent content =
  case content of
    Child element ->
      toString indent element

    Content str ->
      sp indent ++ str

toString : Int -> Element -> String
toString indent element =
  sp indent ++ element.name ++ " " ++ attributesToString element.attributes ++ "\n" ++ contentsToString (indent+1) element.contents


