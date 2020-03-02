module XPathSet exposing
  ( XPathSet
  , empty
  , isEmpty
  , singleton
  , toList
  , fromList
  , map
  , foldl
  , union
  )

import Uncomparable.Set as USet
import XPath exposing (XPath)

type XPathSet =
  XPathSet (USet.Set XPath String)

keyExtractor =
  XPath.toString

empty : XPathSet
empty =
  USet.empty keyExtractor
    |> XPathSet

isEmpty : XPathSet -> Bool
isEmpty (XPathSet set) =
  USet.isEmpty set

singleton : XPath -> XPathSet
singleton xpath =
  xpath
    |> USet.singleton keyExtractor
    |> XPathSet


member : XPath -> XPathSet -> Bool
member xpath (XPathSet set) =
  USet.member xpath set

toList : XPathSet -> List XPath
toList (XPathSet set) =
  USet.toList set

fromList : List XPath -> XPathSet
fromList list =
  XPathSet (USet.fromList keyExtractor list)

map : (XPath -> XPath) -> XPathSet -> XPathSet
map f (XPathSet set) =
  set
    |> USet.map f keyExtractor
    |> XPathSet

foldl : (XPath -> a -> a) -> a -> XPathSet -> a
foldl f init (XPathSet set) =
  USet.foldl f init set

union : XPathSet -> XPathSet -> XPathSet
union (XPathSet l) (XPathSet r) =
  XPathSet (USet.union l r)