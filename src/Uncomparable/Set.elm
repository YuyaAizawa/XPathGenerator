module Uncomparable.Set exposing
  ( Set
  , empty, singleton, insert, remove
  , isEmpty, member, size, eq
  , union, intersect, diff
  , toList, fromList
  , map, foldl, foldr, filter, partition
  )

import Dict exposing (Dict)

type Set e comparable = Set (KeyExtractor e comparable) (Dict comparable e)
type alias KeyExtractor e comparable = e -> comparable



empty: KeyExtractor e comparable -> Set e comparable
empty keyExtractor =
  Set keyExtractor (Dict.empty)


singleton: KeyExtractor e comparable -> e -> Set e comparable
singleton keyExtractor e =
  let
    key = keyExtractor e
  in
    Set keyExtractor (Dict.singleton key e)


insert: e -> Set e comparable -> Set e comparable
insert e (Set keyExtractor dict) =
  let
    key =
      keyExtractor e

    dict_ =
      Dict.insert key e dict
  in
    Set keyExtractor dict_


remove: e -> Set e comparable -> Set e comparable
remove e (Set keyExtractor dict) =
  let
    key =
      keyExtractor e

    dict_ =
      Dict.insert key e dict
  in
    Set keyExtractor dict_

isEmpty: Set e comparable -> Bool
isEmpty (Set keyExtractor dict) =
  Dict.isEmpty dict


member: e -> Set e comparable -> Bool
member e (Set keyExtractor dict) =
  let
    key =
      keyExtractor e
  in
    Dict.member key dict


size: Set e comparable -> Int
size (Set keyExtractor dict) =
  Dict.size dict


eq: Set e comparable -> Set e comparable -> Bool
eq (Set lKeyExtractor lDict) (Set rKeyExtractor rDict) =
  Dict.keys lDict == Dict.keys rDict


union: Set e comparable -> Set e comparable2 -> Set e comparable
union (Set lKeyExtractor lDict) (Set rKeyExtractor rDict) =
  let
    rDict_ =
      rDict
        |> Dict.values
        |> List.map (\v -> ( lKeyExtractor v, v ))
        |> Dict.fromList
    dict =
      Dict.union lDict rDict_
  in
    Set lKeyExtractor dict


intersect: Set e comparable -> Set e comparable2 -> Set e comparable
intersect (Set lKeyExtractor lDict) (Set rKeyExtractor rDict) =
  let
    rDict_ =
      rDict
        |> Dict.values
        |> List.map (\v -> ( lKeyExtractor v, v ))
        |> Dict.fromList
    dict =
      Dict.intersect lDict rDict_
  in
    Set lKeyExtractor dict


diff: Set e comparable -> Set e comparable2 -> Set e comparable
diff (Set lKeyExtractor lDict) (Set rKeyExtractor rDict) =
  let
    rDict_ =
      rDict
        |> Dict.values
        |> List.map (\v -> ( lKeyExtractor v, v ))
        |> Dict.fromList
    dict =
      Dict.diff lDict rDict_
  in
    Set lKeyExtractor dict


toList: Set e comparable -> List e
toList (Set keyExtractor dict) =
  Dict.values dict


fromList: KeyExtractor e comparable -> List e -> Set e comparable
fromList keyExtractor list =
  let
    dict =
      list
        |> List.map (\v -> ( keyExtractor v, v ))
        |> Dict.fromList
  in
    Set keyExtractor dict

map: ( e -> e2 ) -> (KeyExtractor e2 comparable2) -> (Set e comparable) -> (Set e2 comparable2)
map mapper keyExtractor2 (Set keyExtractor dict) =
  let
    dict2 =
      dict
        |> Dict.values
        |> List.map (\v -> mapper v)
        |> List.map (\v -> ( keyExtractor2 v, v ))
        |> Dict.fromList
  in
    Set keyExtractor2 dict2


foldl: (a -> b -> b) -> b -> Set a comparable -> b
foldl f init (Set keyExtractor dict) =
  Dict.foldl (always f) init dict


foldr: (a -> b -> b) -> b -> Set a comparable -> b
foldr f init (Set keyExtractor dict) =
  Dict.foldr (always f) init dict

filter: (e -> Bool) -> Set e comparable -> Set e comparable
filter f (Set keyExtractor dict) =
  let
    dict_ =
      Dict.filter (always f) dict
  in
    Set keyExtractor dict_

partition: (e -> Bool) -> Set e comparable -> ( Set e comparable, Set e comparable )
partition f (Set keyExtractor dict) =
  let
    ( lDict, rDict ) =
      Dict.partition (always f) dict
  in
    ( Set keyExtractor lDict, Set keyExtractor rDict )
