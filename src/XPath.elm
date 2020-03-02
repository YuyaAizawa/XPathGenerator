module XPath exposing
  ( XPath
  , LocationStep
  , Axis(..)
  , NodeTest
  , Predicate
  , dslash
  , star
  , fromLocationSteps
  , fromNamePath
  , toLocationSteps
  , toString
  , eval
  )

import Dict exposing (Dict)
import Set exposing (Set)
import Xml



type XPath = XPath (List LocationStep)

type alias LocationStep =
  { axis : Axis
  , nodeTest : NodeTest
  , predicate : Predicate
  }

type Axis
  = Child
  | DescendantOrSelf

type alias NodeTest = String
type alias Predicate = String

fromLocationSteps : List LocationStep -> XPath
fromLocationSteps steps =
  XPath steps

fromNamePath : List String -> XPath
fromNamePath =
  List.map (\name ->
    { axis = Child
    , nodeTest = name
    , predicate = ""
    })
    >> List.reverse
    >> fromLocationSteps

toLocationSteps : XPath -> List LocationStep
toLocationSteps (XPath steps) =
  steps

toString : XPath -> String
toString xpath =
  xpath
    |> toLocationSteps
    |> List.map locationStepToString
    |> String.join "/"
    |> String.append "/"

locationStepToString { axis, nodeTest, predicate } =
  let
    predicateStr =
      case predicate of
        "" -> ""
        some -> "["++some++"]"
  in
    case ( axis, nodeTest, predicate ) of
        ( Child, _, _ ) ->
          nodeTest++predicateStr

        ( DescendantOrSelf, "node()", "" ) ->
          ""

        _ ->
          axisToString axis++"::"++nodeTest++predicateStr

axisToString axis =
  case axis of
    Child -> "child"
    DescendantOrSelf -> "descendantOrSelf"

dslash =
  { axis = DescendantOrSelf
  , nodeTest = "node()"
  , predicate = ""
  }

star =
  { axis = Child
  , nodeTest = "*"
  , predicate = ""
  }

eval : Xml.Element -> XPath -> Set (List Int)
eval root xpath =
  let
    help : List LocationStep -> List ( Xml.Content, List Int ) -> Set (List Int)
    help restPath candidateNodes =
      case restPath of
        [] ->
          candidateNodes
            |> List.map (\( _, path ) ->
              List.reverse path)
            |> Set.fromList

        { axis, nodeTest, predicate } :: tl ->
          let
            nextCandidates =
              case ( axis, nodeTest ) of
                ( DescendantOrSelf, "node()" ) ->
                  candidateNodes
                    |> List.concatMap descendantOrSelf

                ( Child, "*" ) ->
                  candidateNodes
                    |> List.concatMap allChildren

                ( Child, _ ) ->
                  candidateNodes
                    |> List.concatMap allChildren
                    |> List.filter (nodeTestFilter nodeTest)

                _ ->
                  Debug.todo
                    <| "("++axisToString axis++", "++nodeTest++")"
          in
            help tl nextCandidates

    allChildren ( content, path ) =
      case content of
        Xml.Content _ ->
          []

        Xml.Child element ->
          element.contents
            |> List.indexedMap (\i c ->
              ( c, i :: path ))

    descendantOrSelf target =
      let
        help2 result rest =
          case rest of
            [] ->
              result

            hd :: tl ->
              let
                next =
                  allChildren hd
              in
                help2 (result ++ next) (next ++ tl)
      in
        help2 [ target ] [ target ]

    nodeTestFilter nodeTest =
      (\( content, _ ) ->
        case content of
          Xml.Content _ ->
            False

          Xml.Child element ->
            element.name == nodeTest
      )
  in
    help
    (toLocationSteps xpath)
    [ ( Xml.Child
        { name = "root"
        , attributes = []
        , contents = [ Xml.Child root ]
        }
      , []
      ) ]

