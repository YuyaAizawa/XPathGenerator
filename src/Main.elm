module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, button, textarea, text, br ,pre ,span, select, h2, h3, ul, li)
import Html.Attributes exposing (placeholder, class)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode
import Set exposing (Set)
import Xml
import XPath exposing (..)
import XPathSet exposing (XPathSet)



-- MODEL

type alias Model =
  { xml : Maybe Xml.Element
  , selectedPath : Maybe XPath
  , candidates : XPathSet
  , highlight : Maybe XPath
  , positives : List XPath
  , negatives : List XPath
  , highlightId : Set (List Int)
  }

initialModel : Model
initialModel =
  { xml = Nothing
  , selectedPath = Nothing
  , candidates = XPathSet.empty
  , highlight = Nothing
  , positives = []
  , negatives = []
  , highlightId = Set.empty
  }



-- UPDATE

type Msg
  = Input String
  | SelectNode XPath
  | SelectCandidate XPath
  | AddPositive XPath
  | AddNegative XPath

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input str ->
      { model | xml = Xml.parse str }

    SelectNode path ->
      let a = path |> XPath.toString |> Debug.log "selectNode" in
      { model
      | selectedPath = Just path
      }

    SelectCandidate path ->
      { model
      | highlight =
          Just path
      , highlightId =
          model.xml
            |> Maybe.map (\xml -> eval xml path |> Debug.log "condidates")
            |> Maybe.withDefault Set.empty
      }

    AddPositive path ->
      let
        candidates =
          if XPathSet.isEmpty model.candidates
          then XPathSet.singleton path
          else model.candidates
            |> XPathSet.foldl (\c s -> XPathSet.union (union path c) s) XPathSet.empty
      in
        { model
        | positives = path :: model.positives
        , candidates = candidates
        }

    AddNegative path ->
      let
        candidates =
          if XPathSet.isEmpty model.candidates
          then XPathSet.empty
          else model.candidates
            |> XPathSet.foldl (\c s -> XPathSet.union (difference path c) s) XPathSet.empty
      in
        { model
        | negatives = path :: model.negatives
        , candidates = candidates
        }


notParent : NodeTest -> LocationStep -> LocationStep
notParent target locationStep =
  { locationStep | predicate =
    locationStep.predicate ++ " and not(./parent::" ++ target ++ ")"
  }

last hd tl =
  case tl of
    [] -> hd
    hd_ :: tl_ -> last hd_ tl_

union : XPath -> XPath -> XPathSet
union lhs rhs =  -- todo: dp
  let
    prependSet : LocationStep -> XPathSet -> XPathSet
    prependSet l =
      XPathSet.map (\r -> prepend l r)

    prepend : LocationStep -> XPath -> XPath
    prepend l r =
      let
        rSteps = XPath.toLocationSteps r
        locationSteps =
          if l == XPath.dslash
          then
            case rSteps of
              hd :: x ->
                if hd == XPath.dslash
                then XPath.dslash :: x
                else dslash :: x

              _ -> rSteps
          else
            case rSteps of
              hd :: x ->
                if hd == XPath.dslash
                then l :: XPath.dslash :: x
                else l :: rSteps

              _ ->
                l :: rSteps
      in XPath.fromLocationSteps locationSteps

    help : List LocationStep -> List LocationStep -> XPathSet
    help l r =
      case ( l, r ) of
        ( a :: [], b :: [] ) ->
          if a == b
          then a :: [] |> XPath.fromLocationSteps |> XPathSet.singleton
          else star :: [] |> XPath.fromLocationSteps |> XPathSet.singleton

        ( a :: [], hd :: tl ) ->
          if last hd tl == a
          then dslash :: a :: [] |> XPath.fromLocationSteps |> XPathSet.singleton
          else dslash :: star :: [] |> XPath.fromLocationSteps |> XPathSet.singleton

        ( hd :: tl , b :: [] ) ->
          if last hd tl == b
          then dslash :: b :: [] |> XPath.fromLocationSteps |> XPathSet.singleton
          else dslash :: star :: [] |> XPath.fromLocationSteps |> XPathSet.singleton

        ( a :: x, b :: y ) ->
          let
            t1 = prependSet dslash (help (a :: x) y)
            t2 = prependSet dslash (help x (b :: y))
            t : XPathSet
            t = XPathSet.union t1 t2
          in
            if a == b
            then XPathSet.union t (prependSet a (help x y))
            else t

        _ -> Debug.todo "never happen"
  in
    help (XPath.toLocationSteps lhs) (XPath.toLocationSteps rhs)

difference : XPath -> XPath -> XPathSet
difference original exclude =
  let
    matching =
      matchPath original exclude

    notParentHelp : List ( LocationStep, List LocationStep ) -> List (List LocationStep) -> List (List LocationStep)
    notParentHelp ancestor result =
      case ancestor of
        [] ->
          result

        ( step, [] ) :: tl ->
          notParentHelp tl (result |> List.map (\r -> step::r))

        ( step, stepHd::stepTl ) :: tl -> -- for //
          let
            result_ =
              result
                |> List.concatMap (\r -> case r of
                  target::rest ->
                    [ step :: notParent (last stepHd stepTl).nodeTest target :: rest
                    , step :: target :: rest
                    ]

                  _ ->
                    [])
          in
            notParentHelp tl result_
  in
    case matching |> Maybe.map List.reverse of
      Just ((step,_)::tl) ->
        notParentHelp tl [[step]]
          |> List.map XPath.fromLocationSteps
          |> XPathSet.fromList

      _ ->
        XPathSet.empty


matchPath : XPath -> XPath -> Maybe (List ( LocationStep, List LocationStep ))
matchPath lhs rhs =
  let
    contains lStep rStep =
      if lStep.axis == XPath.Child && lStep.nodeTest == "*"
      then True -- todo think about predicate
      else lStep.nodeTest == rStep.nodeTest

    help : List LocationStep -> List LocationStep -> List LocationStep -> List ( LocationStep, List LocationStep ) -> Maybe (List ( LocationStep, List LocationStep ))
    help lStepReversed rStepReversed commpressed relation =
      case ( lStepReversed, rStepReversed, commpressed ) of
        ( [], [], [] ) ->
          Just relation

        ( [], _, _ ) ->
          Nothing

        ( lhd :: ltl, [], _ ) ->
          if lhd == XPath.dslash
          then help ltl [] [] ((lhd, List.reverse commpressed) :: relation)
          else Nothing

        ( lhd :: ltl, rhd :: rtl, _ ) ->
          if lhd == XPath.dslash
          then case help lStepReversed rtl ( rhd :: commpressed ) relation of
             Nothing ->
               help ltl rStepReversed [] (( lhd, List.reverse commpressed ) :: relation)
             Just result ->
               Just result
          else
            if contains lhd rhd
            then help ltl rtl [] (( lhd, List.reverse <| rhd :: commpressed ) :: relation)
            else Nothing
  in
    help
    (lhs |> XPath.toLocationSteps |> List.reverse)
    (rhs |> XPath.toLocationSteps |> List.reverse)
    []
    []



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Source" ], br [] []
    , textarea [ placeholder "iniput xml", onInput Input ] [], br [] []

    , h2 [] [ text "Selected Node"], br [] []
    , text <| "XPath: " ++ (model.selectedPath |> Maybe.map XPath.toString |> Maybe.withDefault ""), br [] []
    , button
      (model.selectedPath
        |> Maybe.map (AddPositive >> onClick >> List.singleton)
        |> Maybe.withDefault [])
      [ text "add positive" ]
    , button
      (model.selectedPath
        |> Maybe.map (AddNegative >> onClick >> List.singleton)
        |> Maybe.withDefault [])
      [ text "add negative" ], br [] []

    , h2 [] [ text "Samples" ], br [] []
    , h3 [] [ text "Positive" ], br [] []
    , ul []
      (model.positives
        |> List.map (XPath.toString >> text >> List.singleton >> li []))
    , br [] []
    , h3 [] [ text "Negative" ], br [] []
    , ul []
      (model.negatives
        |> List.map (XPath.toString >> text >> List.singleton >> li []))
    , br [] []

    , h2 [] [ text "Candidates" ], br [] []
    , text <| "Highlight: " ++ (model.highlight |> Maybe.map XPath.toString |> Maybe.withDefault ""), br [] []
    , select []
      <| (model.candidates
        |> XPathSet.toList
        |> List.map (\c -> Html.option [ onClick <| SelectCandidate c ] [ text <| XPath.toString c ])), br [] []
    , (case model.xml of
        Nothing -> br [] []
        Just element -> nodeView [ 0 ] [] model.highlightId element)
    ]

nodeView : List Int -> List String -> Set (List Int) -> Xml.Element -> Html Msg
nodeView id parentPath highlightId element =
  let
    isHighlight =
      Set.member (List.reverse id) highlightId

    thisNamePath =
      element.name :: parentPath

    contents =
      element.contents
        |> List.indexedMap (\i c ->
          case c of
            Xml.Content str ->
              text str

            Xml.Child e ->
              nodeView
              (i :: id)
              thisNamePath
              highlightId
              e
        )
  in
    div
    [ class
      ("node"++
        if isHighlight
        then " highlight"
        else "")
    , onClickNoBubble
        <| SelectNode
        <| XPath.fromNamePath
        <| thisNamePath
    ]
    contents

onClickNoBubble message =
  Html.Events.custom
  "click"
  (Decode.succeed { message = message, stopPropagation = True, preventDefault = True })

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
