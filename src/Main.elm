module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, button, textarea, text, br ,pre ,span, select, h2, h3, ul, li)
import Html.Attributes exposing (placeholder, class)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode
import Peg.Parser exposing (..)
import Set exposing (Set)


-- MODEL

type alias Model =
    { tree : Maybe TreeNode
    , selectedPath : Maybe XPath
    , candidates : Set XPath
    , highlight : Maybe XPath
    , positives : List XPath
    , negatives : List XPath
    , highlightId : Set String
    , dicts : Dicts
    }

type alias Dicts =
    { childName : Dict ( String, String ) (List String) -- (id, name) ids
    , childStar : Dict String (List String) -- id ids
    , desendantOrSelfNode : Dict String (List String) -- id ids
    }

initialModel : Model
initialModel =
    { tree = Nothing
    , selectedPath = Nothing
    , candidates = Set.empty
    , highlight = Nothing
    , positives = []
    , negatives = []
    , highlightId = Set.empty
    , dicts = (
        let
            lhs = 
                [ ("descendantOrSelf", "node()", "")
                , ("child", "C", "")
                , ("child", "B", "")
                , ("child", "A", "")
                ]
            rhs =
                [ ("child", "E", "")
                , ("child", "D", "")
                , ("child", "C", "")
                , ("child", "B", "P")
                , ("child", "A", "")
                ]
            a = matchPath
                lhs
                rhs
                    |> Debug.log ("relation "++ xpathToString lhs ++ ", " ++ xpathToString rhs)
          in
              emptyDicts)
    }

emptyDicts =
    { childName = Dict.empty
    , childStar = Dict.empty
    , desendantOrSelfNode = Dict.empty
    }



-- HTML

type Element = Element
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



-- TreeNode

type TreeNode = TreeNode
    { id : String
    , path : XPath
    , name : String
    , attributes : List Attribute
    , contents : List NodeContent
    }

type NodeContent
    = Branch TreeNode
    | Leaf String

fold : (TreeNode -> b -> b) -> b -> TreeNode -> b
fold f init tree =
    let
        (TreeNode { contents }) = tree
        rest : List TreeNode
        rest =
            contents
                |> List.concatMap (\content -> case content of
                    Branch t -> [ t ]
                    Leaf _ -> [])
    in
        List.foldl
        (\a b -> fold f b a)
        (f tree init)
        rest

elementToTree : String -> (List String) -> Element -> TreeNode
elementToTree id location (Element { name, attributes, contents }) =
    let
        location_ =
            name :: location

        contents_ =
            contents
                |> List.indexedMap (\i c ->
                    case c of
                        Child element_ ->
                            Branch <|
                                elementToTree
                                (id++"/"++String.fromInt i)
                                location_
                                element_

                        Content string ->
                            Leaf string)
    in
        TreeNode
        { id = id
        , path = xPathFromLocation <| List.reverse location_
        , name = name
        , attributes = attributes
        , contents = contents_
        }




-- XPath

type alias XPath =
    List LocationStep -- must be comparable

type alias LocationStep =
    ( Axis, NodeTest, Predicate )

type alias Axis = String
type alias NodeTest = String
type alias Predicate = String

getNodeTest ( axis, nodeTest, predicate ) = nodeTest

xpathToString : XPath -> String
xpathToString xpath =
    xpath
        |> List.map locationStepToString
        |> String.join "/"
        |> String.append "/"

locationStepToString ( axis, nodeTest, predicate ) =
    let
        predicateStr =
            case predicate of
                "" -> ""
                some -> "["++some++"]"
    in
        case ( axis, nodeTest, predicate ) of
                ( "child", _, _ ) ->
                    nodeTest++predicateStr

                ( "descendantOrSelf", "node()", "" ) ->
                    ""

                _ ->
                    axis++"::"++nodeTest++predicateStr


eval : Dicts -> XPath -> Set String
eval { childName, childStar, desendantOrSelfNode } xpath  =
    let

        childNodeName name id =
            childName
                |> Dict.get ( id, name )
                |> Maybe.withDefault []

        help currentRoots locationStepList_ =
            let
                a = currentRoots |> Debug.log "root"
                b = locationStepList_ |> Debug.log "path"
                c = childName |> Debug.log "dict"

                mapper =
                    case locationStepList_ of
                        [] ->
                            (\node -> [])

                        ( "descendantOrSelf", "node()", _ ) :: _ ->
                            (\node ->
                                desendantOrSelfNode
                                    |> Dict.get node
                                    |> Maybe.withDefault [])

                        ( "child", "*", _ ) :: _ ->
                            (\node ->
                                childStar
                                    |> Dict.get node
                                    |> Maybe.withDefault [])

                        ( "child", nodeTest, _ ) :: _ ->
                            (\node ->
                                childName
                                    |> Dict.get ( node, nodeTest )
                                    |> Maybe.withDefault [])

                        step :: _ ->
                            Debug.todo <| locationStepToString step

                nextRoots =
                    currentRoots
                        |> Set.toList
                        |> List.concatMap mapper
                        |> Set.fromList

            in
                case locationStepList_ of
                    [] ->
                        currentRoots

                    _ :: rest ->
                        help nextRoots rest
    in
        help (Set.fromList ["root"]) xpath

notParent : NodeTest -> LocationStep -> LocationStep
notParent target ( axis, nodeTest, predicate ) =
    ( axis, nodeTest, predicate ++ " and not(./parent::" ++ target ++ ")" )

last hd tl =
    case tl of
        [] -> hd
        hd_ :: tl_ -> last hd_ tl_

union : XPath -> XPath -> Set XPath
union lhs rhs =  -- todo: dp
    let 
        prependSet : LocationStep -> Set XPath -> Set XPath
        prependSet l =
            Set.map (\r -> prepend l r)

        dslash = ( "descendantOrSelf", "node()", "" )
        star = ( "child", "*", "" )

        prepend : LocationStep -> XPath -> XPath
        prepend l r =
            case ( l, r ) of
                ( ("descendantOrSelf", "node()", ""),  ("descendantOrSelf", "node()", "") :: x ) ->
                    dslash :: x

                ( ("desecndantOrSelf", "node()", ""),  x ) ->
                    dslash :: x

                ( a, ("descendantOrSelf", "node()", "") :: x ) ->
                    a :: dslash :: x

                ( a, x ) ->
                    a :: x

        help : XPath -> XPath -> Set XPath
        help l r =
            case ( l, r ) of
                ( a :: [], b :: [] ) ->
                    if a == b
                    then a :: [] |> Set.singleton
                    else star :: [] |> Set.singleton

                ( a :: [], hd :: tl ) ->
                    if last hd tl == a
                    then dslash :: a :: [] |> Set.singleton
                    else dslash :: star :: [] |> Set.singleton

                ( hd :: tl , b :: [] ) ->
                    if last hd tl == b
                    then dslash :: b :: [] |> Set.singleton
                    else dslash :: star :: [] |> Set.singleton

                ( a :: x, b :: y ) ->
                    let
                        t1 = prependSet dslash (help (a :: x) y)
                        t2 = prependSet dslash (help x (b :: y))
                        t : Set XPath
                        t = Set.union t1 t2
                    in
                        if a == b
                        then Set.union t (prependSet a (help x y))
                        else t

                _ -> Debug.todo "never happen"
    in
        help lhs rhs

difference : XPath -> XPath -> Set XPath
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
                                        [ step :: notParent ((last stepHd stepTl) |> getNodeTest) target :: rest
                                        , step :: target :: rest
                                        ]

                                    _ ->
                                        []
                                )
                    in
                        notParentHelp tl result_

    in
        case matching |> Maybe.map List.reverse of
            Just ((step,_)::tl) ->
                notParentHelp tl [[step]]
                    |> Set.fromList

            _ ->
                Set.empty


matchPath : XPath -> XPath -> Maybe (List ( LocationStep, List LocationStep ))
matchPath lhs rhs =
    let
        contains ( lAxis, lNodeTest, lPredicate ) ( _, rNodeTest, _ ) =
            if lAxis == "child" && lNodeTest == "*"
            then True -- todo think about predicate
            else lNodeTest == rNodeTest

        help : List LocationStep -> List LocationStep -> List LocationStep -> List ( LocationStep, List LocationStep ) -> Maybe (List ( LocationStep, List LocationStep ))
        help lStepReversed rStepReversed commpressed relation =
            case ( lStepReversed, rStepReversed, commpressed ) of
                ( [], [], [] ) ->
                    Just relation

                ( [], _, _ ) ->
                    Nothing

                ( lhd :: ltl, [], _ ) ->
                    if lhd == ( "descendantOrSelf", "node()", "" )
                    then help ltl [] [] ((lhd, List.reverse commpressed) :: relation)
                    else Nothing

                ( lhd :: ltl, rhd :: rtl, _ ) ->
                    if lhd == ( "descendantOrSelf", "node()", "" )
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
        (List.reverse lhs)
        (List.reverse rhs)
        []
        []


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
            let
                maybeTree =
                    parser
                        |> parse str
                        |> Maybe.map (elementToTree "" [])

                childName =
                    maybeTree
                        |> Maybe.map extractChildName
                        |> Maybe.withDefault Dict.empty

                childStar =
                    maybeTree
                        |> Maybe.map extractChildStar
                        |> Maybe.withDefault Dict.empty

                desendantOrSelfNode =
                    maybeTree
                        |> Maybe.map extractDesendantOrSelfNode
                        |> Maybe.withDefault Dict.empty
            in
                { model
                | tree = maybeTree
                , dicts =
                    { childName = childName
                    , childStar = childStar
                    , desendantOrSelfNode = desendantOrSelfNode
                    }
                }

        SelectNode path ->
            let a = path |> xpathToString |> Debug.log "selectNode" in
            { model
            | selectedPath = Just path
            }

        SelectCandidate path ->
            { model
            | highlight = Just path
            , highlightId = eval model.dicts path
            }

        AddPositive path ->
            let
                candidates =
                    if Set.isEmpty model.candidates
                    then Set.singleton path
                    else model.candidates
                        |> Set.foldl (\c s -> Set.union (union path c) s) Set.empty
            in
                { model
                | positives = path :: model.positives
                , candidates = candidates
                }

        AddNegative path ->
            let
                candidates =
                    if Set.isEmpty model.candidates
                    then Set.empty
                    else model.candidates
                        |> Set.foldl (\c s -> Set.union (difference path c) s) Set.empty
            in
                { model
                | negatives = path :: model.negatives
                , candidates = candidates
                }


extractChildName : TreeNode -> Dict ( String, String ) (List String)
extractChildName root =
    fold
    (\(TreeNode parent) b ->
        parent.contents
            |> List.foldl (\n b_->
                case n of
                    Branch (TreeNode child) ->
                        Dict.update
                        ( parent.id, child.name )
                        ( \m -> case m of
                            Just list -> Just (child.id :: list)
                            Nothing -> Just [ child.id ]
                        )
                        b_
                    Leaf _ -> b_)
                b
    )
    (Dict.fromList [ ( ( "root", case root of TreeNode r -> r.name ), [""] ) ])
    root
    
extractChildStar root =
    fold
    (\(TreeNode parent) b ->
        parent.contents
            |> List.foldl (\n b_->
                case n of
                    Branch (TreeNode child) ->
                        Dict.update
                        parent.id
                        ( \m -> case m of
                            Just list -> Just (child.id :: list)
                            Nothing -> Just [ child.id ]
                        )
                        b_
                    Leaf _ -> b_)
                b
    )
    (Dict.fromList [ ( ( "root" ), [""] ) ])
    root

extractDesendantOrSelfNode root =
    let
        getDesendant : List String -> List TreeNode -> List String
        getDesendant result nextChildren =
            case nextChildren of
                [] ->
                    result
                _ ->
                    getDesendant
                    (nextChildren
                        |> List.map (\(TreeNode tree) -> tree.id)
                        |> (++) result
                    )
                    (nextChildren |> List.concatMap (\(TreeNode parent) ->
                        parent.contents
                            |> List.concatMap (\content ->
                                case content of
                                    Branch child ->
                                        [child]
                                    Leaf _ ->
                                        [])))
        
    in
        fold
        (\(TreeNode parent) dict ->
            Dict.insert parent.id (getDesendant [] [(TreeNode parent)]) dict
        )
        (Dict.fromList [ ( ( "root" ), [""] ) ])
        root
            

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
    (\name _ value -> { name = name, value = value } |> Debug.log "pAttribute")

pSTag : Parser { name : String, attributes : List Attribute }
pSTag =
    seq5
    (match "<")
    pName
    (zeroOrMore (seq2 pSp pAttribute (\_ a -> a)))
    (option pSp)
    (match ">")
    (\_ name attributes _ _ -> { name = name, attributes = attributes } |> Debug.log "pSTag")

pETag name =
    seq4
    (match "</")
    (match name)
    (option pSp)
    (match ">")
    (\_ _ _ _ -> name |> Debug.log "pETag")

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
    (\_ name attributes _ _ -> Element { name = name, attributes = attributes, contents = [] })

pElement : Parser Element
pElement =
    choice
    [ \_ ->
        pEmptyElementTag
    ,  \_ ->
        pSTag
            |> andThen (\{ name, attributes } ->
                seq2
                pContent (pETag name)
                (\contents _ -> Element { name = name, attributes = attributes, contents = contents }))
    ]

charIn : String -> Char -> Bool
charIn str c =
    str
        |> String.toList
        |> List.member c



attributeToString { name, value } =
    name++" = \""++value++"\""

attributesToString attrs =
    "["++(attrs |> List.map attributeToString |> String.join ", ")++"]"

sp i =
    List.repeat i "  "
        |> String.concat

contentsToString indent contents =
    List.foldl (\content str -> str ++ "\n" ++ contentToString indent content) "" contents


contentToString indent content =
    case content of
        Child element ->
            elementToString indent element

        Content str ->
            sp indent ++ str

elementToString : Int -> Element -> String
elementToString indent element =
    case element of
        Element { name, attributes, contents } ->
            sp indent ++ name ++ " " ++ attributesToString attributes ++ "\n" ++ contentsToString (indent+1) contents



-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Source" ], br [] []
        , textarea [ placeholder "iniput xml", onInput Input ] [], br [] []
        
        , h2 [] [ text "Selected Node"], br [] []
        , text <| "XPath: " ++ (model.selectedPath |> Maybe.map xpathToString |> Maybe.withDefault ""), br [] []
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
                |> List.map (xpathToString >> text >> List.singleton >> li []))
        , br [] []
        , h3 [] [ text "Negative" ], br [] []
        , ul []
            (model.negatives
                |> List.map (xpathToString >> text >> List.singleton >> li []))
        , br [] []
        
        , h2 [] [ text "Candidates" ], br [] []
        , text <| "Highlight: " ++ (model.highlight |> Maybe.map xpathToString |> Maybe.withDefault ""), br [] []
        , select []
            <| (model.candidates
                |> Set.toList
                |> List.map (\c -> Html.option [ onClick <| SelectCandidate c ] [ text <| xpathToString c ])), br [] []
        , (case model.tree of
                Nothing -> br [] []
                Just e -> nodeView model.highlightId e)
        ]

nodeView : Set String -> TreeNode -> Html Msg
nodeView highlightId (TreeNode { id, path, contents }) =
    div
    [ class
        ("node"++
            if Set.member id highlightId
            then " highlight"
            else "")
    , onClickNoBubble <| SelectNode <| path
    ]
    (contents
        |> List.map (\c -> case c of
            Branch node
                -> nodeView highlightId node
            Leaf str
                -> text str))

xPathFromLocation : List String -> XPath
xPathFromLocation location =
    location
        |> List.map (\name -> ( "child", name, "" ))

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
