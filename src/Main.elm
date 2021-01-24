module Main exposing (..)

import Array exposing (Array)
import Browser
import Hotkeys exposing (onEnter)
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)
import List.Extra
import Random
import Random.Array



---- MODEL ----


type alias Model =
    { questionAnswer : ThreeNumber
    , playerAnswer : Maybe ThreeNumber
    , playerAnswerString : String
    , compareResult : Maybe ThreeNumberCompareResult
    , error : Maybe String
    }


type ThreeNumber
    = ThreeNumber Int Int Int


getFromIntArray : Int -> Array Int -> Int
getFromIntArray index array =
    -- 本当は例外処理か絶対数字があることを保証する型を書かないといけない
    Maybe.withDefault -1 (Array.get index array)


{-| 3つの要素が確実に存在するarrayを渡さないといけない
-}
arrayToThreeNumber : Array Int -> ThreeNumber
arrayToThreeNumber array =
    ThreeNumber (getFromIntArray 0 array) (getFromIntArray 1 array) (getFromIntArray 2 array)


randomThreeNumber : Random.Generator ThreeNumber
randomThreeNumber =
    Random.Array.shuffle (Array.fromList (List.range 0 9))
        |> Random.map arrayToThreeNumber


generateRandomThreeNumber : Cmd Msg
generateRandomThreeNumber =
    Random.generate GenerateRandomThreeNumber randomThreeNumber


transformStringToThreeNumber : String -> Result String ThreeNumber
transformStringToThreeNumber threeNumberString =
    validateThreeDigitNumberString threeNumberString
        |> Result.andThen validateThreeNumberAllDifferent
        |> Result.map transformValidIntListToThreeNumber


{-| 数字が3つ確実に存在するlistを渡さないといけない
-}
transformValidIntListToThreeNumber : List Int -> ThreeNumber
transformValidIntListToThreeNumber list =
    Array.fromList list
        |> arrayToThreeNumber


validateThreeDigitNumberString : String -> Result String (List Int)
validateThreeDigitNumberString string =
    let
        charList =
            String.toList string
    in
    if List.length charList == 3 && List.all Char.isDigit charList then
        Ok
            (List.map String.fromChar charList
                |> List.map String.toInt
                -- ifで数字なのは確認しているので握りつぶす
                |> List.map (Maybe.withDefault -1)
            )

    else
        Err "3桁の数字を入力してください"


validateThreeNumberAllDifferent : List Int -> Result String (List Int)
validateThreeNumberAllDifferent list =
    if List.Extra.allDifferent list then
        Ok list

    else
        Err "数字が重複しています"


type ThreeNumberCompareResult
    = ThreeNumberCompareResult Eat Byte


type Eat
    = Eat Int


type Byte
    = Byte Int


compareThreeNumber : ThreeNumber -> ThreeNumber -> ThreeNumberCompareResult
compareThreeNumber (ThreeNumber a1 b1 c1) (ThreeNumber a2 b2 c2) =
    let
        eatCount =
            [ a1 == a2, b1 == b2, c1 == c2 ]
                |> List.map
                    (\val ->
                        if val then
                            1

                        else
                            0
                    )
                |> List.sum

        byteCount =
            [ List.member a2 [ b1, c1 ], List.member b2 [ a1, c1 ], List.member c2 [ a1, b1 ] ]
                |> List.map
                    (\val ->
                        if val then
                            1

                        else
                            0
                    )
                |> List.sum
    in
    ThreeNumberCompareResult (Eat eatCount) (Byte byteCount)


init : ( Model, Cmd Msg )
init =
    ( { questionAnswer = ThreeNumber -1 -1 -1
      , playerAnswer = Nothing
      , playerAnswerString = ""
      , compareResult = Nothing
      , error = Nothing
      }
    , generateRandomThreeNumber
    )



---- UPDATE ----


type Msg
    = GenerateRandomThreeNumber ThreeNumber
    | InputPlayerAnswer String
    | EnterPlayerAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomThreeNumber threeNumber ->
            ( { model | questionAnswer = threeNumber }, Cmd.none )

        InputPlayerAnswer playerAnswerString ->
            ( { model | playerAnswerString = playerAnswerString }, Cmd.none )

        EnterPlayerAnswer ->
            case transformStringToThreeNumber model.playerAnswerString of
                Ok playerAnswer ->
                    let
                        compareResult =
                            compareThreeNumber model.questionAnswer playerAnswer
                    in
                    ( { model | playerAnswer = Just playerAnswer, compareResult = Just compareResult, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewThreeNumber model.questionAnswer
        , viewPlayerAnswer model.playerAnswer
        , viewCompareResult model.compareResult
        , viewPlayerAnswerInput
        , viewError model.error
        ]


viewThreeNumber : ThreeNumber -> Html msg
viewThreeNumber (ThreeNumber one two three) =
    h1 [] [ text <| String.fromInt one ++ String.fromInt two ++ String.fromInt three ]


viewPlayerAnswer : Maybe ThreeNumber -> Html msg
viewPlayerAnswer maybeThreeNumber =
    case maybeThreeNumber of
        Just threeNumber ->
            viewThreeNumber threeNumber

        Nothing ->
            text ""


viewCompareResult : Maybe ThreeNumberCompareResult -> Html msg
viewCompareResult maybeCompareResult =
    case maybeCompareResult of
        Just (ThreeNumberCompareResult (Eat eat) (Byte byte)) ->
            div [] [ text <| String.fromInt eat ++ "eat " ++ String.fromInt byte ++ "byte" ]

        Nothing ->
            text ""


viewPlayerAnswerInput : Html Msg
viewPlayerAnswerInput =
    input [ onEnter EnterPlayerAnswer, onInput InputPlayerAnswer ] []


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "error" ] [ text error ]

        Nothing ->
            text ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
