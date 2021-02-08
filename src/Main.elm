port module Main exposing (..)

import Array exposing (Array)
import Browser
import Hotkeys exposing (onEnter)
import Html exposing (Html, button, div, input, label, table, td, text, th, tr)
import Html.Attributes exposing (checked, class, disabled, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Random
import Random.Array
import Round



---- MODEL ----


type alias Model =
    { gameMode : GameMode
    , gameRecord : GameRecord
    , gameStatus : GameStatus
    , questionAnswer : ThreeNumber
    , playerAnswerString : String
    , compareResultHistories : List CompareResultHistory
    , error : Maybe String
    }


type GameMode
    = NoItem
    | Items


type alias GameRecord =
    { playCount : Int
    , highScore : Int
    , totalScore : Int
    }


type GameStatus
    = Playing
    | Ready


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


type CompareResultHistory
    = CompareResultHistory ThreeNumber ThreeNumberCompareResult


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


isCorrectAnswer : ThreeNumberCompareResult -> Bool
isCorrectAnswer (ThreeNumberCompareResult (Eat eat) _) =
    eat == 3


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( { gameMode = NoItem
      , gameRecord =
            case D.decodeValue gameRecordDecoder flags of
                Ok gameRecord ->
                    gameRecord

                Err _ ->
                    { totalScore = 0
                    , highScore = 0
                    , playCount = 0
                    }
      , gameStatus = Playing
      , questionAnswer = ThreeNumber -1 -1 -1
      , playerAnswerString = ""
      , compareResultHistories = []
      , error = Nothing
      }
    , generateRandomThreeNumber
    )


port setStorage : E.Value -> Cmd msg


encodeGameRecord : GameRecord -> E.Value
encodeGameRecord gameRecord =
    E.object
        [ ( "playCount", E.int gameRecord.playCount )
        , ( "highScore", E.int gameRecord.highScore )
        , ( "totalScore", E.int gameRecord.totalScore )
        ]


gameRecordDecoder : D.Decoder GameRecord
gameRecordDecoder =
    D.map3 GameRecord
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "totalScore" D.int)



---- UPDATE ----


type Msg
    = GenerateRandomThreeNumber ThreeNumber
    | InputPlayerAnswer String
    | EnterPlayerAnswer
    | GameReset
    | ChangeGameMode GameMode


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

                        latestCompareResultHistory =
                            CompareResultHistory playerAnswer compareResult

                        newCompareResultHistories =
                            Array.toList <| Array.push latestCompareResultHistory <| Array.fromList model.compareResultHistories

                        newModel =
                            { model
                                | compareResultHistories = newCompareResultHistories
                                , error = Nothing
                                , playerAnswerString = ""
                            }
                    in
                    if isCorrectAnswer compareResult then
                        let
                            currentScore =
                                List.length newCompareResultHistories

                            newGameRecord =
                                { totalScore = model.gameRecord.totalScore + currentScore
                                , highScore =
                                    if model.gameRecord.highScore == 0 then
                                        currentScore

                                    else
                                        Basics.min model.gameRecord.highScore currentScore
                                , playCount = model.gameRecord.playCount + 1
                                }
                        in
                        ( { newModel
                            | gameStatus = Ready
                            , gameRecord = newGameRecord
                          }
                        , setStorage <| encodeGameRecord newGameRecord
                        )

                    else
                        ( { newModel
                            | gameStatus = Playing
                          }
                        , Cmd.none
                        )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        GameReset ->
            ( { model
                | gameStatus = Playing
                , compareResultHistories = []
              }
            , generateRandomThreeNumber
            )

        ChangeGameMode gameMode ->
            ( { model
                | gameMode = gameMode
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewGameSelectRadio model.gameMode
        , div [] [ viewGameRecord model.gameRecord ]
        , div []
            [ viewPlayerAnswerInput model
            , viewResetButton model.gameStatus
            ]
        , viewError model.error
        , viewAnswerHistoryTable model.compareResultHistories
        ]


viewGameSelectRadio : GameMode -> Html Msg
viewGameSelectRadio currentGameMode =
    div []
        [ label []
            [ input
                [ type_ "radio"
                , name "gameSelect"
                , onClick <| ChangeGameMode NoItem
                , checked <| currentGameMode == NoItem
                ]
                []
            , text "アイテム無し"
            ]
        , label []
            [ input
                [ type_ "radio"
                , name "gameSelect"
                , onClick <| ChangeGameMode Items
                , checked <| currentGameMode == Items
                ]
                []
            , text "アイテムあり"
            ]
        ]


viewThreeNumber : ThreeNumber -> Html msg
viewThreeNumber (ThreeNumber one two three) =
    text <| String.fromInt one ++ String.fromInt two ++ String.fromInt three


viewCompareResult : ThreeNumberCompareResult -> Html msg
viewCompareResult (ThreeNumberCompareResult (Eat eat) (Byte byte)) =
    text <| String.fromInt eat ++ "eat " ++ String.fromInt byte ++ "byte"


viewGameRecord : GameRecord -> Html msg
viewGameRecord gameRecord =
    let
        highScore =
            if gameRecord.highScore == 0 then
                "なし"

            else
                String.fromInt gameRecord.highScore

        averageScore =
            if gameRecord.playCount == 0 then
                "なし"

            else
                toFloat gameRecord.totalScore
                    / toFloat gameRecord.playCount
                    |> Round.round 2
    in
    div []
        [ div []
            [ text <|
                "ハイスコア: "
                    ++ highScore
            ]
        , div []
            [ text <|
                "平均スコア: "
                    ++ averageScore
            ]
        ]


viewPlayerAnswerInput : { a | playerAnswerString : String, gameStatus : GameStatus } -> Html Msg
viewPlayerAnswerInput model =
    input
        [ onEnter EnterPlayerAnswer
        , onInput InputPlayerAnswer
        , value model.playerAnswerString
        , disabled <| model.gameStatus == Ready
        ]
        []


viewResetButton : GameStatus -> Html Msg
viewResetButton status =
    case status of
        Playing ->
            text ""

        Ready ->
            button [ class "resetButton", onClick GameReset ] [ text "reset" ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "error" ] [ text error ]

        Nothing ->
            text ""


viewAnswerHistoryTable : List CompareResultHistory -> Html Msg
viewAnswerHistoryTable compareResultHistories =
    table []
        (tr []
            [ th [] [ text "解答" ]
            , th [] [ text "結果" ]
            ]
            :: List.map viewAnswerHistoryTableRow compareResultHistories
        )


viewAnswerHistoryTableRow : CompareResultHistory -> Html Msg
viewAnswerHistoryTableRow (CompareResultHistory threeNumber threeNumberCompareResult) =
    tr []
        [ td [] [ viewThreeNumber threeNumber ]
        , td [] [ viewCompareResult threeNumberCompareResult ]
        ]



---- PROGRAM ----


main : Program E.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
