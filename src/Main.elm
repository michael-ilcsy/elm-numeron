module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, text)
import Random
import Random.Array



---- MODEL ----


type alias Model =
    { questionAnswer : ThreeNumber
    }


type ThreeNumber
    = ThreeNumber Int Int Int


getFromIntArray : Int -> Array Int -> Int
getFromIntArray index array =
    -- 本当は例外処理か絶対数字があることを保証する型を書かないといけない
    Maybe.withDefault -1 (Array.get index array)


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


init : ( Model, Cmd Msg )
init =
    ( { questionAnswer = ThreeNumber 0 0 0 }, generateRandomThreeNumber )



---- UPDATE ----


type Msg
    = GenerateRandomThreeNumber ThreeNumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomThreeNumber threeNumber ->
            ( { model | questionAnswer = threeNumber }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewThreeNumber model.questionAnswer
        ]


viewThreeNumber : ThreeNumber -> Html msg
viewThreeNumber (ThreeNumber one two three) =
    h1 [] [ text <| String.fromInt one ++ String.fromInt two ++ String.fromInt three ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
