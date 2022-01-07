import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Helpers

mmToIn : Float -> Float
mmToIn milli =
  milli * 0.03937008

roundTo : Int -> Float -> Float
roundTo places x =
  let
    scale =
      toFloat (10 ^ places)
  in
  toFloat (round (x * scale)) / scale

closestNumerator : Int -> Float -> Int
closestNumerator denom x =
  denom
    |> List.range 0
    |> List.map (\n -> (abs (toFloat n / toFloat denom - x), n))
    |> List.sort
    |> List.head
    |> Maybe.map Tuple.second
    |> Maybe.withDefault 0

-- Model

denoms : List Int
denoms =
  [2, 4, 8, 16, 32, 64]

type alias Model =
  { mm : Maybe Float
  }

-- Update

type Msg
  = MmInput String

update : Msg -> Model -> Model
update msg model =
  case msg of
    MmInput s ->
      { model | mm = String.toFloat s }

-- View

row : List (Html Msg) -> Html Msg
row =
  div
    [ style "padding" "30px"
    ]

srow : List (Html Msg) -> Html Msg
srow =
  div
    [ style "padding" "10px 30px"
    ]

lab : String -> Html Msg
lab s =
  div
    [ style "margin-bottom" "10px"
    ]
    [ text s
    ]

bigInput : (String -> Msg) -> Html Msg
bigInput msg =
  input
    [ type_ "number"
    , onInput msg
    , style "font-size" "2em"
    , style "width" "100%"
    , style "display" "block"
    ]
    []

big : String -> Html Msg
big s =
  div
    [ style "font-size" "5em"
    ]
    [ text s
    ]

view : Model -> Html Msg
view model =
  div
    [ style "font-size" "2em"
    , style "font-family" "sans-serif"
    ] <|
    [ row
        [ lab "Millimeters:"
        , bigInput MmInput
        ]
    ] ++
    ( case model.mm of
        Nothing ->
          []

        Just milli ->
          List.map
            ( \denom ->
                let
                  inches =
                    mmToIn milli

                  numer =
                    closestNumerator denom inches

                  distance =
                    toFloat numer / toFloat denom - inches

                  overUnder =
                    if distance > 0 then
                      "over"
                    else
                      "under"
                in
                srow
                  [ text <|
                      String.fromInt numer ++ " / " ++ String.fromInt denom
                        ++ " in. (" ++ overUnder ++ "approximates by ~"
                        ++ String.fromFloat (roundTo 3 <| abs distance)
                        ++ " in.)"
                  ]
            )
            denoms
    )

-- Main

main : Program () Model Msg
main =
  Browser.sandbox
    { init = { mm = Nothing }
    , update = update
    , view = view
    }

