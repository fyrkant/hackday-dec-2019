module Main exposing (..)

import Browser
import Css exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isNothing)



-- Main


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Part =
    { content : String
    , id : Int
    , parentId : Maybe Int
    }


type alias Model =
    { parts : List Part
    , newPart : String
    }


init : Model
init =
    { parts =
        [ Part "this is cool" 1 Nothing
        , Part "this is cooler" 2 Nothing
        , Part "I agree" 3 (Just 1)
        ]
    , newPart = ""
    }



-- Update


type Msg
    = Change String
    | Save


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | newPart = newContent }

        Save ->
            { model
                | parts =
                    List.append model.parts
                        [ { content = model.newPart
                          , id = List.length model.parts + 1
                          , parentId = Maybe.Nothing
                          }
                        ]
                , newPart = ""
            }



-- View


renderPart : Part -> Html Msg
renderPart part =
    div [] [ text (String.fromInt part.id ++ ": " ++ part.content) ]


onlyParents : Part -> Bool
onlyParents part =
    isNothing part.parentId


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map renderPart <| List.filter onlyParents model.parts)
        , div []
            [ input [ placeholder "Enter text", value model.newPart, onInput Change ] []
            , button [ onClick Save ] [ text "Save" ]
            ]
        ]
