module Main exposing (..)

import Browser
import Css exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isJust, isNothing)



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
        , Part "yes" 4 (Just 1)
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


commentCount : Int -> List Part -> Int
commentCount parentId parts =
    List.length
        (List.filter
            (\p ->
                Maybe.withDefault False (Maybe.map (\id -> id == parentId) p.parentId)
            )
            parts
        )


renderPart : Part -> List Part -> Html Msg
renderPart part parts =
    div [] [ text (String.fromInt part.id ++ ": " ++ part.content ++ " (" ++ String.fromInt (commentCount part.id parts) ++ ")") ]


onlyParents : Part -> Bool
onlyParents part =
    isNothing part.parentId


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map (\p -> renderPart p model.parts) <| List.filter onlyParents model.parts)
        , div []
            [ input [ placeholder "Enter text", value model.newPart, onInput Change ] []
            , button [ onClick Save ] [ text "Save" ]
            ]
        ]
