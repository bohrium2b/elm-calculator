module Main exposing (main)

import Browser
import Html.Styled exposing (Html, button, div, input, node, text)
import Html.Styled.Attributes exposing (href, rel, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (css, class)
import Css exposing (..)


-- Model


type alias Model =
    { left : Float, right : Float, operator : String, result : Float, display : String, error : String }


init : Model
init =
    { left = 0
    , right = 0
    , operator = "+"
    , result = 0
    , display = "0"
    , error = ""
    }



-- Groups
-- Update


type Msg
    = Add
    | Subtract
    | Multiply
    | Divide
    | Equals
    | Clear
    | SetLeft String
    | SetRight String
    | NoOp


compute : Model -> Float
compute model =
    case model.operator of
        "+" ->
            model.left + model.right

        "-" ->
            model.left - model.right

        "×" ->
            model.left * model.right

        "÷" ->
            model.left / model.right

        _ ->
            0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model | operator = "+" }

        Subtract ->
            { model | operator = "-" }

        Multiply ->
            { model | operator = "×" }

        Divide ->
            { model | operator = "÷" }

        Equals ->
            { model | result = compute model, left = compute model}

        Clear ->
            { model | left = 0, right = 0, operator = "_", result = 0 }

        SetLeft newnumber ->
            { model | left = Maybe.withDefault 0.0 (String.toFloat newnumber) }

        SetRight newnumber ->
            { model | right = Maybe.withDefault 0.0 (String.toFloat newnumber) }

        NoOp ->
            model



-- View


stylesheet : Html msg
stylesheet =
    node "link"
        [ rel "stylesheet"
        , href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
        ]
        []


calculator : Model -> Html Msg
calculator model =
    div []
        [ div [ class "row mb-3" ]
            [ div [ class "col-1" ] [ input [ type_ "number", value <| String.fromFloat model.left, onInput SetLeft, class "col-1 form-control" ] [] ]
            , div [ class "col-1 h-center v-center", css [alignSelf middle] ] [ text model.operator ]
            , div [ class "col-1" ] [ input [ type_ "number", value <| String.fromFloat model.right, onInput SetRight, class "col-1 form-control" ] [] ]
            , div [ class "col-7" ] []
            , div [ class "col-1 h-right v-center" ] [ text " = " ]
            , div [ class "col-1 v-center" ] [ text (String.fromFloat model.result) ]
            ]
        , div [ class "card" ]
            [ div [ class "card-body" ]
                [ div [ class "row " ]
                    [ button [ onClick Add, class "col-3 btn btn-primary" ] [ text "+" ]
                    , button [ onClick Subtract, class "col-3 btn btn-primary" ] [ text "-" ]
                    , div [ class "col-2" ] [ text "" ]
                    , button [ onClick Clear, class "col-3 btn btn-danger" ] [ text "CE" ]
                    ]
                , div [ class "row " ]
                    [ button [ onClick Multiply, class "col-3 btn btn-primary" ] [ text "×" ]
                    , button [ onClick Divide, class "col-3 btn btn-primary" ] [ text "÷" ]
                    , div [ class "col-2" ] [ text "" ]
                    , button [ onClick Equals, class "col-3 btn btn-success" ] [ text "=" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , div[class "container"] [calculator model]
        ]



-- Main


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> Html.Styled.toUnstyled}
