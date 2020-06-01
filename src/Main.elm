module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text,input)
import Html.Attributes exposing (..)

import Html.Events exposing (onClick,onInput)




-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { id : String 
    , label : String
    }

type alias Model = {
    todos: List Todo, 
    newTodo: String}



init : Model
init = { todos = [], newTodo = ""}




-- UPDATE


type Msg
  = HandleNewTodoChange String | AddTodo | DeleteTodo 


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleNewTodoChange newTodoText ->
            { model | newTodo = newTodoText}

        AddTodo ->
            { model | newTodo = "", todos = Todo "asdasd" model.newTodo :: model.todos }

        DeleteTodo ->
            model



-- VIEW
viewTodo : Todo -> Html Msg
viewTodo todoModel = 

    div [] [
            div [] [ text todoModel.label]
            , button [ onClick DeleteTodo ] [text "Delete"]
        ]


view : Model -> Html Msg
view model =
    div [] [
        div [ style "display" "flex"] [ 
            input [ placeholder "Enter Todo", value model.newTodo, onInput HandleNewTodoChange ] []
            , button [ onClick AddTodo ] [ text "Add" ]
        ],
        div [] (List.map viewTodo model.todos)
    ]
