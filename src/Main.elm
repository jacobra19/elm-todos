module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text,input,h1)
import Html.Attributes exposing (..)
import UUID exposing (UUID)

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
  = HandleNewTodoChange String | AddTodo | DeleteTodo String 

-- isFoundIdToRemove: Todo String -> Bool 
-- isFoundIdToRemove todoModel id =
--     todoModel.id == id
    

update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleNewTodoChange newTodoText ->
            { model | newTodo = newTodoText}

        AddTodo ->
            if model.newTodo == "" then
                model
            else 
                { model | newTodo = "", todos = Todo model.newTodo model.newTodo :: model.todos }

        DeleteTodo id->
            { model | todos = List.filter (\item -> item.id /= id) model.todos}



-- VIEW
viewTodoStyle : List (Html.Attribute msg)
viewTodoStyle = [ style "border-radius" "5px"
    , style "border" "1px solid grey"
    -- , style "background-color" "red"
    , style "padding" "0px 15px"
    , style "display" "flex"
    ]

rootStyle : List (Html.Attribute msg)
rootStyle = [ 
    style "display" "flex"
    , style "align-items" "center"
    , style "flex-direction" "column"
    ]

viewTodo : Todo -> Html Msg
viewTodo todoModel = 

    div viewTodoStyle [
            div [] [ text todoModel.label]
            , button [ onClick (DeleteTodo todoModel.id) ] [text "Delete"]
        ]
-- viewHeader: 


view : Model -> Html Msg
view model =
    div rootStyle [
        h1 [ ] [ text "Todos in Elm"],
        div [ ] [ 
            input [ placeholder "Enter Todo", value model.newTodo, onInput HandleNewTodoChange ] []
            , button [ onClick AddTodo ] [ text "Add" ]
        ],
        div [] (List.map viewTodo model.todos)
    ]
