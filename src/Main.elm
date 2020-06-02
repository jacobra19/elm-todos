module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text,input,h1,header,main_,footer)
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
    , isEditMode : Bool
    }

type alias Model = {
    todos: List Todo, 
    newTodo: String}



init : Model
init = { todos = [], newTodo = ""}




-- UPDATE


type Msg
  = HandleNewTodoChange String | AddTodo | DeleteTodo String | HandleEditBtn String | HandleTodoChange String String

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
                { model | newTodo = "", todos = Todo model.newTodo model.newTodo False :: model.todos }
        HandleEditBtn id->
            { model | todos = List.map (\item -> if item.id == id then {item | isEditMode = True} else item ) model.todos }
        
        HandleTodoChange id todoText->
            { model | todos = List.map (\item -> if item.id == id then {item | label = todoText} else item ) model.todos }

        DeleteTodo id->
            { model | todos = List.filter (\item -> item.id /= id) model.todos}



-- VIEW
todoRowAttr : List (Html.Attribute msg)
todoRowAttr = [ style "border-radius" "5px"
    , style "border" "1px solid lightgrey"
    -- , style "background-color" "red"
    , style "padding" "5px 15px"
    , style "display" "flex"
    , style "justify-content" "space-between"
    , style "margin-bottom" "15px"
    ]

rootAttr : List (Html.Attribute msg)
rootAttr = [ 
    style "display" "flex"
    , style "align-items" "center"
    , style "flex-direction" "column"
    ]

addTodoInputAttr : Model -> List (Html.Attribute Msg)
addTodoInputAttr model = [
    placeholder "Enter Todo"
    , value model.newTodo
    , onInput HandleNewTodoChange 
    ,style "border" "1"
    ,style "background" "whitesmoke"
    ,style "border-radius" ".25rem"
    ,style "padding" "5px"
    ,style "color" "black"
    ,style "margin-right" "15px"
    ]

addTodoBtnAttr: List (Html.Attribute Msg)
addTodoBtnAttr = [
    onClick AddTodo 
    ,style "border" "1"
    ,style "background" "whitesmoke"
    ,style "border-radius" ".25rem"
    ,style "padding" "5px"
    ,style "color" "black"
    ]

viewTodoRow : Todo -> Html Msg
viewTodoRow todoModel = 

    div todoRowAttr [
        if todoModel.isEditMode then
            input [ value todoModel.label , onInput (HandleTodoChange todoModel.id) ] []
        else
            div [ ] [ text todoModel.label],
        div [ style "display" "flex" ] [
            button [ onClick (HandleEditBtn todoModel.id), style "margin-right" "15px" ] [text "Edit"]
            , button [ onClick (DeleteTodo todoModel.id) ] [text "Delete"]
        ]
    ]

viewHeader: Html Msg
viewHeader= 
    header [] [
        h1 [style "font-family" "Ubuntu, cursive"] [text "Todos App (Elm)"]
    ]

viewAddTodo: Model -> Html Msg
viewAddTodo model = 
    div [ style "margin-bottom" "15px", style "max-width" "400px"] [ 
        input (addTodoInputAttr model) []
        , button addTodoBtnAttr [ text "Add" ]
    ]


view : Model -> Html Msg
view model =
    div rootAttr [
        viewHeader,
        main_ [] [
            viewAddTodo model,

            div [] (List.map viewTodoRow model.todos)

        ],
        footer [] []
    ]
