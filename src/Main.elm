module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, footer, h1, header, input, main_, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput,onFocus)
import UUID exposing (UUID,toString,forName)
import Random
import Time exposing (utc, toHour, toMinute, toSecond)

-- MAIN


main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { id : String
    , label : String
    , isEditMode : Bool
    }


type alias Model =
    { todos : List Todo
    , newTodo : String
    , tempEditTodo : String
    }

init : Model
init = { todos = [], newTodo = "",tempEditTodo = "" }

-- UPDATE


type Msg
    = HandleNewTodoChange String
    | AddTodo
    | DeleteTodo String
    | HandleEditBtn String String
    | HandleSaveBtn String String
    | HandleTodoChange String
    | HandleOnFocus

update : Msg -> Model -> Model
update msg model =
    let _ = Debug.log "yo" model
    in
    case msg of
        HandleOnFocus ->
            { model | todos = List.map
                        (\item ->
                            { item | isEditMode = False }
                        )
                        model.todos
            }


        HandleNewTodoChange newTodoText ->
            { model | newTodo = newTodoText }

        AddTodo ->
            if model.newTodo == "" then
                model

            else
                let 
                    uuid = Random.initialSeed 12345
                        |> Random.step UUID.generator
                        |> Tuple.first
                        |> UUID.toString
                in 
                { model | newTodo = "", todos = List.map (\item -> { item | isEditMode = False}) (Todo uuid model.newTodo False :: model.todos) }

        HandleEditBtn id tempLabel->
            { model
                | todos =
                    List.map
                        (\item ->
                            if item.label == id then
                                { item | isEditMode = True }

                            else
                                { item | isEditMode = False }
                        )
                        model.todos,
                tempEditTodo = tempLabel
            }

        HandleSaveBtn id newContent->
            { model
                | todos =
                    List.map
                        (\item ->
                            if item.label == id then
                                { item | isEditMode = False, label = newContent }

                            else
                                item
                        )
                        model.todos,
                tempEditTodo = ""
                
            }

        HandleTodoChange todoText ->
            {model | tempEditTodo = todoText}

        DeleteTodo id ->
            { model | todos = List.map (\item -> { item | isEditMode = False }) (List.filter (\item -> item.label /= id) model.todos) }



-- VIEW


todoRowAttr : List (Html.Attribute msg)
todoRowAttr =
    [ style "border-radius" "5px"
    , style "border" "1px solid lightgrey"

    -- , style "background-color" "red"
    , style "padding" "5px 15px"
    , style "display" "flex"
    , style "justify-content" "space-between"
    , style "margin-bottom" "15px"
    ]


rootAttr : List (Html.Attribute msg)
rootAttr =
    [ style "display" "flex"
    , style "align-items" "center"
    , style "flex-direction" "column"
    ]


headerAttr : List (Html.Attribute msg)
headerAttr =
    [ style "max-width" "400px"
    , style "width" "400px"
    , style "display" "flex"
    , style "justify-content" "center"
    ]


mainAttr : List (Html.Attribute msg)
mainAttr =
    [ style "max-width" "400px"
    , style "width" "400px"
    ]


addTodoInputAttr : Model -> List (Html.Attribute Msg)
addTodoInputAttr model =
    [ placeholder "Enter Todo"
    , value model.newTodo
    , onInput HandleNewTodoChange
    , onFocus HandleOnFocus
    , style "border" "1"
    , style "background" "whitesmoke"
    , style "border-radius" ".25rem"
    , style "padding" "5px"
    , style "color" "black"
    , style "margin-right" "15px"
    , style "flex" "1"
    ]


addTodoBtnAttr : Model -> List (Html.Attribute Msg)
addTodoBtnAttr model=
    let
        isDisabled = model.newTodo == ""
    in
        [ onClick AddTodo
        , disabled isDisabled
        , style "border" "1"
        , style "background" "whitesmoke"
        , style "border-radius" ".25rem"
        , style "padding" "5px"
        , style "color" "black"
        ]


viewTodoRow : Model -> Todo -> Html Msg
viewTodoRow model todoModel =
    -- TempContent: String
    -- TempContent = ""

    div todoRowAttr
        [ if todoModel.isEditMode then
            input [ value model.tempEditTodo, onInput HandleTodoChange ] []

          else
            div [] [ text todoModel.label ]
        , div [ style "display" "flex" ]
            [ if todoModel.isEditMode then
                button [ onClick (HandleSaveBtn todoModel.label model.tempEditTodo), style "margin-right" "15px" ] [ text "Save" ]

              else
                button [ onClick (HandleEditBtn todoModel.label todoModel.label), style "margin-right" "15px" ] [ text "Edit" ]
            , button [ onClick (DeleteTodo todoModel.label) ] [ text "Delete" ]
            ]
        ]


viewHeader : Html Msg
viewHeader =
    header headerAttr
        [ h1 [ style "font-family" "Ubuntu, cursive" ] [ text "Todos App (Elm)" ]
        ]


viewAddTodo : Model -> Html Msg
viewAddTodo model =
    div [ style "margin-bottom" "15px", style "width" "100%", style "display" "flex" ]
        [ input (addTodoInputAttr model) []
        , button (addTodoBtnAttr model) [ text "Add" ]
        ]


view : Model -> Html Msg
view model =
    div rootAttr
        [ viewHeader
        , main_ mainAttr
            [ viewAddTodo model
            , div [] (List.map (viewTodoRow model) model.todos)
            ]
        , footer [] []
        ]
