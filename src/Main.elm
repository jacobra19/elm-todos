module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, h1, header, input, main_, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { uid : Int
    , label : String
    , isEditMode : Bool
    }


type alias Model =
    { todos : List Todo
    , newTodo : String
    , tempEditTodo : String
    , uid : Int
    }


init : Model
init =
    { todos = [], newTodo = "", tempEditTodo = "", uid = 0 }



-- UPDATE


type Msg
    = HandleNewTodoChange String
    | AddTodo
    | DeleteTodo Int
    | HandleEditBtn Int String
    | HandleSaveBtn Int String
    | HandleTodoChange String
    | HandleOnFocus


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleOnFocus ->
            { model
                | todos =
                    List.map
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
                { model | newTodo = "", uid = model.uid + 1, todos = List.map (\item -> { item | isEditMode = False }) (Todo model.uid model.newTodo False :: model.todos) }

        HandleEditBtn id tempLabel ->
            { model
                | todos =
                    List.map
                        (\item ->
                            if item.uid == id then
                                { item | isEditMode = True }

                            else
                                { item | isEditMode = False }
                        )
                        model.todos
                , tempEditTodo = tempLabel
            }

        HandleSaveBtn id newContent ->
            { model
                | todos =
                    List.map
                        (\item ->
                            if item.uid == id then
                                { item | isEditMode = False, label = newContent }

                            else
                                item
                        )
                        model.todos
                , tempEditTodo = ""
            }

        HandleTodoChange todoText ->
            { model | tempEditTodo = todoText }

        DeleteTodo id ->
            { model | todos = List.map (\item -> { item | isEditMode = False }) (List.filter (\item -> item.uid /= id) model.todos) }



-- VIEW
-- -- html attributes


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


h1Attr : List (Html.Attribute msg)
h1Attr =
    [ style "font-family" "Ubuntu, cursive" ]


mainAttr : List (Html.Attribute msg)
mainAttr =
    [ style "max-width" "400px"
    , style "width" "400px"
    ]


addTodoAttr : List (Html.Attribute msg)
addTodoAttr =
    [ style "margin-bottom" "15px"
    , style "width" "100%"
    , style "display" "flex"
    ]


addTodoInputAttr : Model -> List (Html.Attribute Msg)
addTodoInputAttr model =
    [ placeholder "Enter Todo"
    , value model.newTodo
    , onInput HandleNewTodoChange
    , onFocus HandleOnFocus
    , autofocus True
    , style "border" "1"
    , style "background" "whitesmoke"
    , style "border-radius" ".25rem"
    , style "padding" "5px"
    , style "color" "black"
    , style "margin-right" "15px"
    , style "flex" "1"
    ]


addTodoBtnAttr : Model -> List (Html.Attribute Msg)
addTodoBtnAttr model =
    [ onClick AddTodo
    , disabled (model.newTodo == "")
    , style "border" "1"
    , style "background" "whitesmoke"
    , style "border-radius" ".25rem"
    , style "padding" "5px"
    , style "color" "black"
    ]


todoRowAttr : List (Html.Attribute Msg)
todoRowAttr =
    [ style "border-radius" "5px"
    , style "border" "1px solid lightgrey"
    , style "padding" "5px 15px"
    , style "display" "flex"
    , style "justify-content" "space-between"
    , style "margin-bottom" "15px"
    ]


todoRowInputAttr : Model -> List (Html.Attribute Msg)
todoRowInputAttr model =
    [ value model.tempEditTodo
    , onInput HandleTodoChange
    , autofocus True
    ]



-- -- views


viewHeader : Html Msg
viewHeader =
    header headerAttr
        [ h1 h1Attr [ text "Todos App (Elm)" ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    main_ mainAttr
        [ viewAddTodo model
        , div [] (List.map (viewTodoRow model) model.todos)
        ]


viewAddTodo : Model -> Html Msg
viewAddTodo model =
    div addTodoAttr
        [ input (addTodoInputAttr model) []
        , button (addTodoBtnAttr model) [ text "Add" ]
        ]


viewTodoRow : Model -> Todo -> Html Msg
viewTodoRow model todoModel =
    div todoRowAttr
        [ if todoModel.isEditMode then
            input (todoRowInputAttr model) []

          else
            div [] [ text todoModel.label ]
        , div [ style "display" "flex" ]
            [ if todoModel.isEditMode then
                button [ onClick (HandleSaveBtn todoModel.uid model.tempEditTodo), style "margin-right" "15px" ] [ text "Save" ]

              else
                button [ onClick (HandleEditBtn todoModel.uid todoModel.label), style "margin-right" "15px" ] [ text "Edit" ]
            , button [ onClick (DeleteTodo todoModel.uid) ] [ text "Delete" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div rootAttr
        [ viewHeader
        , viewMain model
        ]
