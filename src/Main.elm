module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL



type alias Todo =
    { id : Int 
    , label : String
    }

type alias Model = { todos: List Todo}



init : Model
init = 
    { todos = [] }




-- UPDATE


type Msg
  = AddTodo


update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo newTodo->
        { model | todos = push newTodo model.todos }   -- push 3 (fromList [1,2]) == fromList [1,2,3]

    DeleteTodo id->
        { model | todos = List.filter id== }   -- push 3 (fromList [1,2]) == fromList [1,2,3]




-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
        button [ onClick AddTodo ] [ text "Add" ]
        -- , div [] [ text (String.fromInt model) ]
        -- , button [ onClick Increment ] [ text "+" ]
    ]