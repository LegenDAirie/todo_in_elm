module Main exposing (..)

import Html exposing (Html, text, ul, li, div, button, input)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Html.App as App


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type alias Todo =
    { text : String
    , id : Int
    , completed : Bool
    }


type alias Model =
    { todos : List Todo
    , todoFilter : String
    , textField : String
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = []
      , todoFilter =
            "All"
      , textField = ""
      }
    , Cmd.none
    )


type Msg
    = Add
    | ChangeField String
    | ToggleCompleted Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        ChangeField newText ->
            ( { model
                | textField = newText
              }
            , Cmd.none
            )

        Add ->
            ( { model
                | todos =
                    Todo model.textField (getId model.todos) False
                        :: model.todos
              }
            , Cmd.none
            )

        ToggleCompleted todo ->
            ( { model
                | todos = List.map (\todo' -> (toggleTodo todo todo')) model.todos
              }
            , Cmd.none
            )


toggleTodo : Todo -> Todo -> Todo
toggleTodo todo todo' =
    if todo == todo' then
        { todo
            | completed = not todo.completed
        }
    else
        todo'


getId : List Todo -> Int
getId todos =
    let
        lastTodo =
            List.head todos

        id =
            case lastTodo of
                Just todo ->
                    todo.id

                Nothing ->
                    0
    in
        id + 1


view : Model -> Html Msg
view model =
    div []
        [ ul
            []
            (List.map
                (\todo ->
                    li
                        [ onClick (ToggleCompleted todo), style [ liStyle todo.completed ] ]
                        [ text todo.text ]
                )
                model.todos
            )
        , input [ onInput ChangeField ] []
        , button [ onClick Add ] [ text "Add Todo Item" ]
        ]


liStyle : Bool -> ( String, String )
liStyle completed =
    if completed then
        ( "text-decoration", "line-through" )
    else
        ( "", "" )
