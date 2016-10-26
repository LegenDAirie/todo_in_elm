module Main exposing (..)

import Html exposing (Html, text, ul, li, div, button, input, h1)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Html.App as App
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)
import Material.Layout as Layout


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
    , mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = []
      , todoFilter = "All"
      , textField = ""
      , mdl = Material.model
      }
    , Cmd.none
    )


type Msg
    = Add
    | ChangeField String
    | ToggleCompleted Todo
    | Mdl (Material.Msg Msg)


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

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            Material.update msg' model


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


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = [ h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Todos" ] ]
        , drawer = []
        , tabs = ( [ text "All", text "Pending", text "Completed" ], [] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
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
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.onClick Add
            , css "margin" "0 24px"
            ]
            [ text "Add Todo Item" ]
          -- , button [ onClick Add ] [ text "Add Todo Item" ]
        ]
        |> Material.Scheme.top



-- |> Material.Scheme.top


liStyle : Bool -> ( String, String )
liStyle completed =
    if completed then
        ( "text-decoration", "line-through" )
    else
        ( "", "" )
