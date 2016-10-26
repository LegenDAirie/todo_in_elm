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
import Material.Color as Color


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
    , selectedTab : Int
    , textField : String
    , mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = []
      , selectedTab = 0
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
    | SelectTab Int


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

        SelectTab num ->
            { model | selectedTab = num } ! []


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
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.onSelectTab SelectTab
            ]
            { header = [ h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Todos" ] ]
            , drawer = []
            , tabs = ( [ text "All", text "Pending", text "Completed" ], [] )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            displayTodos model model.todos

        1 ->
            pending model

        2 ->
            completed model

        _ ->
            text "404"


completed : Model -> Html Msg
completed model =
    displayTodos model (List.filter (\todo -> todo.completed == True) model.todos)


pending : Model -> Html Msg
pending model =
    displayTodos model (List.filter (\todo -> todo.completed == False) model.todos)


displayTodos : Model -> List Todo -> Html Msg
displayTodos model todos =
    div []
        [ ul
            []
            (List.map
                (\todo ->
                    li
                        [ onClick (ToggleCompleted todo), style [ liStyle todo.completed ] ]
                        [ text todo.text ]
                )
                todos
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


liStyle : Bool -> ( String, String )
liStyle completed =
    if completed then
        ( "text-decoration", "line-through" )
    else
        ( "", "" )
