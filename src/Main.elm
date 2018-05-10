module Main exposing (..)

import Html exposing (Html, button, div, program, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Location, Matrix, loc, mapWithLocation, set, square)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { step : Int
    , world : World
    , life : Life
    }


type alias Life =
    { location : Location
    , energy : Int
    }


type alias World =
    Matrix Tile


type Tile
    = Resource Int
    | Empty


initialModel : Model
initialModel =
    { step = 0
    , life =
        { location = loc 5 5
        , energy = 10
        }
    , world =
        square 12 (\_ -> Empty)
            |> set (loc 5 6) (Resource 5)
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NextStep
    | DoLife



-- Hi Joseph!
-- You've just moved "life" into the model rather than in the grid because that
-- will allow you to move life without having to modify the grid in one update
-- cycle. The next step would be to do something like:
--   1. Get neighbouring tiles that contain resources
--   2a. If there are some, increase the life energy and decrease the resource
--   2b. If there are none, find the closest resource tile and move towards it
--   3. If there are no resource tiles, do nothing to conserve energy
--   4. Write the code that randomly spawns resources
--   5. Figure out a way to write messages to a log so that you can narrate what
--      is happening on each turn
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep ->
            { model | step = model.step + 1 }
                |> update DoLife

        DoLife ->
            ( { model | world = processLifeTurn model.world model.life }, Cmd.none )


processLifeTurn : World -> Life -> World
processLifeTurn world life =
    case getFirstNeighbouringResource world life.location of
        Just (Resource _) ->
            world

        _ ->
            world


isResourceTile : Tile -> Bool
isResourceTile tile =
    case tile of
        Resource _ ->
            True

        _ ->
            False


getFirstNeighbouringResource : World -> Location -> Maybe Tile
getFirstNeighbouringResource world location =
    getNeighbours world location
        |> List.map (Maybe.withDefault Empty)
        |> List.filter isResourceTile
        |> List.head


getNeighbours : World -> Location -> List (Maybe Tile)
getNeighbours world loc =
    List.map (\(x, y) -> Matrix.get ( x, y ) world) [(1, 1)]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "text-align", "center" ), ( "padding", "2em" ) ] ]
        [ div
            [ class "controls", style [ ( "margin-bottom", "2em" ) ] ]
            [ text ("Current Step: " ++ toString model.step ++ " / ")
            , button [ onClick NextStep ] [ text "Next Step" ]
            ]
        , div [ class "world", style [ ( "display", "grid" ), ( "grid-template-columns", "repeat(12, 40px)" ), ( "justify-content", "center" ) ] ] (Matrix.flatten (mapWithLocation (renderTile model.life) model.world))
        ]


renderTile : Life -> Location -> Tile -> Html Msg
renderTile life tileLoc tile =
    if tileLoc == life.location then
        div [ style (( "background", "rgba(255, 0, 0, 1)" ) :: tileStyle) ] [ text (toString life.energy) ]
    else
        case tile of
            Empty ->
                div [ style (( "background", "rgba(0, 0, 0, 0.8)" ) :: tileStyle) ] []

            Resource energy ->
                let
                    energyAsFloat =
                        toString (toFloat energy / 10)
                in
                div [ style (( "background", "rgba(0, 202, 0, " ++ energyAsFloat ++ ")" ) :: tileStyle) ] [ text (toString energy) ]


tileStyle =
    [ ( "align-content", "center" )
    , ( "color", "rgba(255, 255, 255, 1)" )
    , ( "display", "grid" )
    , ( "font-family", "monospace" )
    , ( "height", "40px" )
    , ( "text-shadow", "0 1px 6px rgba(0, 0, 0, 0.8)" )
    ]
