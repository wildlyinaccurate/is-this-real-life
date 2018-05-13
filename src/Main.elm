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
    }


type alias World =
    Matrix Tile


type Tile
    = Life Int
    | Resource Int
    | Empty


worldSize : Int
worldSize =
    24


initialModel : Model
initialModel =
    { step = 0
    , world =
        square worldSize (\_ -> Empty)
            |> set (loc 11 11) (Life 10)
            |> set (loc 2 14) (Resource 2)
            |> set (loc 3 22) (Resource 6)
            |> set (loc 5 6) (Resource 5)
            |> set (loc 7 7) (Resource 10)
            |> set (loc 10 11) (Resource 6)
            |> set (loc 14 4) (Resource 7)
            |> set (loc 17 9) (Resource 9)
            |> set (loc 14 21) (Resource 1)
            |> set (loc 23 17) (Resource 4)
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NextStep
    | DoLife
    | MoveLife


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep ->
            { model | step = model.step + 1 }
                |> update DoLife

        DoLife ->
            ( { model | world = processLifeTurn model.world }, Cmd.none )

        MoveLife ->
            ( model, Cmd.none )


processLifeTurn : World -> World
processLifeTurn world =
    let
        lifeLoc =
            getLifeLocation world

        firstNeighbouringResource =
            getFirstNeighbouringResource world lifeLoc

        incrementLifeEnergy tile =
            case tile of
                Life energy ->
                    Life (energy + 1)
                        |> Debug.log "Life took some energy from a resource"

                _ ->
                    Debug.crash "Tried to increment energy of a non-life tile"
    in
    case firstNeighbouringResource of
        Just ( location, Resource energy ) ->
            if energy > 1 then
                Matrix.update location (\_ -> Resource (energy - 1)) world
                    |> Matrix.update lifeLoc incrementLifeEnergy
            else
                Matrix.update location (\_ -> Empty) world
                    |> Matrix.update lifeLoc incrementLifeEnergy

        Nothing ->
            moveTowardsClosestResource world lifeLoc

        _ ->
            world


getLifeLocation : World -> Location
getLifeLocation world =
    let
        life =
            getAllTilesWithLocation world
                |> List.filter (\( _, tile ) -> isLifeTile tile)
                |> List.head
    in
    case life of
        Just ( location, _ ) ->
            location

        Nothing ->
            Debug.crash "Couldn't find a life tile in the world!"


getAllTilesWithLocation : World -> List ( Location, Tile )
getAllTilesWithLocation world =
    let
        lastColIndex =
            Matrix.colCount world - 1

        lastRowIndex =
            Matrix.rowCount world - 1

        cols =
            List.range 0 lastColIndex

        rows =
            List.range 0 lastRowIndex
    in
    List.concatMap (\col -> List.map (\row -> getTileWithLocation world row col) rows) cols


getTileWithLocation : World -> Int -> Int -> ( Location, Tile )
getTileWithLocation world row col =
    let
        location =
            loc row col

        tile =
            Matrix.get location world
    in
    ( location, Maybe.withDefault Empty tile )


isLifeTile : Tile -> Bool
isLifeTile tile =
    case tile of
        Life _ ->
            True

        _ ->
            False


moveTowardsClosestResource : World -> Location -> World
moveTowardsClosestResource world lifeLoc =
    let
        closestResource =
            getAllTilesWithLocation world
                |> List.filter (\( _, tile ) -> isResourceTile tile)
                |> List.sortBy (\( _, tile ) -> tileEnergy tile)
                |> List.reverse
                -- TODO: Sort by distance to life location (currently sorting by energy)
                |> List.head
    in
    case closestResource of
        Just ( resourceLoc, Resource energy ) ->
            let
                _ =
                    Debug.log "Expending energy to move towards" resourceLoc

                lRow =
                    Matrix.row lifeLoc

                lCol =
                    Matrix.col lifeLoc

                rRow =
                    Matrix.row resourceLoc

                rCol =
                    Matrix.col resourceLoc

                newLifeTile =
                    case Matrix.get lifeLoc world of
                        Just (Life energy) ->
                            Life (energy - 1)

                        Just notLifeTile ->
                            Debug.crash <| "Expected to find life at tile " ++ toString lifeLoc ++ " but found " ++ toString notLifeTile

                        Nothing ->
                            Debug.crash <| "Expected to find life at tile " ++ toString lifeLoc ++ " but found nothing. Absolutely NOTHING"
            in
            if lRow > rRow then
                Matrix.set lifeLoc Empty world
                    |> Matrix.set (loc (lRow - 1) lCol) newLifeTile
            else if lRow < rRow then
                Matrix.set lifeLoc Empty world
                    |> Matrix.set (loc (lRow + 1) lCol) newLifeTile
            else if lCol > rCol then
                Matrix.set lifeLoc Empty world
                    |> Matrix.set (loc lRow (lCol - 1)) newLifeTile
            else if lCol < rCol then
                Matrix.set lifeLoc Empty world
                    |> Matrix.set (loc lRow (lCol + 1)) newLifeTile
            else
                Debug.crash "Trying to move life tile towards a resource but it's already right next to it?!"

        _ ->
            Debug.log "No more resources to move to" world


tileEnergy : Tile -> Int
tileEnergy tile =
    case tile of
        Resource energy ->
            energy

        Life energy ->
            energy

        Empty ->
            0


getFirstNeighbouringResource : World -> Location -> Maybe ( Location, Tile )
getFirstNeighbouringResource world location =
    getNeighbours world location
        |> List.map (\( loc, mTile ) -> ( loc, Maybe.withDefault Empty mTile ))
        |> List.filter (\( _, tile ) -> isResourceTile tile)
        |> List.head
        |> Debug.log "First neighbouring resource"


getNeighbours : World -> Location -> List ( Location, Maybe Tile )
getNeighbours world origin =
    let
        row =
            Matrix.row origin

        col =
            Matrix.col origin

        neighbourLocs =
            [ ( row - 1, col - 1 )
            , ( row - 1, col )
            , ( row - 1, col + 1 )
            , ( row, col - 1 )
            , ( row, col + 1 )
            , ( row + 1, col - 1 )
            , ( row + 1, col )
            , ( row + 1, col + 1 )
            ]
    in
    List.map (\( x, y ) -> ( loc x y, Matrix.get ( x, y ) world )) neighbourLocs


isResourceTile : Tile -> Bool
isResourceTile tile =
    case tile of
        Resource _ ->
            True

        _ ->
            False



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
        , div [ class "world", style [ ( "display", "grid" ), ( "grid-template-columns", "repeat(" ++ toString worldSize ++ ", 28px)" ), ( "justify-content", "center" ) ] ] (Matrix.flatten (mapWithLocation renderTile model.world))
        ]


renderTile : Location -> Tile -> Html Msg
renderTile tileLoc tile =
    case tile of
        Life energy ->
            div [ style (( "background", "rgba(255, 0, 0, 1)" ) :: tileStyle) ] [ text (toString energy) ]

        Resource energy ->
            let
                energyAsFloat =
                    toString (toFloat energy / 10)
            in
            div [ style (( "background", "rgba(0, 202, 0, " ++ energyAsFloat ++ ")" ) :: tileStyle) ] [ text (toString energy) ]

        Empty ->
            div [ style (( "background", "rgba(0, 0, 0, 0.8)" ) :: tileStyle) ] []


tileStyle =
    [ ( "align-content", "center" )
    , ( "color", "rgba(255, 255, 255, 1)" )
    , ( "display", "grid" )
    , ( "font-family", "monospace" )
    , ( "height", "28px" )
    , ( "text-shadow", "0 1px 6px rgba(0, 0, 0, 0.8)" )
    ]
