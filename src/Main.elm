module Main exposing (..)

import Html exposing (Html, button, div, program, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Location, Matrix, loc, mapWithLocation, set, square)
import Random exposing (Generator)
import Tile exposing (..)
import Time exposing (Time, millisecond)


randomResourceParams : Generator ( Int, Int, Int )
randomResourceParams =
    let
        max =
            worldSize - 1

        makeTriple a b c =
            ( a, b, c )
    in
    Random.map3 makeTriple (Random.int 0 max) (Random.int 0 max) (Random.int 1 10)


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
    , autoplay : Bool
    , autoplaySpeed : Int
    }


type alias World =
    Matrix Tile


worldSize =
    24


resourceSpawnChance =
    -- Lower number is a higher chance
    60


initialModel : Model
initialModel =
    { step = 0
    , autoplay = False
    , autoplaySpeed = 5
    , world =
        square worldSize (\_ -> Empty)
            |> set (loc 11 11) (Life 20)
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
    | Tick Time
    | ToggleAutoPlay Bool
    | DecreaseSpeed
    | IncreaseSpeed
    | DoLife
    | GrowResources Int
    | SpawnRandomResource ( Int, Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            update NextStep model

        ToggleAutoPlay b ->
            ( { model | autoplay = b }, Cmd.none )

        DecreaseSpeed ->
            ( { model | autoplaySpeed = max 1 (model.autoplaySpeed - 1) }, Cmd.none )

        IncreaseSpeed ->
            ( { model | autoplaySpeed = min 20 (model.autoplaySpeed + 1) }, Cmd.none )

        NextStep ->
            { model | step = model.step + 1 }
                |> update DoLife

        DoLife ->
            ( { model | world = processLifeTurn model.world }, Random.generate GrowResources (Random.int 1 resourceSpawnChance) )

        GrowResources x ->
            if x <= 5 then
                ( model, Random.generate SpawnRandomResource randomResourceParams )
            else if x <= 10 then
                -- TODO: Increase energy of random resource
                ( model, Cmd.none )
            else
                ( model, Cmd.none )

        SpawnRandomResource ( x, y, increaseAmount ) ->
            let
                location =
                    loc x y |> Debug.log ("Sprinkling " ++ toString increaseAmount ++ " energy at")
            in
            ( { model | world = Matrix.update location (increaseTileEnergy increaseAmount) model.world }, Cmd.none )


processLifeTurn : World -> World
processLifeTurn world =
    let
        lifeLoc =
            getLifeLocation world

        firstNeighbouringResource =
            getFirstNeighbouringResource world lifeLoc
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


moveTowardsClosestResource : World -> Location -> World
moveTowardsClosestResource world lifeLoc =
    let
        closestResource =
            getAllTilesWithLocation world
                |> List.filter (\( _, tile ) -> isResourceTile tile)
                |> List.sortBy (\( tileLoc, tile ) -> distanceBetweenLocations lifeLoc tileLoc)
                |> List.head
    in
    case closestResource of
        Just ( resourceLoc, Resource energy ) ->
            let
                _ =
                    Debug.log "Moving towards closest resource" resourceLoc

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


distanceBetweenLocations : Location -> Location -> Float
distanceBetweenLocations l1 l2 =
    let
        r1 =
            Matrix.row l1

        c1 =
            Matrix.col l1

        r2 =
            Matrix.row l2

        c2 =
            Matrix.col l2
    in
    abs <| sqrt <| toFloat <| (c1 - c2) ^ 2 + (r1 - r2) ^ 2


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.autoplay == True then
        Time.every (millisecond * 1000 / toFloat model.autoplaySpeed) Tick
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "text-align", "center" ), ( "padding", "2em" ) ] ]
        [ div
            [ class "controls", style [ ( "margin-bottom", "2em" ) ] ]
            [ text ("Current Step: " ++ toString model.step)
            , separator
            , button [ onClick NextStep, style [ ( "margin", "0 0.3em" ) ] ] [ text "Next Step" ]
            , if model.autoplay then
                button [ onClick (ToggleAutoPlay False), style [ ( "margin", "0 0.3em" ) ] ] [ text "Turn Autoplay Off" ]
              else
                button [ onClick (ToggleAutoPlay True), style [ ( "margin", "0 0.3em" ) ] ] [ text "Turn Autoplay On" ]
            , separator
            , button [ onClick DecreaseSpeed, style [ ( "margin", "0 0.3em" ) ] ] [ text "-" ]
            , text ("Speed: " ++ toString model.autoplaySpeed)
            , button [ onClick IncreaseSpeed, style [ ( "margin", "0 0.3em" ) ] ] [ text "+" ]
            ]
        , div [ class "world", style [ ( "display", "grid" ), ( "grid-template-columns", "repeat(" ++ toString worldSize ++ ", 28px)" ), ( "justify-content", "center" ) ] ] (Matrix.flatten (mapWithLocation renderTile model.world))
        ]


separator : Html Msg
separator =
    span [ style [ ( "margin", "0 0.1em" ) ] ] [ text " | " ]


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
