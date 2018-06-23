module Main exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Location, loc, mapWithLocation, set, square)
import Random exposing (Generator)
import Tile exposing (Tile(..), getTileEnergy, increaseTileEnergy, incrementLifeEnergy)
import Time exposing (Time, millisecond)
import World exposing (World, getEggTiles, getFirstNeighbouringEmpty, getFirstNeighbouringLife, getFirstNeighbouringResource, getLifeTiles, getNeighbouringResources, moveTowardsBestResource)


randomResourceParams : Generator ( Int, Int, Int )
randomResourceParams =
    let
        max =
            worldSize - 1

        makeTriple a b c =
            ( a, b, c )
    in
    Random.map3 makeTriple (Random.int 0 max) (Random.int 0 max) (Random.int 1 10)


main : Program Never Model Msg
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
    , gameOver : Bool
    , world : World
    , autoplay : Bool
    , autoplaySpeed : Int
    }


worldSize : Int
worldSize =
    24


lifeStartingEnergy : Int
lifeStartingEnergy =
    20


eggHatchSteps : Int
eggHatchSteps =
    lifeStartingEnergy * 2


randomEventChance : Int
randomEventChance =
    worldSize * 3


randomResourceChance : Int
randomResourceChance =
    8


initialModel : Model
initialModel =
    { step = 0
    , gameOver = False
    , autoplay = False
    , autoplaySpeed = 5
    , world =
        square worldSize (\_ -> Empty)
            |> set (loc 0 0) (Egg 10)
            |> set (loc 2 2) (Resource 6)
            |> set (loc 3 3) (Resource 5)
            |> set (loc 12 18) (Resource 6)
            |> set (loc 13 18) (Resource 4)
            |> set (loc 14 18) (Resource 3)
            |> set (loc 14 19) (Resource 10)
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
    | RandomEvent Int
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
            let
                allLifeEnergy =
                    getLifeTiles model.world
                        |> List.map (\( _, tile ) -> getTileEnergy tile)
                        |> List.sum

                eggsInPlay =
                    List.length <| getEggTiles model.world
            in
            if allLifeEnergy == 0 && eggsInPlay == 0 then
                ( { model | autoplay = False, gameOver = True }, Cmd.none )
            else
                ( { model | step = model.step + 1, world = updateWorld model.world }, Random.generate RandomEvent (Random.int 1 randomEventChance) )

        RandomEvent x ->
            if x <= randomResourceChance then
                ( model, Random.generate SpawnRandomResource randomResourceParams )
            else
                ( model, Cmd.none )

        SpawnRandomResource ( x, y, increaseAmount ) ->
            let
                location =
                    loc x y |> Debug.log ("Sprinkling " ++ toString increaseAmount ++ " energy at")
            in
            ( { model | world = Matrix.update location (increaseTileEnergy increaseAmount) model.world }, Cmd.none )


updateWorld : World -> World
updateWorld world =
    updateStaticTiles world
        |> processLifeTiles


updateStaticTiles : World -> World
updateStaticTiles world =
    Matrix.mapWithLocation (updateStaticTile world) world


updateStaticTile : World -> Location -> Tile -> Tile
updateStaticTile world tileLoc tile =
    case tile of
        Life _ ->
            tile

        Egg stepsToHatch ->
            if stepsToHatch == 1 then
                Life lifeStartingEnergy
            else
                Egg (stepsToHatch - 1)

        Empty ->
            let
                lifeIsNearby =
                    case getFirstNeighbouringLife world tileLoc of
                        Nothing ->
                            False

                        _ ->
                            True

                neighbouringResources =
                    getNeighbouringResources world tileLoc

                neighbouringResourceEnergy =
                    neighbouringResources
                        |> List.map (\( _, tile ) -> getTileEnergy tile)
                        |> List.sum
            in
            if List.length neighbouringResources == 4 && lifeIsNearby == False then
                Resource 1
                    |> Debug.log "Tile had enough surrounding resources to bloom into a resource"
            else if neighbouringResourceEnergy >= 20 && lifeIsNearby == False then
                Resource 2
                    |> Debug.log "Tile had enough surrounding energy to bloom into a resource"
            else
                tile

        _ ->
            tile


processLifeTiles : World -> World
processLifeTiles world =
    let
        lifeTiles =
            getLifeTiles world
    in
    List.foldl processLifeTile world lifeTiles


processLifeTile : ( Location, Tile ) -> World -> World
processLifeTile ( lifeLoc, lifeTile ) world =
    let
        lifeEnergy =
            getTileEnergy lifeTile
    in
    if lifeEnergy == 0 then
        Matrix.set (lifeLoc |> Debug.log "Life ran out of energy and died") Empty world
    else if lifeEnergy >= 50 then
        case getFirstNeighbouringEmpty world lifeLoc of
            Just ( emptyTileLoc, _ ) ->
                reproduce ( lifeLoc, lifeTile ) emptyTileLoc world

            _ ->
                aquireEnergyFromResources ( lifeLoc, lifeTile ) world
    else
        aquireEnergyFromResources ( lifeLoc, lifeTile ) world


reproduce : ( Location, Tile ) -> Location -> World -> World
reproduce ( lifeLoc, lifeTile ) spawnLoc world =
    case lifeTile of
        Life energy ->
            Matrix.set lifeLoc (Life (energy - lifeStartingEnergy)) world
                |> Matrix.set spawnLoc (Egg eggHatchSteps)

        _ ->
            world


aquireEnergyFromResources : ( Location, Tile ) -> World -> World
aquireEnergyFromResources ( lifeLoc, _ ) world =
    let
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

        _ ->
            moveTowardsBestResource world lifeLoc



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
        [ div [ style [ ( "margin-bottom", "2em" ) ] ] <|
            List.concat
                [ [ text ("Current Step: " ++ toString model.step), separator ]
                , if model.gameOver == False then
                    playControls model
                  else
                    gameOverMessage
                ]
        , div [ class "world", gridStyle model ] (Matrix.flatten (mapWithLocation renderTile model.world))
        ]


gridStyle : Model -> Html.Attribute Msg
gridStyle model =
    style
        [ ( "display", "grid" )
        , ( "grid-template-columns", "repeat(" ++ toString worldSize ++ ", 28px)" )
        , ( "justify-content", "center" )
        , if model.gameOver == True then
            ( "opacity", "0.5" )
          else
            ( "opacity", "1" )
        ]


playControls : Model -> List (Html Msg)
playControls model =
    [ button [ onClick NextStep, style [ ( "margin", "0 0.3em" ) ] ] [ text "Next Step" ]
    , if model.autoplay then
        button [ onClick (ToggleAutoPlay False), style [ ( "margin", "0 0.3em" ) ] ] [ text "Turn Autoplay Off" ]
      else
        button [ onClick (ToggleAutoPlay True), style [ ( "margin", "0 0.3em" ) ] ] [ text "Turn Autoplay On" ]
    , separator
    , button [ onClick DecreaseSpeed, style [ ( "margin", "0 0.3em" ) ] ] [ text "-" ]
    , text ("Speed: " ++ toString model.autoplaySpeed)
    , button [ onClick IncreaseSpeed, style [ ( "margin", "0 0.3em" ) ] ] [ text "+" ]
    ]


gameOverMessage : List (Html Msg)
gameOverMessage =
    [ span [ style [ ( "color", "rgba(255, 0, 0, 1)" ), ( "font-weight", "bold" ) ] ] [ text "ENERGY DEPLETED. LIFE HAS ENDED." ] ]


separator : Html msg
separator =
    span [ style [ ( "margin", "0 0.1em" ) ] ] [ text " | " ]


renderTile : Location -> Tile -> Html Msg
renderTile _ tile =
    case tile of
        Life energy ->
            div [ style (( "background", "rgba(255, 0, 0, 1)" ) :: tileStyle) ] [ text (toString energy) ]

        Egg stepsToHatch ->
            div [ style (( "background", "rgba(0, 0, 255, 1)" ) :: tileStyle) ] [ text (toString stepsToHatch) ]

        Resource energy ->
            let
                energyAsFloat =
                    toString (toFloat energy / 10)
            in
            div [ style (( "background", "rgba(0, 202, 0, " ++ energyAsFloat ++ ")" ) :: tileStyle) ] [ text (toString energy) ]

        Empty ->
            div [ style (( "background", "rgba(0, 0, 0, 0.8)" ) :: tileStyle) ] []


tileStyle : List ( String, String )
tileStyle =
    [ ( "align-content", "center" )
    , ( "color", "rgba(255, 255, 255, 1)" )
    , ( "display", "grid" )
    , ( "font-family", "monospace" )
    , ( "height", "28px" )
    , ( "text-shadow", "0 1px 6px rgba(0, 0, 0, 0.8)" )
    ]
