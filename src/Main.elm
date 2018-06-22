module Main exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Location, loc, mapWithLocation, set, square)
import Random exposing (Generator)
import Tile exposing (Tile(..), increaseTileEnergy, incrementLifeEnergy, tileEnergy)
import Time exposing (Time, millisecond)
import World exposing (World, getFirstNeighbouringResource, getLifeTiles, moveTowardsBestResource)


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


randomEventChance : Int
randomEventChance =
    worldSize * 3


randomResourceChance : Int
randomResourceChance =
    6


initialModel : Model
initialModel =
    { step = 0
    , gameOver = False
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
                        |> List.map (\( _, tile ) -> tileEnergy tile)
                        |> List.sum
            in
            if allLifeEnergy == 0 then
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
    updateResourceTiles world
        |> processLifeTiles


updateResourceTiles : World -> World
updateResourceTiles world =
    Matrix.mapWithLocation (updateResourceTile world) world


updateResourceTile : World -> Location -> Tile -> Tile
updateResourceTile _ _ tile =
    case tile of
        Resource _ ->
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
processLifeTile ( lifeLoc, _ ) world =
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

        Nothing ->
            moveTowardsBestResource world lifeLoc

        _ ->
            world



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
