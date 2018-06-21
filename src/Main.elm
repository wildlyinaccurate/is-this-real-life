module Main exposing (..)

import Html exposing (Html, button, div, input, program, span, text)
import Html.Attributes exposing (class, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Matrix exposing (Location, Matrix, loc, mapWithLocation, set, square)
import Random exposing (Generator)
import Tile exposing (..)
import Time exposing (Time, millisecond)
import World exposing (..)


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
    , gameOver : Bool
    , world : World
    , autoplay : Bool
    , autoplaySpeed : Int
    }


worldSize =
    24


resourceSpawnChance =
    -- Lower number is a higher chance
    60


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
            let
                lifeEnergy =
                    case getLifeTile model.world of
                        Life energy ->
                            energy

                        _ ->
                            0
            in
            if lifeEnergy > 0 then
                { model | step = model.step + 1 }
                    |> update DoLife
            else
                ( { model | autoplay = False, gameOver = True }, Cmd.none )

        DoLife ->
            ( { model | world = processLifeTurn model }, Random.generate GrowResources (Random.int 1 resourceSpawnChance) )

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


processLifeTurn : Model -> World
processLifeTurn model =
    let
        world =
            model.world

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
                    gameOverMessage model
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


gameOverMessage : Model -> List (Html Msg)
gameOverMessage model =
    [ span [ style [ ( "color", "rgba(255, 0, 0, 1)" ), ( "font-weight", "bold" ) ] ] [ text "ENERGY DEPLETED. LIFE HAS ENDED." ] ]


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
