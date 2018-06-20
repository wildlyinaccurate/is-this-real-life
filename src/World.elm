module World exposing (..)

import Matrix exposing (Location, Matrix, loc)
import Tile exposing (..)


type alias World =
    Matrix Tile


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


getLifeTileWithLocation : World -> ( Location, Tile )
getLifeTileWithLocation world =
    let
        life =
            getAllTilesWithLocation world
                |> List.filter (\( _, tile ) -> isLifeTile tile)
                |> List.head
    in
    case life of
        Just pair ->
            pair

        Nothing ->
            Debug.crash "Couldn't find a life tile in the world!"


getLifeLocation : World -> Location
getLifeLocation world =
    let
        ( loc, _ ) =
            getLifeTileWithLocation world
    in
    loc


getLifeTile : World -> Tile
getLifeTile world =
    let
        ( _, life ) =
            getLifeTileWithLocation world
    in
    life


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
