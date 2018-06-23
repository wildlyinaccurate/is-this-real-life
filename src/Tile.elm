module Tile exposing (..)


type Tile
    = Life Int
    | Resource Int
    | Egg Int
    | Empty


getTileEnergy : Tile -> Int
getTileEnergy tile =
    case tile of
        Resource energy ->
            energy

        Life energy ->
            energy

        _ ->
            0


isLifeTile : Tile -> Bool
isLifeTile tile =
    case tile of
        Life _ ->
            True

        _ ->
            False


isEggTile : Tile -> Bool
isEggTile tile =
    case tile of
        Egg _ ->
            True

        _ ->
            False


isResourceTile : Tile -> Bool
isResourceTile tile =
    case tile of
        Resource _ ->
            True

        _ ->
            False


isEmptyTile : Tile -> Bool
isEmptyTile tile =
    case tile of
        Empty ->
            True

        _ ->
            False


increaseTileEnergy : Int -> Tile -> Tile
increaseTileEnergy amount tile =
    case tile of
        Life _ ->
            tile
                |> Debug.log "Life was blessed with random energy but the world is cruel coveted the energy for itself"

        Egg _ ->
            tile
                |> Debug.log "Egg was blessed with random energy but the world is cruel coveted the energy for itself"

        Resource energy ->
            Resource (energy + amount)
                |> Debug.log "Resource grew itself"

        Empty ->
            Resource amount |> Debug.log "New resource spawned"


incrementLifeEnergy : Tile -> Tile
incrementLifeEnergy tile =
    case tile of
        Life energy ->
            Life (energy + 1)
                |> Debug.log "Took energy from a resource"

        _ ->
            tile |> Debug.log "Tried to increment life energy but tile was"
