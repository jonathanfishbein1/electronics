module Electronics exposing
    ( Circuit
    , Component(..)
    , calculateEquivalentResistance
    )

import MultiwayTree
import Real


type Component
    = Voltage Float
    | Resistor Float
    | Ground


type alias Circuit =
    MultiwayTree.Tree (List Component)


calculateEquivalentResistance : Circuit -> Float
calculateEquivalentResistance circuit =
    MultiwayTree.foldl
        (\section accumulator ->
            let
                resistorsEquivelent =
                    List.foldl
                        (\component sum ->
                            case component of
                                Resistor r ->
                                    sum + r

                                _ ->
                                    sum
                        )
                        0.0
                        section
            in
            if accumulator == 0 then
                resistorsEquivelent

            else
                1 / ((1 / accumulator) + (1 / resistorsEquivelent))
        )
        0.0
        circuit
