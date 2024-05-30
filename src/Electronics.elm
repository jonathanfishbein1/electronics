module Electronics exposing
    ( Circuit
    , Component(..)
    , calculateEquivalentCapacitance
    , calculateEquivalentResistance
    )

import MultiwayTree
import Real


type Component
    = Voltage Float
    | Resistor Float
    | Capacitor Float
    | Ground


type alias Circuit =
    MultiwayTree.Tree (List Component)


calculateEquivalentResistance : Circuit -> Float
calculateEquivalentResistance =
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


calculateEquivalentCapacitance : Circuit -> Float
calculateEquivalentCapacitance =
    MultiwayTree.foldl
        (\section accumulator ->
            let
                capacitorsEquivelent =
                    List.foldl
                        (\component sum ->
                            case component of
                                Capacitor c ->
                                    if sum == 0 then
                                        c

                                    else
                                        1 / (1 / sum + 1 / c)

                                _ ->
                                    sum
                        )
                        0.0
                        section
            in
            accumulator + capacitorsEquivelent
        )
        0.0
