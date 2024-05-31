module Electronics exposing
    ( Circuit
    , Component(..)
    , calculateEquivalentCapacitance
    , calculateEquivalentResistance
    )

import MultiwayTree
import Real
import Resistance


type Component
    = Voltage Float
    | Resistor Resistance.Resistance
    | Capacitor Float
    | Ground


type alias Circuit =
    MultiwayTree.Tree (List Component)


calculateEquivalentResistance : Circuit -> Resistance.Resistance
calculateEquivalentResistance =
    MultiwayTree.foldl
        (\section accumulator ->
            let
                resistorsEquivelent =
                    List.foldl
                        (\component sum ->
                            case component of
                                Resistor r ->
                                    Resistance.ohms (Resistance.inOhms sum + Resistance.inOhms r)

                                _ ->
                                    sum
                        )
                        (Resistance.ohms 0.0)
                        section
            in
            if accumulator == Resistance.ohms 0 then
                resistorsEquivelent

            else
                Resistance.ohms (1 / ((1 / Resistance.inOhms accumulator) + (1 / Resistance.inOhms resistorsEquivelent)))
        )
        (Resistance.ohms 0.0)


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
