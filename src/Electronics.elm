module Electronics exposing
    ( Circuit
    , Component(..)
    , calculateEquivalentCapacitance
    , calculateEquivalentResistance
    , calculateCurrent
    )

import Capacitance
import MultiwayTree
import Real
import Resistance
import Voltage
import Current

type Component
    = Voltage Voltage.Voltage
    | Resistor Resistance.Resistance
    | Capacitor Capacitance.Capacitance
    | Current Current.Current
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


calculateEquivalentCapacitance : Circuit -> Capacitance.Capacitance
calculateEquivalentCapacitance =
    MultiwayTree.foldl
        (\section accumulator ->
            let
                capacitorsEquivelent =
                    List.foldl
                        (\component sum ->
                            case component of
                                Capacitor c ->
                                    if sum == Capacitance.farads 0 then
                                        c

                                    else
                                        Capacitance.farads (1 / (1 / Capacitance.inFarads sum + 1 / Capacitance.inFarads c))

                                _ ->
                                    sum
                        )
                        (Capacitance.farads 0.0)
                        section
            in
            Capacitance.farads (Capacitance.inFarads accumulator + Capacitance.inFarads capacitorsEquivelent)
        )
        (Capacitance.farads 0.0)

calculateCurrent : Circuit -> Component
calculateCurrent circuit = 
    case circuit of
         ( MultiwayTree.Tree [Voltage v, Resistor r, Ground ] _) ->
            Current (Current.amperes (Voltage.inVolts v / (Resistance.inOhms r)))
         _ ->
                Current (Current.amperes 1)  
