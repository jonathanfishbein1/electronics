module ElectronicTests exposing (suite)

import Capacitance
import ComplexNumbers
import Electronics
import Expect
import Fuzz
import MultiwayTree
import Real
import Resistance
import Test
import Voltage
import Current

suite : Test.Test
suite =
    Test.describe "Electronic tests"
        [ Test.fuzz
            (Fuzz.map Resistance.ohms (Fuzz.floatRange 0 10))
            "tests equivalent resistence"
          <|
            \resistence ->
                let
                    circuit =
                        MultiwayTree.Tree
                            [ Electronics.Voltage (Voltage.volts 1.0)
                            ]
                            [ MultiwayTree.Tree 
                                [ Electronics.Resistor resistence
                                , Electronics.Resistor resistence
                                , Electronics.Ground
                                ]
                                []
                            , MultiwayTree.Tree
                                [ Electronics.Resistor resistence
                                , Electronics.Ground
                                ]
                                []
                            ]

                    rEqExpected =
                        Resistance.ohms
                            (1 / (1 / (2 * Resistance.inOhms resistence) + (1 / Resistance.inOhms resistence)))
                in
                if Electronics.calculateEquivalentResistance circuit == rEqExpected then
                    Expect.pass

                else
                    Expect.fail "equivalent resistence wrong"
        , Test.fuzz
            (Fuzz.map Capacitance.farads (Fuzz.floatRange 1 10))
            "tests equivalent capacitance"
          <|
            \capacitance ->
                let
                    circuit =
                        MultiwayTree.Tree
                            [ Electronics.Voltage (Voltage.volts 1.0)
                            ]
                            [ MultiwayTree.Tree
                                [ Electronics.Capacitor capacitance
                                , Electronics.Capacitor capacitance
                                , Electronics.Ground
                                ]
                                []
                            , MultiwayTree.Tree
                                [ Electronics.Capacitor capacitance
                                , Electronics.Ground
                                ]
                                []
                            ]

                    rEqExpected =
                        Capacitance.farads
                            (Capacitance.inFarads capacitance + (1 / ((1 / Capacitance.inFarads capacitance) + (1 / Capacitance.inFarads capacitance))))
                in
                if Electronics.calculateEquivalentCapacitance circuit == rEqExpected then
                    Expect.pass

                else
                    Expect.fail "equivalent capacitance wrong"
    , Test.fuzz2
             (Fuzz.map Voltage.volts (Fuzz.floatRange 0 10))
            (Fuzz.map Resistance.ohms (Fuzz.floatRange 1 10))
            "tests calculate current"
          <|
            \voltage resistence ->
                let
                    circuit =
                        MultiwayTree.Tree
                            [ Electronics.Voltage voltage
                            , Electronics.Resistor resistence
                            , Electronics.Ground
                            ]
                            [ 
                            ]

                    currentExpected =
                        Electronics.Current (Current.amperes ((Voltage.inVolts voltage) / (Resistance.inOhms resistence)))
                in
                if Electronics.calculateCurrent circuit == currentExpected then
                    Expect.pass

                else
                    Expect.fail "current wrong"
        ]
