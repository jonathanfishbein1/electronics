module ElectronicTests exposing (suite)

import ComplexNumbers
import Electronics
import Expect
import Fuzz
import MultiwayTree
import Real
import Resistance
import Test


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
                            [ Electronics.Voltage 1.0
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
            (Fuzz.floatRange 1 10)
            "tests equivalent capacitance"
          <|
            \capacitance ->
                let
                    circuit =
                        MultiwayTree.Tree
                            [ Electronics.Voltage 1.0
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
                        capacitance + (1 / ((1 / capacitance) + (1 / capacitance)))
                in
                if Electronics.calculateEquivalentCapacitance circuit == rEqExpected then
                    Expect.pass

                else
                    Expect.fail "equivalent capacitance wrong"
        ]
