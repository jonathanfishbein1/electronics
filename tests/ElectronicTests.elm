module ElectronicTests exposing (suite)

import ComplexNumbers
import Electronics
import Expect
import Fuzz
import MultiwayTree
import Real
import Test


suite : Test.Test
suite =
    Test.describe "Electronic tests"
        [ Test.fuzz
            (Fuzz.floatRange 0 10)
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
                        1 / (1 / (2 * resistence) + (1 / resistence))
                in
                if Electronics.calculateEquivalentResistance circuit == rEqExpected then
                    Expect.pass

                else
                    Expect.fail "equivalent resistence wrong"
        ]
