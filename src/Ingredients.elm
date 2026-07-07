module Ingredients exposing (Ingredient, catalog)


{-| A catalog of common ice cream ingredients.

Each ingredient lists its composition as percentages (0–100).
The three tracked nutrients are water, sugar, and fat.
The remainder (not listed) consists of protein, fibre, minerals, etc.
and is ignored for the proportional calculation.

Edit this file to add, remove, or adjust ingredients.
Make sure water + sugar + fat ≤ 100 for every entry.

-}


type alias Ingredient =
    { name : String
    , water : Float
    , sugar : Float
    , fat : Float
    }


{-| The full catalog of available ingredients.

Sorted by category for readability:
  • Dairy (milk, cream, butter, …)
  • Sugars & syrups
  • Eggs
  • Powders & concentrates
  • Fruit & flavour bases

-}
catalog : List Ingredient
catalog =
    [ ------------------------------------------------
      --  DAIRY
      ------------------------------------------------
      { name = "Whole milk (3.5%)"
      , water = 88.0
      , sugar = 4.8
      , fat = 3.5
      }
    , { name = "Semi-skimmed milk (1.7%)"
      , water = 89.5
      , sugar = 4.9
      , fat = 1.7
      }
    , { name = "Skim milk (0.1%)"
      , water = 91.0
      , sugar = 5.0
      , fat = 0.1
      }
    , { name = "Heavy cream (35%)"
      , water = 58.0
      , sugar = 3.0
      , fat = 35.0
      }
    , { name = "Double cream (48%)"
      , water = 46.0
      , sugar = 2.5
      , fat = 48.0
      }
    , { name = "Butter (82%)"
      , water = 16.0
      , sugar = 0.0
      , fat = 82.0
      }
    , { name = "Mascarpone"
      , water = 44.0
      , sugar = 3.0
      , fat = 44.0
      }
    , { name = "Ricotta (whole milk)"
      , water = 72.0
      , sugar = 3.0
      , fat = 13.0
      }

    ------------------------------------------------
    --  SUGARS & SYRUPS
    ------------------------------------------------
    , { name = "White sugar (sucrose)"
      , water = 0.0
      , sugar = 100.0
      , fat = 0.0
      }
    , { name = "Dextrose (glucose powder)"
      , water = 0.0
      , sugar = 100.0
      , fat = 0.0
      }
    , { name = "Glucose syrup"
      , water = 20.0
      , sugar = 80.0
      , fat = 0.0
      }
    , { name = "Honey"
      , water = 17.0
      , sugar = 82.0
      , fat = 0.0
      }
    , { name = "Maple syrup"
      , water = 33.0
      , sugar = 67.0
      , fat = 0.0
      }

    ------------------------------------------------
    --  EGGS
    ------------------------------------------------
    , { name = "Egg yolk (1 yolk ≈ 18 g)"
      , water = 50.0
      , sugar = 0.5
      , fat = 27.0
      }
    , { name = "Whole egg"
      , water = 75.0
      , sugar = 0.5
      , fat = 10.0
      }

    ------------------------------------------------
    --  POWDERS & CONCENTRATES
    ------------------------------------------------
    , { name = "Skim milk powder"
      , water = 3.0
      , sugar = 52.0
      , fat = 1.0
      }
    , { name = "Condensed milk (sweetened)"
      , water = 27.0
      , sugar = 55.0
      , fat = 8.0
      }

    ------------------------------------------------
    --  FRUIT & FLAVOUR BASES
    ------------------------------------------------
    , { name = "Strawberry purée"
      , water = 88.0
      , sugar = 9.0
      , fat = 0.3
      }
    , { name = "Mango purée"
      , water = 83.0
      , sugar = 14.0
      , fat = 0.3
      }
    , { name = "Dark chocolate (70%)"
      , water = 1.0
      , sugar = 30.0
      , fat = 42.0
      }
    , { name = "Cocoa powder (unsweetened)"
      , water = 3.0
      , sugar = 0.0
      , fat = 11.0
      }
    ]
