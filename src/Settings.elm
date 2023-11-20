module Settings exposing (..)

{-| This module handles everything on the Settings screen.

TODO: You will need to modify this file to add / remove settings for your game.

Adding/removing a setting is a 5-step process.
(I know it seems like a lot, but it is necessary so Elm can make static
guarantees at compile time about your Settings).

I've outlined the five steps below under SETTING DEFINITIONS.

-}

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



--------------------------------------------------------------------------------
-- SETTING DEFINITIONS
--
-- You can add / delete settings by modifying the following 5 steps:
-- 1. Define the data model for your settings and their types.
-- 2. Define the default values for your settings.
-- 3. Add a message type to update your settings.
-- 4. Define explicitly what happens to your settings when a message is received.
-- 5. Define a list of pickers for each setting you want to be able to change.
--
-- This should cover most of the basic use cases. If you need extra
-- customisation, you're welcome to edit the code below or delete everything
-- here and start from scratch.
--------------------------------------------------------------------------------


{-| STEP 1: Define the data model for your settings and their types.

Keep it simple: you probably don't as many settings as there are here
(you might only need 1 or 2). You'll have access to the data of this type
in your Game when the user clicks StartGame.

-}
type alias Settings =
    { initialCount : Int
    }


{-| STEP 2: Define the default values for your settings.

For simplicity's sake, every setting MUST have a default value.

-}
default : Settings
default =
    { initialCount = 0
    }


{-| STEP 3: Add a message type to update your settings.

Your message type should have a payload attached (the new value for the
setting). This is typically the same type as your setting.

-}
type Msg
    = SetInitialCount Int


{-| STEP 4: Define explicitly what happens to your settings when a message is received.

Handle each Msg case below. Most likely, you'll just update the settings record
with the new payload. You can see the implementations below for this.

-}
update : Msg -> Settings -> Settings
update msg settings =
    case msg of
        SetInitialCount count ->
            { settings | initialCount = count }


{-| STEP 5: Define a list of pickers for each setting you want to be able to change.

I've defined a bunch of helper functions for you to make things easier.

Helper functions include:

  - inputString (a small text input for the user to input a string)
  - inputFloat (a number input for floats)
  - inputInt (a number input for ints)
  - inputFloatRange (a range slider for floats)
  - inputIntRange (a range slider for ints)
  - pickChoiceButtons (a set of buttons for the user to pick from - good for small enums)
  - pickChoiceDropdown (a dropdown of options for the user to pick from)

Each function has it's own type defining what data it needs; see the HELPER
FUNCTIONS section.

You can customise this further if you so wish (see the HELPER FUNCTIONS section below).

-}
pickers : Settings -> List SettingPickerItem
pickers settings =
    [ inputIntRange
        { label = "Initial Count"
        , value = settings.initialCount
        , min = 0
        , max = 100
        , onChange = SetInitialCount
        }
    ]



-- =============================================================================
-- =============================================================================
-- NOTE: YOU PROBABLY DON'T HAVE TO MODIFY ANYTHING BELOW THIS LINE
-- =============================================================================
-- =============================================================================
--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- If your use cases are covered by the basic types of settings above, you don't have to
-- edit any of the code below (it's boilerplate to make things easier for you).
-- However, if you need extra customisation, then you're welcome to edit it
-- if you know what you're doing (e.g. show a setting only in certain conditions,
-- or add extra specific styling to a setting).
--------------------------------------------------------------------------------
-- Helper functions to create Setting picker item types.
-- These are the functions you'll actually use to construct your pickers.
-- INPUT STRING


type alias InputStringConfig =
    { label : String
    , value : String
    , onChange : String -> Msg
    }


{-| A basic text box that allows the user to input a string.
-}
inputString : InputStringConfig -> SettingPickerItem
inputString data =
    InputString data



-- INPUT FLOAT


type alias InputFloatConfig =
    { label : String
    , value : Float
    , min : Float
    , max : Float
    , onChange : Float -> Msg
    }


{-| A basic box that allows the user to input a float.
-}
inputFloat : InputFloatConfig -> SettingPickerItem
inputFloat data =
    InputFloat data



-- INPUT INT


type alias InputIntConfig =
    { label : String
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> Msg
    }


{-| A basic box that allows the user to input an int.
-}
inputInt : InputIntConfig -> SettingPickerItem
inputInt data =
    InputInt data



-- INPUT FLOAT RANGE


type alias InputFloatRangeConfig =
    { label : String
    , value : Float
    , step : Float
    , min : Float
    , max : Float
    , onChange : Float -> Msg
    }


{-| A range slider that allows the user to input a float.
-}
inputFloatRange : InputFloatRangeConfig -> SettingPickerItem
inputFloatRange data =
    InputFloatRange data



-- INPUT INT RANGE


type alias InputIntRangeConfig =
    { label : String
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> Msg
    }


{-| A range slider that allows the user to input an int.
-}
inputIntRange : InputIntRangeConfig -> SettingPickerItem
inputIntRange data =
    InputIntRange data



-- PICK CHOICE BUTTONS


type alias PickChoiceButtonsGenericConfig enum =
    { label : String
    , onSelect : enum -> Msg
    , current : enum
    , options : List ( String, enum )
    }


{-| A set of buttons that allows the user to pick from a list of options.
-}
pickChoiceButtons : PickChoiceButtonsGenericConfig enum -> SettingPickerItem
pickChoiceButtons { label, onSelect, current, options } =
    PickChoiceButtons
        { label = label
        , options = List.map (\( optionLabel, value ) -> { label = optionLabel, onSelect = onSelect value, isSelected = value == current }) options
        }



-- PICK CHOICE DROPDOWN


type alias PickChoiceDropdownGenericConfig enum =
    { label : String
    , onSelect : enum -> Msg
    , toString : enum -> String
    , fromString : String -> enum
    , current : enum
    , options : List ( String, enum )
    }


{-| A dropdown that allows the user to pick from a list of options.
-}
pickChoiceDropdown : PickChoiceDropdownGenericConfig enum -> SettingPickerItem
pickChoiceDropdown { label, onSelect, toString, fromString, current, options } =
    PickChoiceDropdown
        { label = label
        , onSelect = fromString >> onSelect
        , options = List.map (\( optionLabel, value ) -> { label = optionLabel, value = toString value, isSelected = value == current }) options
        }



--------------------------------------------------------------------------------
-- PICKER TYPES
--------------------------------------------------------------------------------


{-| A type of a single item in a setting picker

Note: these are NOT constructed directly. Instead, there are specific helper
functions to construct each of these. The reason is because Elm's type
system is a bit limited, and we want to be able to have different types of Enums
stored as items - so my compromise is to use more generic helper functions to convert it
into these types instead.

-}
type SettingPickerItem
    = InputString InputStringConfig
    | InputFloat InputFloatConfig
    | InputInt InputIntConfig
    | InputFloatRange InputFloatRangeConfig
    | InputIntRange InputIntRangeConfig
    | PickChoiceButtons PickChoiceButtonsConfig
    | PickChoiceDropdown PickChoiceDropdownConfig


type alias PickChoiceOptionButton =
    { label : String
    , onSelect : Msg
    , isSelected : Bool
    }


type alias PickChoiceButtonsConfig =
    { label : String
    , options : List PickChoiceOptionButton
    }


type alias PickChoiceDropdownOption =
    { label : String
    , value : String
    , isSelected : Bool
    }


type alias PickChoiceDropdownConfig =
    { label : String
    , onSelect : String -> Msg
    , options : List PickChoiceDropdownOption
    }



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


{-| The view function for a single setting picker item.

Renders each item based on its type. You also have access to the
current settings in this function (as Settings) so can use that
information to make decisions on what to render as well.

-}
viewPickerItem : Settings -> SettingPickerItem -> Html Msg
viewPickerItem settings item =
    case item of
        InputString data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input [ class "setting-picker-item-input setting-picker-item-input-string", type_ "text", value data.value, onInput data.onChange ] []
                ]

        InputFloat data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input
                    [ class "setting-picker-item-input setting-picker-item-input-float"
                    , type_ "number"
                    , value (String.fromFloat data.value)
                    , Html.Attributes.min (String.fromFloat data.min)
                    , Html.Attributes.max (String.fromFloat data.max)
                    , onInput (String.toFloat >> Maybe.withDefault 0.0 >> data.onChange)
                    ]
                    []
                ]

        InputInt data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input
                    [ class "setting-picker-item-input setting-picker-item-input-int"
                    , type_ "number"
                    , value (String.fromInt data.value)
                    , Html.Attributes.min (String.fromInt data.min)
                    , Html.Attributes.max (String.fromInt data.max)
                    , onInput (String.toInt >> Maybe.withDefault 0 >> data.onChange)
                    ]
                    []
                ]

        InputFloatRange data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input-container" ]
                    [ input
                        [ class "setting-picker-item-input setting-picker-item-input-float-range"
                        , type_ "range"
                        , value (String.fromFloat data.value)
                        , Html.Attributes.min (String.fromFloat data.min)
                        , Html.Attributes.max (String.fromFloat data.max)
                        , step (String.fromFloat data.step)
                        , onInput (String.toFloat >> Maybe.withDefault 0.0 >> data.onChange)
                        ]
                        []
                    , div [ class "setting-picker-item-input-value" ] [ text (String.fromFloat data.value) ]
                    ]
                ]

        InputIntRange data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input-container" ]
                    [ input
                        [ class "setting-picker-item-input setting-picker-item-input-int-range"
                        , type_ "range"
                        , value (String.fromInt data.value)
                        , Html.Attributes.min (String.fromInt data.min)
                        , Html.Attributes.max (String.fromInt data.max)
                        , onInput (String.toInt >> Maybe.withDefault 0 >> data.onChange)
                        ]
                        []
                    , div [ class "setting-picker-item-input-value" ] [ text (String.fromInt data.value) ]
                    ]
                ]

        PickChoiceButtons data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input setting-picker-item-input-buttons" ]
                    (List.map
                        (\{ label, onSelect, isSelected } ->
                            button
                                [ class ("setting-picker-item-button setting-picker-item-button-" ++ String.replace " " "-" label)
                                , classList [ ( "selected", isSelected ) ]
                                , onClick onSelect
                                ]
                                [ text label ]
                        )
                        data.options
                    )
                ]

        PickChoiceDropdown data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , select [ class "setting-picker-item-input setting-picker-item-input-select", onInput data.onSelect ]
                    (List.map
                        (\optionData ->
                            option [ value optionData.value, selected optionData.isSelected ] [ text optionData.label ]
                        )
                        data.options
                    )
                ]


{-| View just the picker part of the settings
-}
viewPicker : Settings -> List SettingPickerItem -> Html Msg
viewPicker settings items =
    div [ id "settings-picker" ]
        (List.map (viewPickerItem settings) items)


{-| The function that views all settings which gets called from the Main application.
-}
view : Settings -> Html Msg
view settings =
    div [ id "settings" ]
        [ viewPicker settings (pickers settings)
        ]
