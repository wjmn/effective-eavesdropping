module Main exposing (..)

{-| This is the main entrypoint for your game in Elm.

Essentially, this file does the routing to the Settings screen or the Gameplay
screen. There are a few parts of this file you'll probably want to edit from a
text perspective, but most of what you will need to edit from a logic
perspective is actually in the `Settings.elm` and `Game.elm` files.

-}

import Browser
import Common exposing (..)
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Settings exposing (..)



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


{-| The application shows either the Settings screen or the Gameplay screen.

Each of these screens has their own separate model (although when you start
the Gameplay screen, all the current settings given to it are saved into
part of its model).

-}
type Model
    = SettingsScreen Settings
    | GameplayScreen Game


{-| The initial model for the application (when it starts) is the Settings
screen with the default settings.
-}
init : ( Model, Cmd Msg )
init =
    ( SettingsScreen Settings.default, Cmd.none )



--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------


{-| The application can receive messages from the Settings screen or the
Gameplay screen. This function routes those messages to the appropriate
update function for the screen they came from.

There are also two messages that allow transitions between screens:

  - ClickedStartGame (Settings screen => Gameplay screen)
  - ClickedRestart (Gamplay screen => Settings screen)

These are the only two ways to communicate between the Settings screen
and Gameplay screen.

-}
type Msg
    = SettingsMsg Settings.Msg
    | GameplayMsg Game.Msg
    | ClickedStartGame
    | ClickedRestart


{-| Helper function to allow piping of a Cmd Msg into a tuple with a Model.
-}
withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd screen =
    ( screen, cmd )


{-| Helper function to lift the Game Model and Msg into this file's Main model.
-}
mapGameCmd : ( Game, Cmd Game.Msg ) -> ( Model, Cmd Msg )
mapGameCmd ( game, cmd ) =
    ( GameplayScreen game, Cmd.map GameplayMsg cmd )


{-| The update function for the application. This function routes messages
to the appropriate update function for the screen they came from, and also handles
the two messages that allow transitions between screens.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg screen =
    case screen of
        SettingsScreen settings ->
            case msg of
                -- If we get a Settings message, we update the Settings screen as per the update function in Settings.elm.
                SettingsMsg settingsMsg ->
                    SettingsScreen (Settings.update settingsMsg settings)
                        |> withCmd Cmd.none

                -- When the user clicks Start Game, we initialize a new Game with the current settings.
                ClickedStartGame ->
                    Game.init settings
                        |> mapGameCmd

                -- You shouldn't get any Gameplay messages from the Settings screen, but if you do, just return the current screen as-is.
                _ ->
                    screen
                        |> withCmd Cmd.none

        GameplayScreen game ->
            case msg of
                -- If we get a Gameplay message, we update the Gameplay screen as per the update function in Game.elm.
                GameplayMsg gameMsg ->
                    Game.update gameMsg game
                        |> mapGameCmd

                -- When the user clicks Restart, we go back to the Settings screen with the current settings.
                ClickedRestart ->
                    SettingsScreen game.settings
                        |> withCmd Cmd.none

                -- You shouldn't get any Settings messages from the Gameplay screen, but if you do, just return the current screen as-is.
                _ ->
                    screen
                        |> withCmd Cmd.none



--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------


{-| This is the intro text (as HTML) that appears on the Settings screen.
You can modify it to whatever you like, but if you put a lot of text here (e.g.
if you want to put your full rules here), you will probably need to change the
CSS styles for the Settings modal to make it bigger. Keep in mind that the
user will have also been on the game description page, where you can write
long instructions too.

TODO: modify this function to include your own intro text.

-}
introText : Html Msg
introText =
    div [ class "intro-text" ]
        [ p [] [ text "This is the blank game template. Fill this part in with your own intro text." ] ]


{-| The view function for the application. This function mostly just routes the view
to whatever was defined in the Settings.elm view function and the Game.elm view function
(with some wrappers around them).

I've put the Game Title and Team Name in the Settings modal header, but you
can delete these if you don't feel like you need them.

TODO: modify this function to include your own title and team name, or delete the
modal header.

-}
view : Model -> Html Msg
view screen =
    case screen of
        SettingsScreen settings ->
            div [ id "settings-screen", class "screen" ]
                [ div [ id "settings-modal" ]
                    [ div [ id "settings-modal-header" ]
                        [ h1 [ id "settings-modal-header-title" ] [ text "My Game Name" ]
                        , h2 [ id "settings-modal-header-team" ] [ text "My Team Name" ]
                        ]
                    , div [ id "settings-modal-intro" ] [ introText ]
                    , div [ id "settings-modal-body" ] [ Settings.view settings |> Html.map SettingsMsg ]
                    , div [ id "settings-modal-footer" ] [ button [ id "start-game-button", onClick ClickedStartGame ] [ text "Start Game" ] ]
                    ]
                ]

        GameplayScreen game ->
            div [ id "gameplay-screen", class "screen" ]
                [ Game.view game |> Html.map GameplayMsg
                , div [ id "restart-button-container" ] [ button [ id "restart-button", onClick ClickedRestart ] [ text "Restart" ] ]
                ]



--------------------------------------------------------------------------------
-- PROGRAM
--------------------------------------------------------------------------------


{-| The actual main entrypoint to run the application.

If your game isn't too complicated, you probably don't need to modify this at all.

If your game uses advanced features of Elm (e.g. if you need to use Ports to
communicate with JavaScript, or if you need to use subscriptions for events
that trigger automatically), you will need to modify this to include those.
You can ask me if you need any help with this.

-}
main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

