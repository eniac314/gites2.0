module Internals.Helpers exposing (..)


type PluginResult a
    = PluginQuit
    | PluginData a
