module Main exposing (..)

import Browser
import Browser.Navigation
import List
import Set
import Maybe
import Tuple
import Dict
import Array
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import File
import File.Select
import File.Download
import Json.Decode as JD
import Json.Encode as JE
import Task
import Basics

import AbstractValue
import Utils
import Types exposing (..)
import View exposing (..)
import OrderedSet
import OrderedDict

update msg model =
  case msg of
    MsgSelectTab tab -> ( { model| tab = tab }, Cmd.none)
    MsgChangeAddedEdgeFrom from ->
      let
        oldState = model.state
        oldAddedEdge = oldState.addedEdge
        newAddedEdge =
          {oldAddedEdge
          | from =
            if from == ""
            then Nothing
            else Just from
          }
        newState =
          {oldState
          | addedEdge = newAddedEdge
          }
      in
        ({model| tab = TabEdgeData, state = newState}, Cmd.none)
    MsgChangeAddedEdgeTo to ->
      let
        oldState = model.state
        oldAddedEdge = oldState.addedEdge
        newAddedEdge =
          {oldAddedEdge
          | to =
            if to == ""
            then Nothing
            else Just to
          }
        newState =
          {oldState
          | addedEdge = newAddedEdge
          }
      in
        ({model| tab = TabEdgeData, state = newState}, Cmd.none)
    MsgAddNewEdge ->
      let
        oldState = model.state
        newState =
          { oldState
          | currentEdge = Nothing
          , addedEdge =
            { from = Nothing
            , to = Nothing
            }
          }
      in
        ({model| tab = TabEdgeData, state = newState}, Cmd.none)
    MsgAddEdge edge ->
      let
        oldValue = model.value
        oldEgdes = oldValue.edges
        newEdges =
          if OrderedDict.member edge oldEgdes
          then oldEgdes
          else OrderedDict.prepend edge oldEgdes
        newValue = {oldValue| edges = newEdges}
      in
        update (MsgActivateEdge edge) {model| value = newValue}
    MsgSwitchEdge edge -> update (MsgAddEdge edge) { model| tab = TabEdgeData }
    MsgAddNewStatus ->
      let
        oldState = model.state
      in
        ({ model
        | state = 
          { oldState
          | currentStatus = Nothing
          , addedStatus = ""
          }
        , tab = TabStatusData
        }, Cmd.none)
    MsgAddStatus status ->
      let
        oldValue = model.value
        oldState = model.state
      in
        ({ model
        | value = 
          { oldValue
          | statuses =
            if status == ""
            then oldValue.statuses
            else OrderedDict.prepend status oldValue.statuses
          }
        , state = 
          { oldState
          | currentStatus =
            if status == ""
            then Nothing
            else Just status
          }
        , tab = TabStatusData
        }, Cmd.none)
    MsgAddNewRole ->
      let
        oldState = model.state
      in
        ({ model
        | state = 
          { oldState
          | currentRole = Nothing
          , addedRole = ""
          }
        , tab = TabRoleData
        }, Cmd.none)
    MsgAddRole role ->
      let
        oldValue = model.value
        oldState = model.state
        oldEdgeDataInfo = model.value.edgeDataInfo
      in
        ({ model
        | value = 
          { oldValue
          | edgeDataInfo = { oldEdgeDataInfo
            | roles =
              if role == ""
              then oldValue.edgeDataInfo.roles
              else OrderedDict.prepend role oldValue.edgeDataInfo.roles
            }
          }
        , state = 
          { oldState
          | currentRole =
            if role == ""
            then Nothing
            else Just role
          }
        , tab = TabRoleData
        }, Cmd.none)
    MsgAddNewValidator ->
      let
        oldState = model.state
      in
        ({ model
        | state = 
          { oldState
          | currentValidator = Nothing
          , addedValidator = ""
          }
        , tab = TabValidatorData
        }, Cmd.none)
    MsgAddValidator validator ->
      let
        oldValue = model.value
        oldState = model.state
        oldEdgeDataInfo = model.value.edgeDataInfo
      in
        ({ model
        | value = 
          { oldValue
          | edgeDataInfo = { oldEdgeDataInfo
            | validators =
              if validator == ""
              then oldValue.edgeDataInfo.validators
              else OrderedDict.prepend validator oldValue.edgeDataInfo.validators
            }
          }
        , state = 
          { oldState
          | currentValidator =
            if validator == ""
            then Nothing
            else Just validator
          }
        , tab = TabValidatorData
        }, Cmd.none)
    MsgAddNewModificator ->
      let
        oldState = model.state
      in
        ({ model
        | state = 
          { oldState
          | currentModificator = Nothing
          , addedModificator = ""
          }
        , tab = TabModificatorData
        }, Cmd.none)
    MsgAddModificator modificator ->
      let
        oldValue = model.value
        oldState = model.state
        oldEdgeDataInfo = model.value.edgeDataInfo
      in
        ({ model
        | value = 
          { oldValue
          | edgeDataInfo = { oldEdgeDataInfo
            | modificators =
              if modificator == ""
              then oldValue.edgeDataInfo.modificators
              else OrderedDict.prepend modificator oldValue.edgeDataInfo.modificators
            }
          }
        , state = 
          { oldState
          | currentModificator =
            if modificator == ""
            then Nothing
            else Just modificator
          }
        , tab = TabModificatorData
        }, Cmd.none)
    MsgChangeRoleInEdge edge role value ->
      let
        oldEdgeData = OrderedDict.get edge model.value.edges |> Maybe.withDefault emptyEdgeData
        newEdgeData =
          { oldEdgeData
          | roles =
              if value
              then Set.insert role oldEdgeData.roles
              else Set.remove role oldEdgeData.roles
          }
        newEdges = OrderedDict.setValue edge newEdgeData model.value.edges
        oldValue = model.value
        newValue = {oldValue| edges = newEdges}
      in
        ({model| value = newValue}, Cmd.none)
    MsgChangeValidatorInEdge edge validator ->
      let
        oldEdgeData = OrderedDict.get edge model.value.edges |> Maybe.withDefault emptyEdgeData
        newEdgeData =
          { oldEdgeData
          | validator = validator
          }
        newEdges = OrderedDict.setValue edge newEdgeData model.value.edges
        oldValue = model.value
        newValue = {oldValue| edges = newEdges}
      in
        ({model| value = newValue}, Cmd.none)
    MsgChangeModificatorInEdge edge modificator ->
      let
        oldEdgeData = OrderedDict.get edge model.value.edges |> Maybe.withDefault emptyEdgeData
        newEdgeData =
          { oldEdgeData
          | modificator = modificator
          }
        newEdges = OrderedDict.setValue edge newEdgeData model.value.edges
        oldValue = model.value
        newValue = {oldValue| edges = newEdges}
      in
        ({model| value = newValue}, Cmd.none)
    MsgChangeDescriptionInEdge edge description ->
      let
        oldEdgeData = OrderedDict.get edge model.value.edges |> Maybe.withDefault emptyEdgeData
        newEdgeData =
          { oldEdgeData
          | description = description
          }
        newEdges = OrderedDict.setValue edge newEdgeData model.value.edges
        oldValue = model.value
        newValue = {oldValue| edges = newEdges}
      in
        ({model| value = newValue}, Cmd.none)
    MsgChangeAddedStatus status ->
      let
        oldState = model.state
      in
        ({model| state = {oldState| addedStatus = status}}, Cmd.none)
    MsgChangeAddedRole role ->
      let
        oldState = model.state
      in
        ({model| state = {oldState| addedRole = role}}, Cmd.none)
    MsgChangeAddedValidator validator ->
      let
        oldState = model.state
      in
        ({model| state = {oldState| addedValidator = validator}}, Cmd.none)
    MsgChangeAddedModificator modificator ->
      let
        oldState = model.state
      in
        ({model| state = {oldState| addedModificator = modificator}}, Cmd.none)
    MsgActivateEdge edge ->
      let
        oldState = model.state
        newState = {oldState| currentEdge = Just edge}
      in
        ({model| state = newState}, Cmd.none)
    MsgActivateStatus status ->
      let
        oldState = model.state
        newState = {oldState| currentStatus = Just status}
      in
        ({model| state = newState}, Cmd.none)
    MsgActivateRole role ->
      let
        oldState = model.state
        newState = {oldState| currentRole = Just role}
      in
        ({model| state = newState}, Cmd.none)
    MsgActivateValidator validator ->
      let
        oldState = model.state
        newState = {oldState| currentValidator = Just validator}
      in
        ({model| state = newState}, Cmd.none)
    MsgActivateModificator modificator ->
      let
        oldState = model.state
        newState = {oldState| currentModificator = Just modificator}
      in
        ({model| state = newState}, Cmd.none)
    MsgChangeLabelInStatus status label ->
      let
        oldValue = model.value
        oldStatusData = OrderedDict.get status oldValue.statuses |> Maybe.withDefault Types.emptyStatusData
        newValue =
          { oldValue
          | statuses = OrderedDict.setValue status {oldStatusData| label = label} oldValue.statuses
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeDescriptionInStatus status description ->
      let
        oldValue = model.value
        oldStatusData = OrderedDict.get status oldValue.statuses |> Maybe.withDefault Types.emptyStatusData
        newValue =
          { oldValue
          | statuses = OrderedDict.setValue status {oldStatusData| description = description} oldValue.statuses
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeLabelInRole role label ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
        oldRoleData = OrderedDict.get role oldEdgeDataInfo.roles |> Maybe.withDefault Types.emptyRoleData
        newEdgeDataInfo =
          { oldEdgeDataInfo
          | roles = OrderedDict.setValue role {oldRoleData| label = label} oldEdgeDataInfo.roles
          }
        newValue =
          { oldValue
          | edgeDataInfo = newEdgeDataInfo
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeDescriptionInRole role description ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
        oldRoleData = OrderedDict.get role oldEdgeDataInfo.roles |> Maybe.withDefault Types.emptyRoleData
        newEdgeDataInfo =
          { oldEdgeDataInfo
          | roles = OrderedDict.setValue role {oldRoleData| description = description} oldEdgeDataInfo.roles
          }
        newValue =
          { oldValue
          | edgeDataInfo = newEdgeDataInfo
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeLabelInValidator validator label ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
        oldValidatorData = OrderedDict.get validator oldEdgeDataInfo.validators |> Maybe.withDefault Types.emptyRoleData
        newEdgeDataInfo =
          { oldEdgeDataInfo
          | validators = OrderedDict.setValue validator {oldValidatorData| label = label} oldEdgeDataInfo.validators
          }
        newValue =
          { oldValue
          | edgeDataInfo = newEdgeDataInfo
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeDescriptionInValidator validator description ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
        oldValidatorData = OrderedDict.get validator oldEdgeDataInfo.validators |> Maybe.withDefault Types.emptyRoleData
        newEdgeDataInfo =
          { oldEdgeDataInfo
          | validators = OrderedDict.setValue validator {oldValidatorData| description = description} oldEdgeDataInfo.validators
          }
        newValue =
          { oldValue
          | edgeDataInfo = newEdgeDataInfo
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeLabelInModificator modificator label ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
        oldModificatorData = OrderedDict.get modificator oldEdgeDataInfo.modificators |> Maybe.withDefault Types.emptyRoleData
        newEdgeDataInfo =
          { oldEdgeDataInfo
          | modificators = OrderedDict.setValue modificator {oldModificatorData| label = label} oldEdgeDataInfo.modificators
          }
        newValue =
          { oldValue
          | edgeDataInfo = newEdgeDataInfo
          }
      in ({model| value = newValue}, Cmd.none)
    MsgChangeDescriptionInModificator modificator description ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
        oldModificatorData = OrderedDict.get modificator oldEdgeDataInfo.modificators |> Maybe.withDefault Types.emptyRoleData
        newEdgeDataInfo =
          { oldEdgeDataInfo
          | modificators = OrderedDict.setValue modificator {oldModificatorData| description = description} oldEdgeDataInfo.modificators
          }
        newValue =
          { oldValue
          | edgeDataInfo = newEdgeDataInfo
          }
      in ({model| value = newValue}, Cmd.none)
    MsgMoveEdgeStart edge ->
      let
        oldValue = model.value
      in
        ( {model
        | value = 
          { oldValue
          | edges =
            if OrderedDict.member edge model.value.edges
            then OrderedDict.prepend edge model.value.edges
            else model.value.edges
          }
        }, Cmd.none)
    MsgMoveEdgeEnd edge ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | edges =
            if OrderedDict.member edge model.value.edges
            then OrderedDict.append edge model.value.edges
            else model.value.edges
          }
        }, Cmd.none)
    MsgMoveEdgeUp edge ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | edges =
            if OrderedDict.member edge model.value.edges
            then OrderedDict.moveUp edge model.value.edges
            else model.value.edges
          }
        }, Cmd.none)
    MsgMoveEdgeDown edge ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | edges =
            if OrderedDict.member edge model.value.edges
            then OrderedDict.moveDown edge model.value.edges
            else model.value.edges
          }
        }, Cmd.none)
    MsgClearEdge edge ->
      let
        oldValue = model.value
      in ( { model
        | value = { oldValue
          | edges = OrderedDict.setValue edge emptyEdgeData model.value.edges
          }
      }, Cmd.none )
    MsgRemoveEdge edge ->
      let
        oldValue = model.value
      in ( { model
        | tab = TabEdges
        , value = { oldValue
        | edges = OrderedDict.remove edge model.value.edges
        }
      }, Cmd.none )
    MsgSelectEdge edge value ->
      let
        oldState = model.state
      in
        ({model| state =
          { oldState
          | selectedEdges =
            (
              if value
              then Set.insert
              else Set.remove
            ) edge oldState.selectedEdges
          }
        }, Cmd.none)
    MsgMoveStatusStart status ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | statuses =
            if OrderedDict.member status model.value.statuses
            then OrderedDict.prepend status model.value.statuses
            else model.value.statuses
          }
        }, Cmd.none)
    MsgMoveStatusEnd status ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | statuses =
            if OrderedDict.member status model.value.statuses
            then OrderedDict.append status model.value.statuses
            else model.value.statuses
          }
        }, Cmd.none)
    MsgMoveStatusUp status ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | statuses =
            if OrderedDict.member status model.value.statuses
            then OrderedDict.moveUp status model.value.statuses
            else model.value.statuses
          }
        }, Cmd.none)
    MsgMoveStatusDown status ->
      let
        oldValue = model.value
      in
        ({ model
        | value = 
          { oldValue
          | statuses =
            if OrderedDict.member status model.value.statuses
            then OrderedDict.moveDown status model.value.statuses
            else model.value.statuses
          }
        }, Cmd.none)
    MsgClearStatus status ->
      let
        oldValue = model.value
      in ( { model
        | value =
          { oldValue
          | statuses = OrderedDict.setValue status emptyStatusData model.value.statuses
          }
      }, Cmd.none )
    MsgRemoveStatus status ->
      let
        oldValue = model.value
      in ( { model
        | tab = TabStatuses
        , value =
          { oldValue
          | statuses = OrderedDict.remove status model.value.statuses
          }
      }, Cmd.none )
    MsgStartRenameStatus status ->
      let
        oldState = model.state
      in
        ({ model
        | tab = TabRenameStatus
        , state =
          { oldState
          | addedStatus = status
          }
        }, Cmd.none)
    MsgRenameStatus from to ->
      let
        oldState = model.state
        oldValue = model.value
        data = OrderedDict.get from model.value.statuses
        -- TODO оптимизировать замену всех ключей
        changedEdges = 
          model.value.edges
          |> OrderedDict.keys
          |> List.filter (\(f, t)-> (f == from) || (t == from))
        changer (f, t) edges =
          OrderedDict.replaceKey (f, t) (if f == from then to else f, if t == from then to else t) edges
        newEdges = List.foldl changer model.value.edges changedEdges
      in
        ({ model
        | tab = TabStatusData
        , state =
          { oldState
          | currentStatus = Just to
          }
        , value =
          { oldValue
          | statuses = OrderedDict.replaceKey from to model.value.statuses
          , edges = newEdges
          }
        }, Cmd.none)
    MsgMoveRoleStart role ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles =
                if OrderedDict.member role model.value.edgeDataInfo.roles
                then OrderedDict.prepend role model.value.edgeDataInfo.roles
                else model.value.edgeDataInfo.roles
            }
          }
        }, Cmd.none )
    MsgMoveRoleEnd role ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles =
                if OrderedDict.member role model.value.edgeDataInfo.roles
                then OrderedDict.append role model.value.edgeDataInfo.roles
                else model.value.edgeDataInfo.roles
            }
          }
        }, Cmd.none )
    MsgMoveRoleUp role ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles =
                if OrderedDict.member role model.value.edgeDataInfo.roles
                then OrderedDict.moveUp role model.value.edgeDataInfo.roles
                else model.value.edgeDataInfo.roles
            }
          }
        }, Cmd.none )
    MsgMoveRoleDown role ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles =
                if OrderedDict.member role model.value.edgeDataInfo.roles
                then OrderedDict.moveDown role model.value.edgeDataInfo.roles
                else model.value.edgeDataInfo.roles
            }
          }
        }, Cmd.none )
    MsgClearRole role ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in ( { model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles = OrderedDict.setValue role emptyRoleData model.value.edgeDataInfo.roles
            }
          }
      }, Cmd.none )
    MsgRemoveRole role ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in ( { model
        | tab = TabRoles
        , value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles = OrderedDict.remove role model.value.edgeDataInfo.roles
            }
            
          }
      }, Cmd.none )
    MsgStartRenameRole role ->
      let
        oldState = model.state
      in
        ({ model
        | tab = TabRenameRole
        , state =
          { oldState
          | addedRole = role
          }
        }, Cmd.none)
    MsgRenameRole from to ->
      let
        oldState = model.state
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        data = OrderedDict.get from model.value.edgeDataInfo.roles
      in
        ({ model
        | tab = TabRoleData
        , state =
          { oldState
          | currentRole = Just to
          }
         , value =
           { oldValue
           | edgeDataInfo =
             { oldEdgeDataInfo
             | roles = OrderedDict.replaceKey from to model.value.edgeDataInfo.roles
             }
           , edges = OrderedDict.map (
             \_ ed ->
               { ed
               | roles =
                 if Set.member from ed.roles
                 then
                  ed.roles
                  |> Set.remove from
                  |> Set.insert to
                 else ed.roles
               }
           ) model.value.edges
           }
        }, Cmd.none)
    MsgMoveValidatorStart validator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators =
                if OrderedDict.member validator model.value.edgeDataInfo.validators
                then OrderedDict.prepend validator model.value.edgeDataInfo.validators
                else model.value.edgeDataInfo.validators
            }
          }
        }, Cmd.none )
    MsgMoveValidatorEnd validator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators =
                if OrderedDict.member validator model.value.edgeDataInfo.validators
                then OrderedDict.append validator model.value.edgeDataInfo.validators
                else model.value.edgeDataInfo.validators
            }
          }
        }, Cmd.none )
    MsgMoveValidatorUp validator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators =
                if OrderedDict.member validator model.value.edgeDataInfo.validators
                then OrderedDict.moveUp validator model.value.edgeDataInfo.validators
                else model.value.edgeDataInfo.validators
            }
          }
        }, Cmd.none )
    MsgMoveValidatorDown validator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators =
                if OrderedDict.member validator model.value.edgeDataInfo.validators
                then OrderedDict.moveDown validator model.value.edgeDataInfo.validators
                else model.value.edgeDataInfo.validators
            }
          }
        }, Cmd.none )
    MsgClearValidator validator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in ( { model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators = OrderedDict.setValue validator emptyValidatorData model.value.edgeDataInfo.validators
            }
          }
      }, Cmd.none )
    MsgRemoveValidator validator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in ( { model
        | tab = TabValidators
        , value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators = OrderedDict.remove validator model.value.edgeDataInfo.validators
            }
          }
      }, Cmd.none )
    MsgStartRenameValidator validator ->
      let
        oldState = model.state
      in
        ({ model
        | tab = TabRenameValidator
        , state =
          { oldState
          | addedValidator = validator
          }
        }, Cmd.none)
    MsgRenameValidator from to ->
      let
        oldState = model.state
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        data = OrderedDict.get from model.value.edgeDataInfo.validators
      in
        ({ model
        | tab = TabValidatorData
        , state =
          { oldState
          | currentValidator = Just to
          }
         , value =
           { oldValue
           | edgeDataInfo =
             { oldEdgeDataInfo
             | validators = OrderedDict.replaceKey from to model.value.edgeDataInfo.validators
             }
           , edges = OrderedDict.map (
             \_ ed ->
               if ed.validator == Just from
               then { ed| validator = Just to }
               else ed
           ) model.value.edges
           }
        }, Cmd.none)
    MsgMoveModificatorStart modificator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators =
                if OrderedDict.member modificator model.value.edgeDataInfo.modificators
                then OrderedDict.prepend modificator model.value.edgeDataInfo.modificators
                else model.value.edgeDataInfo.modificators
            }
          }
        }, Cmd.none )
    MsgMoveModificatorEnd modificator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators =
                if OrderedDict.member modificator model.value.edgeDataInfo.modificators
                then OrderedDict.append modificator model.value.edgeDataInfo.modificators
                else model.value.edgeDataInfo.modificators
            }
          }
        }, Cmd.none )
    MsgMoveModificatorUp modificator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators =
                if OrderedDict.member modificator model.value.edgeDataInfo.modificators
                then OrderedDict.moveUp modificator model.value.edgeDataInfo.modificators
                else model.value.edgeDataInfo.modificators
            }
          }
        }, Cmd.none )
    MsgMoveModificatorDown modificator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in
        ( { model
        | value = 
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators =
                if OrderedDict.member modificator model.value.edgeDataInfo.modificators
                then OrderedDict.moveDown modificator model.value.edgeDataInfo.modificators
                else model.value.edgeDataInfo.modificators
            }
          }
        }, Cmd.none )
    MsgClearModificator modificator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in ( { model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators = OrderedDict.setValue modificator emptyModificatorData model.value.edgeDataInfo.modificators
            }
          }
      }, Cmd.none )
    MsgRemoveModificator modificator ->
      let
        oldValue = model.value
        oldEdgeDataInfo = oldValue.edgeDataInfo
      in ( { model
        | tab = TabModificatirs
        , value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators = OrderedDict.remove modificator model.value.edgeDataInfo.modificators
            }
          }
      }, Cmd.none )
    MsgStartRenameModificator modificator ->
      let
        oldState = model.state
      in
        ({ model
        | tab = TabRenameModificator
        , state =
          { oldState
          | addedModificator = modificator
          }
        }, Cmd.none)
    MsgRenameModificator from to ->
      let
        oldState = model.state
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        data = OrderedDict.get from model.value.edgeDataInfo.modificators
      in
        ({ model
        | tab = TabModificatorData
        , state =
          { oldState
          | currentStatus = Just to
          }
         , value =
           { oldValue
           | edgeDataInfo =
             { oldEdgeDataInfo
             | modificators = OrderedDict.replaceKey from to model.value.edgeDataInfo.modificators
             }
           , edges = OrderedDict.map (
             \_ ed ->
               if ed.modificator == Just from
               then { ed| modificator = Just to }
               else ed
           ) model.value.edges
           }
        }, Cmd.none)
    MsgSelectStatus status value ->
      let
        oldState = model.state
      in
        ({model| state =
          { oldState
          | selectedStatuses =
            (
              if value
              then Set.insert
              else Set.remove
            ) status oldState.selectedStatuses
          }
        }, Cmd.none)
    MsgSelectRole role value ->
      let
        oldState = model.state
      in
        ({model| state =
          { oldState
          | selectedRoles =
            (
              if value
              then Set.insert
              else Set.remove
            ) role oldState.selectedRoles
          }
        }, Cmd.none)
    MsgSelectValidator validator value ->
      let
        oldState = model.state
      in
        ({model| state =
          { oldState
          | selectedValidators =
            (
              if value
              then Set.insert
              else Set.remove
            ) validator oldState.selectedValidators
          }
        }, Cmd.none)
    MsgSelectModificator modificator value ->
      let
        oldState = model.state
      in
        ({model| state =
          { oldState
          | selectedModificators =
            (
              if value
              then Set.insert
              else Set.remove
            ) modificator oldState.selectedModificators
          }
        }, Cmd.none)
    
    MsgChangeImageInStatus status value ->
      let
        oldValue = model.value
        oldStatusData = OrderedDict.get status model.value.statuses |> Maybe.withDefault emptyStatusData
      in
        ( { model
        | value = { oldValue
          | statuses = OrderedDict.setValue status { oldStatusData
            | image = value
            } model.value.statuses
          }
        }, Cmd.none)
    MsgLoadImageInStatusFromFile status ->
      (model, File.Select.file ["image/*"] (MsgSelectImageFileInStatus status))
    MsgSelectImageFileInStatus status file->
      (model, Task.perform (\v->MsgChangeImageInStatus status (Just v)) ( File.toUrl file))
     
    MsgFileLoaded str -> case Utils.parseJSON str of
      Ok value -> ({model| value = value}, Cmd.none)
      Err error -> ({model| json = JD.errorToString error}, Cmd.none)
    MsgLoadFromFile ->
      (model, File.Select.file ["application/json"] MsgFileSelected)
    MsgFileSelected file->
      (model, Task.perform MsgFileLoaded ( File.toString file))
    
    MsgLoadFromUrl url -> ({model| tab = TabUtils}, Http.get
      { url = url
      , expect = Http.expectString MsgUrlLoaded
      })
    MsgUrlLoaded res -> case res of
      Ok str -> update (MsgFileLoaded str) model
      Err err ->
        let
          alert = case err of
            Http.BadUrl str -> "Битая ссылка " ++ str
            Http.Timeout -> "Таймаут"
            Http.NetworkError -> "Ошибка сети"
            Http.BadStatus st -> "Статус " ++ JE.encode 0 (JE.int st)
            Http.BadBody str -> "Невалидный рерультат " ++ str
        in ({ model| json = alert}, Cmd.none)

    MsgMoveSelectedEdgesStart ->
      let
        oldValue = model.value
        list =
          model.value.edges
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedEdges)
        newEdges = List.foldr OrderedDict.prepend model.value.edges list
      in
        ({ model
        | value =
          { oldValue
          | edges = newEdges 
          }
        }, Cmd.none)
    MsgMoveSelectedEdgesEnd ->
      let
        oldValue = model.value
        list =
          model.value.edges
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedEdges)
        newEdges = List.foldl OrderedDict.append model.value.edges list
      in
        ({ model
        | value =
          { oldValue
          | edges = newEdges 
          }
        }, Cmd.none)
    MsgMoveSelectedStatusesStart ->
      let
        oldValue = model.value
        list =
          model.value.statuses
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedStatuses)
        newStatuses = List.foldr OrderedDict.prepend model.value.statuses list
      in
        ({ model
        | value =
          { oldValue
          | statuses = newStatuses
          }
        }, Cmd.none)
    MsgMoveSelectedStatusesEnd ->
      let
        oldValue = model.value
        list =
          model.value.statuses
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedStatuses)
        newStatuses = List.foldl OrderedDict.append model.value.statuses list
      in
        ({ model
        | value =
          { oldValue
          | statuses = newStatuses
          }
        }, Cmd.none)
    MsgMoveSelectedRolesStart ->
      let
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        list =
          model.value.edgeDataInfo.roles
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedRoles)
        newRoles = List.foldr OrderedDict.prepend model.value.edgeDataInfo.roles list
      in
        ({ model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles = newRoles
            }
          }
        }, Cmd.none)
    MsgMoveSelectedRolesEnd ->
      let
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        list =
          model.value.edgeDataInfo.roles
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedRoles)
        newRoles = List.foldl OrderedDict.append model.value.edgeDataInfo.roles list
      in
        ({ model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | roles = newRoles
            }
          }
        }, Cmd.none)
    MsgMoveSelectedValidatorsStart ->
      let
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        list =
          model.value.edgeDataInfo.validators
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedValidators)
        newValidators = List.foldr OrderedDict.prepend model.value.edgeDataInfo.validators list
      in
        ({ model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators = newValidators
            }
          }
        }, Cmd.none)
    MsgMoveSelectedValidatorsEnd ->
      let
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        list =
          model.value.edgeDataInfo.validators
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedValidators)
        newValidators = List.foldl OrderedDict.append model.value.edgeDataInfo.validators list
      in
        ({ model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | validators = newValidators
            }
          }
        }, Cmd.none)
    MsgMoveSelectedModificatorsStart ->
      let
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        list =
          model.value.edgeDataInfo.modificators
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedModificators)
        newModificators = List.foldr OrderedDict.prepend model.value.edgeDataInfo.modificators list
      in
        ({ model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators = newModificators
            }
          }
        }, Cmd.none)
    MsgMoveSelectedModificatorsEnd ->
      let
        oldValue = model.value
        oldEdgeDataInfo = model.value.edgeDataInfo
        list =
          model.value.edgeDataInfo.modificators
          |> OrderedDict.keys
          |> List.filter (\e -> Set.member e model.state.selectedModificators)
        newModificators = List.foldl OrderedDict.append model.value.edgeDataInfo.modificators list
      in
        ({ model
        | value =
          { oldValue
          | edgeDataInfo =
            { oldEdgeDataInfo
            | modificators = newModificators
            }
          }
        }, Cmd.none)
    MsgShowJSON -> ({model| json = Utils.generateJSON model}, Cmd.none)
    MsgClearJSON -> ({model| json = ""}, Cmd.none)
    MsgClearValue -> ({model| value = emptyValue}, Cmd.none)
    MsgChangeLoadedUrl url ->
      let
        oldState = model.state
      in ({model| state = {oldState| loadedUrl = url}}, Cmd.none)
    --MsgSeveToFile -> (model, Browser.Navigation.load <| "data:application/octed-stream," ++ Utils.generateJSON model)
    _ -> (model, Cmd.none)

main =
  Browser.element
    { init = init
    --  (init, Http.get
    --    { url = "statuses.json"
    --    , expect = Http.expectString MsgUrlLoaded
    --    }
    --  )
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
