module View exposing (..)

import List
import Set
import Maybe
import Tuple
import Dict
import Array

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Browser.Navigation

import Json.Decode as JD
import Json.Encode as JE

import AbstractValue
import Types exposing (..)
import OrderedSet
import OrderedDict
import Help
import Utils

drawTabs model = div [] 
  [ button
    [ onClick (MsgSelectTab TabUtils)
    , title "инструменты"
    , class "navigation-btn"
    ] [text "🔧"]
  , button
    [ onClick (MsgSelectTab TabEdges)
    , title "переходы"
    , class "navigation-btn"
    ] [text "↔️"]
  , button
    [ onClick (MsgSelectTab TabStatuses)
    , title "статусы"
    , class "navigation-btn"
    ] [text "🔖"]
  , button
    [ onClick (MsgSelectTab TabRoles)
    , title "роли"
    , class "navigation-btn"
    ] [text "🔐"]
  , button
    [ onClick (MsgSelectTab TabValidators)
    , title "валидаторы"
    , class "navigation-btn"
    ] [text "🗝"]
  , button
    [ onClick (MsgSelectTab TabModificatirs)
    , title "модификаторы"
    , class "navigation-btn"
    ] [text "⚙️"]
  , button
    [ onClick (MsgSelectTab TabAnalytics)
    , title "аналитика"
    , class "navigation-btn"
    ] [text "🧮"]
  , button
    [ onClick (MsgSelectTab TabConfig)
    , title "настройки"
    , class "navigation-btn"
    ] [text "🗜"]
  , button
    [ onClick (MsgSelectTab TabHelp)
    , title "справочник"
    , class "navigation-btn"
    ] [text "❓"]
  ]

drawMarkedEdge model edge =
  let
    (from, to) = edge
    edgeStyle = 
      if model.state.currentEdge == Just edge
      then [class "current-edge"]
      else []
    edgeEvents =
      [ onClick (
        if model.state.currentEdge == Just edge
        then MsgSelectTab TabEdgeData
        else MsgActivateEdge edge
      )
      ]
  in
    li []
      [ input
        [ type_ "checkbox"
        , checked (Set.member edge model.state.selectedEdges)
        , onCheck <| MsgSelectEdge edge
        ] []
      , span [] [
        span (edgeStyle ++ edgeEvents)
          [ span [] <| List.singleton <| text <| (
            if model.config.preferLabel
            then
              model.value.statuses
              |> OrderedDict.get from
              |> Maybe.andThen .label
              |> Maybe.withDefault from
            else from
          )
          , text " -> "
          , span [] <| List.singleton <| text <| (
            if model.config.preferLabel
            then
              model.value.statuses
              |> OrderedDict.get to
              |> Maybe.andThen .label
              |> Maybe.withDefault to
            else to
          )
          ]
        ]
      ]
drawEdge model (from, to) = li
  [ onClick <| MsgEditEdge (from, to)]
  [ span [] <| List.singleton <| text <| (
    if model.config.preferLabel
    then
      model.value.statuses
      |> OrderedDict.get from
      |> Maybe.andThen .label
      |> Maybe.withDefault from
    else from
  )
  , text " -> "
  , span [] <| List.singleton <| text <| (
    if model.config.preferLabel
    then
      model.value.statuses
      |> OrderedDict.get to
      |> Maybe.andThen .label
      |> Maybe.withDefault to
    else to
  )
  ]

drawStatus model status =
  let
    attrs =
      if model.state.currentStatus ==  Just status
      then
        [ class "current-status"
        , onClick <| MsgSelectTab TabStatusData
        ]
      else [onClick <| MsgActivateStatus status]
  in
    li []
      [ input
        [ type_ "checkbox"
        , checked (Set.member status model.state.selectedStatuses)
        , onCheck <| MsgSelectStatus status
        ] []
      , span attrs [text status]
      ]
drawRole model role =
  let
    attrs =
      if model.state.currentRole ==  Just role
      then
        [ class "current-role"
        , onClick <| MsgSelectTab TabRoleData
        ]
      else [onClick <| MsgActivateRole role]
  in
    li []
      [ input
        [ type_ "checkbox"
        , checked (Set.member role model.state.selectedRoles)
        , onCheck <| MsgSelectRole role
        ] []
      , span attrs [text role]
      ]
drawValidator model validator =
  let
    attrs =
      if model.state.currentValidator ==  Just validator
      then
        [ class "current-validator"
        , onClick <| MsgSelectTab TabValidatorData
        ]
      else [onClick <| MsgActivateValidator validator]
  in
    li []
      [ input
        [ type_ "checkbox"
        , checked (Set.member validator model.state.selectedValidators)
        , onCheck <| MsgSelectValidator validator
        ] []
      , span attrs [text validator]
      ]
drawModificator model modificator =
  let
    attrs =
      if model.state.currentModificator ==  Just modificator
      then
        [ class "current-modificator"
        , onClick <| MsgSelectTab TabModificatorData
        ]
      else [onClick <| MsgActivateModificator modificator]
  in
    li []
      [ input
        [ type_ "checkbox"
        , checked (Set.member modificator model.state.selectedModificators)
        , onCheck <| MsgSelectModificator modificator
        ] []
      , span attrs [text modificator]
      ]

drawContent model = case model.tab of
  TabUtils -> div []
    [ button [onClick <| MsgSelectTab TabLoadedUrl] [text "из ссылки"]
    , button [onClick MsgLoadFromFile] [text "загрузить"]
    , a
      [ download model.config.filename
      , href <| "data:text/json," ++ Utils.generateJSON model
      ] [button [] [text "сохранить"]]
    -- , button [onClick MsgSeveToFile] [text "сохранить"]
    , button [onClick MsgShowJSON] [text "JSON"]
    , button [onClick MsgClearValue] [text "сбросить"]
    , br [] []
    , pre [] [text model.json]
    , br [] []
    , button [onClick MsgClearJSON] [text "очистить"]
    ]
  TabEdges ->
    div []
      [ h1 [] [text "Переходы"]
      , button [onClick MsgAddNewEdge] [text "добавить"]
      , ul [] <| List.map  (drawMarkedEdge model) <| OrderedDict.keys model.value.edges
      , button
        [ class "navigation-btn"
        , title "вначало"
        , disabled <| Set.isEmpty model.state.selectedEdges
        , onClick MsgMoveSelectedEdgesStart
        ] [text "🔼"]
      , button
        [ class "navigation-btn"
        , title "вначало"
        , disabled <| Set.isEmpty model.state.selectedEdges
        , onClick MsgMoveSelectedEdgesEnd
        ] [text "🔽"]
      , button
        [ class "navigation-btn"
        , title "поднять"
        , disabled <| (model.state.currentEdge == Nothing) || (model.state.currentEdge == List.head (OrderedDict.keys model.value.edges))
        , onClick <| (
          case model.state.currentEdge of
            Just edge -> MsgMoveEdgeUp edge
            Nothing -> MsgNone
        )
        ] [text "⬆️"]
      , button
        [ class "navigation-btn"
        , title "опустить"
        , disabled <| (model.state.currentEdge == Nothing) || (model.state.currentEdge == List.head (List.reverse (OrderedDict.keys model.value.edges)))
        , onClick <| (
          case model.state.currentEdge of
            Just edge -> MsgMoveEdgeDown edge
            Nothing -> MsgNone
        )
        ] [text "⬇️"]
      , button
        [ class "navigation-btn"
        , title "в начало"
        , disabled <| (model.state.currentEdge == Nothing) || (model.state.currentEdge == List.head (OrderedDict.keys model.value.edges))
        , onClick <| (
          case model.state.currentEdge of
            Just edge -> MsgMoveEdgeStart edge
            Nothing -> MsgNone
        )
        ] [text "⏫"]
      , button
        [ class "navigation-btn"
        , title "в конец"
        , disabled <| (model.state.currentEdge == Nothing) || (model.state.currentEdge == List.head (List.reverse (OrderedDict.keys model.value.edges)))
        , onClick <| (
          case model.state.currentEdge of
            Just edge -> MsgMoveEdgeEnd edge
            Nothing -> MsgNone
        )
        ] [text "⏬"]
      , button
        [ class "navigation-btn"
        , title "очистить"
        , disabled <| model.state.currentEdge == Nothing
        , onClick <| (
          case model.state.currentEdge of
            Just edge -> MsgResetCurrentEdge
            Nothing -> MsgNone
        )
        ] [text "❎"]
      , button
        [ class "navigation-btn"
        , title "удалить"
        , disabled <| model.state.currentEdge == Nothing
        , onClick <| (
          case model.state.currentEdge of
            Just edge -> MsgRemoveEdge edge
            Nothing -> MsgNone
        )
        ] [text "🚽"]
      ]
  TabEdgeData -> case model.state.currentEdge of
    Just (from, to) ->
      let
        statuses = model.value.statuses |> OrderedDict.keys
        statusesChangeFrom =
          if List.member from statuses
          then statuses
          else from :: statuses
        statusesChangeTo =
          if List.member to statuses
          then statuses
          else to :: statuses
        statusesSwitchBackword = model.value.edges
          |> OrderedDict.keys
          |> List.filter (\(f, t)-> t == from)
          |> List.map Tuple.first
        statusesSwitchForword = model.value.edges
          |> OrderedDict.keys
          |> List.filter (\(f, t)-> f == to)
          |> List.map Tuple.second
        statusesAddBackword = List.filter (\s->not <| List.member s statusesSwitchBackword) statuses
        statusesAddForword = List.filter (\s->not <| List.member s statusesSwitchForword) statuses
        statusToOption status = option [value status] [text status]
        emptyOption = option [selected True, value "", disabled True] [text "---"]
        optionsChangeFrom = List.map
          ( \status->
            option [value status, selected (status == from)] [text status]
          ) statusesChangeFrom
        optionsChangeTo = List.map
          ( \status->
            option [value status, selected (status == to)] [text status]
          ) statusesChangeTo
        optionsSwitchBackword = emptyOption :: List.map statusToOption statusesSwitchBackword
        optionsSwitchForword = emptyOption :: List.map statusToOption statusesSwitchForword
        optionsAddBackword = emptyOption :: List.map statusToOption statusesAddBackword
        optionsAddForword = emptyOption :: List.map statusToOption statusesAddForword
        send  (f, t) =
          if f == "" || t == ""
          then MsgNone
          else MsgAddEdge (f, t)
        roles = OrderedDict.keys model.value.edgeDataInfo.roles
        edgeData = OrderedDict.get (from, to) model.value.edges |> Maybe.withDefault emptyEdgeData
        viewRoles = List.map (
          \role->
            let
              hasRole = Set.member role edgeData.roles
            in
              li []
                [ input
                  [ type_ "checkbox"
                  , checked hasRole
                  , onClick <| MsgChangeRoleInEdge (from, to) role (not hasRole)
                  ] []
                , span [onClick <| MsgEditRole role] [text role]
                ]) roles
        optionsValidator =
          option [selected <| edgeData.validator == Nothing, value ""] [text "---"]
          :: List.map (
            \validator->
              option
                [ selected <| edgeData.validator == Just validator
                , value validator
                ] [text validator]
          ) (OrderedDict.keys model.value.edgeDataInfo.validators)
        optionsModificator =
          option [selected <| edgeData.modificator == Nothing, value ""] [text "---"]
          :: List.map (
            \modificator->
              option
                [ selected <| edgeData.modificator == Just modificator
                , value modificator
                ] [text modificator]
          ) (OrderedDict.keys model.value.edgeDataInfo.modificators)
        edgeList = OrderedDict.keys model.value.edges
        edgeArray = Array.fromList edgeList
        edgeId = edgeArray
          |> Array.toIndexedList
          |> List.filter (\(_, v) -> v == (from, to))
          |> List.head
          |> Maybe.map Tuple.first
        beforeEdge = edgeId |> Maybe.andThen (\id->Array.get (id - 1) edgeArray)
        afterEdge = edgeId |> Maybe.andThen (\id->Array.get (id + 1) edgeArray)
        firstEdge = List.head edgeList |> Maybe.andThen ( \edge->
          if edge == (from, to)
          then Nothing
          else Just edge )
        lastEgde = (List.head <| List.reverse edgeList) |> Maybe.andThen ( \edge->
          if edge == (from, to)
          then Nothing
          else Just edge )
      in
        table []
          [ thead []
            [ tr []
              [ td []
                [ text "<"
                , select [name "switch-backward", value "", onInput <| \s->send (s, from)] optionsSwitchBackword
                ]
              , th [rowspan 2]
                [ button [onClick <| MsgAddStatus from]
                  [ case OrderedDict.get from model.value.statuses |> Maybe.andThen .image of
                    Just image -> img [src image, class "icon"] []
                    Nothing -> text "🛠"
                  ]
                , select [onInput <| \s->send (s, to)] optionsChangeFrom
                , span [] [text " ~> "]
                , select [onInput <| \s->send (from, s)] optionsChangeTo
                , button [onClick <| MsgAddStatus to] 
                  [ case OrderedDict.get to model.value.statuses |> Maybe.andThen .image of
                    Just image -> img [src image, class "icon"] []
                    Nothing -> text "🛠"
                  ]
                ]
              , td [style "text-align" "right"]
                [ select [name "switch-forward",value "", onInput <| \s->send (to, s)] optionsSwitchForword
                , text ">"
                ]
              ]
            , tr [] 
              [ td []
                [ text "+"
                , select [name "add-backward", value "", onInput <| \s->send (s, from)] optionsAddBackword
                ]
              , td [style "text-align" "right"]
                [ select [name "add-forward", value "", onInput <| \s->send (to, s)] optionsAddForword
                , text "+"
                ]
              ]
            ]
          , tbody []
            [ tr []
              [ td [colspan 3]
                [ span [style "display" "inline-block", style "vertical-align" "top"]
                  [ h2 [] [text "Описание:"]
                  , textarea
                    [ onInput <| MsgChangeDescriptionInEdge (from, to)
                    , value edgeData.description
                    ] []
                  , h2 [] [text "Валидатор:"]
                  , select
                    [ onInput <| \v-> MsgChangeValidatorInEdge (from, to) (if v == "" then Nothing else Just v)
                    , value (edgeData.validator |> Maybe.withDefault "")
                    ]  optionsValidator
                  , button
                    [ onClick (edgeData.validator |> Maybe.map MsgEditValidator |> Maybe.withDefault MsgNone)
                    , disabled <| edgeData.validator == Nothing
                    ] [text "🛠"]
                  , h2 [] [text "Модификатор:"]
                  , select
                    [ onInput <| \m-> MsgChangeModificatorInEdge (from, to) (if m == "" then Nothing else Just m)
                    , value (edgeData.modificator |> Maybe.withDefault "")
                    ] optionsModificator
                  , button
                    [ onClick (edgeData.modificator |> Maybe.map MsgEditModificator |> Maybe.withDefault MsgNone)
                    , disabled <| edgeData.modificator == Nothing
                    ] [text "🛠"]
                  ]
                , span [style "display" "inline-block", style "vertical-align" "top"]
                  [ h2 [] [text "Роли:"]
                  , ul [] viewRoles
                  ]
                ]
              ]
            ]
          , tfoot []
            [ tr []
              [ th [colspan 3]
                [ button
                  [ class "navigation-btn"
                  , title "взад"
                  , disabled <| beforeEdge == Nothing
                  , onClick <| MsgActivateEdge ( beforeEdge |> Maybe.withDefault (from, to) )
                  ] [text "◀️"]
                , button
                  [ class "navigation-btn"
                  , title "вначало"
                  , disabled <| firstEdge == Nothing
                  , onClick <| MsgActivateEdge ( firstEdge |> Maybe.withDefault (from, to) )
                  ] [text "🔽"]
                , button
                  [ class "navigation-btn"
                  , title "поднять"
                  , onClick <| MsgMoveEdgeUp (from, to)
                  ] [text "⬆️"]
                , button
                  [ class "navigation-btn"
                  , title "опустить"
                  , onClick <| MsgMoveEdgeDown (from, to)
                  ] [text "⬇️"]
                , button
                  [ class "navigation-btn"
                  , title "в начало"
                  , onClick <| MsgMoveEdgeStart (from, to)
                  ] [text "⏫"]
                , button
                  [ class "navigation-btn"
                  , title "в конец"
                  , onClick <| MsgMoveEdgeEnd (from, to)
                  ] [text "⏬"]
                , button
                  [ class "navigation-btn"
                  , title "очистить"
                  , onClick <| MsgClearEdge (from, to)
                  ] [text "🗑"]
                , button
                  [ class "navigation-btn"
                  , title "удалить"
                  , onClick <| MsgRemoveEdge (from, to)
                  ] [text "🚽"]
                , button
                  [ class "navigation-btn"
                  , title "закрыть"
                  , onClick <| MsgSelectTab TabEdges
                  ] [text "❎"]
                , button
                  [ class "navigation-btn"
                  , title "вперёд"
                  , disabled <| lastEgde == Nothing
                  , onClick <| MsgActivateEdge ( lastEgde |> Maybe.withDefault (from, to) )
                  ] [text "🔼"]
                , button
                  [ class "navigation-btn"
                  , title "вконец"
                  , disabled <| afterEdge == Nothing
                  , onClick <| MsgActivateEdge ( afterEdge |> Maybe.withDefault (from, to) )
                  ] [text "▶️"]
                ]
              ]
            ]
          ]
    Nothing ->
      let
        optionFromStatus status =  option [value status] [text status]
        options = (option [default True] [text "~~~"]) :: (model.value.statuses |> OrderedDict.keys |> List.map optionFromStatus)
        click = case (model.state.addedEdge.from, model.state.addedEdge.to) of
          (Just f, Just t) -> [onClick <| MsgAddEdge (f, t)]
          _ -> []
        (canAdd, canSwitch) = case (model.state.addedEdge.from, model.state.addedEdge.to) of
          (Just f, Just t) ->
            let
             exists = model.value.edges |> OrderedDict.member (f, t)
            in
              (not exists, exists)
          _ -> (False, False)
      in
        div []
          [ select [onInput MsgChangeAddedEdgeFrom] options
          , text "->"
          , select [onInput MsgChangeAddedEdgeTo] options
          , button ((disabled <| not canAdd) :: click) [text "+"]
          , button ((disabled <| not canSwitch) :: click) [text "->"]
          ]
  TabStatuses -> div []
    [ div []
      [ h1 [] [text "Статусы"]
      , button [onClick MsgAddNewStatus] [text "добавить"]
      ]
    , div [] [ul [] (
      model.value.statuses
      |> OrderedDict.keys
      |> List.map (drawStatus model)
    )]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedStatuses
      , onClick MsgMoveSelectedStatusesStart
      ] [text "🔼"]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedStatuses
      , onClick MsgMoveSelectedStatusesEnd
      ] [text "🔽"]
    , button
      [ class "navigation-btn"
      , title "поднять"
      , disabled <| (model.state.currentStatus == Nothing) || (model.state.currentStatus == List.head (OrderedDict.keys model.value.statuses))
      , onClick <| (
        case model.state.currentStatus of
          Just status -> MsgMoveStatusUp status
          Nothing -> MsgNone
      )
      ] [text "⬆️"]
    , button
      [ class "navigation-btn"
      , title "опустить"
      , disabled <| (model.state.currentStatus == Nothing) || (model.state.currentStatus == List.head (List.reverse (OrderedDict.keys model.value.statuses)))
      , onClick <| (
        case model.state.currentStatus of
          Just status -> MsgMoveStatusDown status
          Nothing -> MsgNone
      )
      ] [text "⬇️"]
    , button
      [ class "navigation-btn"
      , title "в начало"
      , disabled <| (model.state.currentStatus == Nothing) || (model.state.currentStatus == List.head (OrderedDict.keys model.value.statuses))
      , onClick <| (
        case model.state.currentStatus of
          Just status -> MsgMoveStatusStart status
          Nothing -> MsgNone
      )
      ] [text "⏫"]
    , button
      [ class "navigation-btn"
      , title "в конец"
      , disabled <| (model.state.currentStatus == Nothing) || (model.state.currentStatus == List.head (List.reverse (OrderedDict.keys model.value.statuses)))
      , onClick <| (
        case model.state.currentStatus of
          Just status -> MsgMoveStatusEnd status
          Nothing -> MsgNone
      )
      ] [text "⏬"]
    , button
      [ class "navigation-btn"
      , title "очистить"
      , disabled <| model.state.currentStatus == Nothing
      , onClick <| (
        case model.state.currentStatus of
          Just status -> MsgResetCurrentStatus
          Nothing -> MsgNone
      )
      ] [text "❎"]
    , button
      [ class "navigation-btn"
      , title "удалить"
      , disabled <| model.state.currentStatus == Nothing
      , onClick <| (
        case model.state.currentStatus of
          Just status -> MsgRemoveStatus status
          Nothing -> MsgNone
      )
      ] [text "🚽"]
    ]
  TabStatusData -> case model.state.currentStatus of
    Just status ->
      let
        statusData =
          OrderedDict.get status model.value.statuses
          |> Maybe.withDefault emptyStatusData
        fromList = model.value.edges
          |> OrderedDict.keys
          |> List.filter (\(f, _)-> Just f == model.state.currentStatus)
          |> List.map Tuple.second
        toList =  model.value.edges
          |> OrderedDict.keys
          |> List.filter (\(_, t)-> Just t == model.state.currentStatus)
          |> List.map Tuple.first

        statusList = OrderedDict.keys model.value.statuses
        statusArray = Array.fromList statusList
        statusId = statusArray
          |> Array.toIndexedList
          |> List.filter (\(_, v)-> v == status)
          |> List.head
          |> Maybe.map Tuple.first
        beforeStatus = statusId |> Maybe.andThen (\id->Array.get (id - 1) statusArray)
        afterStatus = statusId |> Maybe.andThen (\id->Array.get (id + 1) statusArray)
        firstStatus = List.head statusList |> Maybe.andThen ( \st->
          if st == status
          then Nothing
          else Just st )
        lastStatus = (List.head <| List.reverse statusList) |> Maybe.andThen ( \st->
          if st == status
          then Nothing
          else Just st )
      in div []
        [ ul [style "display" "inline-block"] <| List.map (
          \from-> li []
            [ button [onClick <| MsgAddStatus from] [text from]
            , button [onClick <| MsgSwitchEdge (status, from)] [text "<---"]
            ]
        ) fromList
        , span [style "display" "inline-block", style "vertical-align" "top"]
          [ h1 []
            [ text status
            ]
          , br [] []
          , button [onClick <| MsgStartRenameStatus status] [text "переименовать"]
          , h2 [] [text "Иконка"]
          , hr [] []
          , div []
            <| List.filterMap identity [ statusData.image |> Maybe.map (\im -> img [class "image", src im] [])
            , Just <| br [] []
            , Just <| button [onClick <| MsgLoadImageInStatusFromFile status] [text "загрузить"]
            , Just <| button [onClick <| MsgSelectTab TabChangeStatusImage] [text "ввести"]
            , statusData.image |> Maybe.map (\_ -> button [onClick <| MsgChangeImageInStatus status Nothing] [text "удалить"])
            ]
          , hr [] []
          , h2 [] [text "Название:"]
          , input
            [ onInput (\l->MsgChangeLabelInStatus status (Just l))
            , value (statusData.label |> Maybe.withDefault "")
            ] []
          , h2 [] [text "Описание:"]
          , textarea
            [ onInput <| MsgChangeDescriptionInStatus status
            , value statusData.description
            ] []
          ]
        , ul [style "display" "inline-block"] <| List.map (
          \to -> li []
            [ button [onClick <| MsgSwitchEdge (to, status)] [text "--->"]
            , button [onClick <| MsgAddStatus to] [text to]
            ]
        ) toList
        , div[]
          [ button
            [ class "navigation-btn"
            , title "взад"
            , disabled <| beforeStatus == Nothing
            , onClick <| MsgActivateStatus ( beforeStatus |> Maybe.withDefault status )
            ] [text "◀️"]
          , button
            [ class "navigation-btn"
            , title "вначало"
            , disabled <| firstStatus == Nothing
            , onClick <| MsgActivateStatus ( firstStatus |> Maybe.withDefault status )
            ] [text "🔽"]
          , button
            [ class "navigation-btn"
            , title "поднять"
            , onClick <| MsgMoveStatusUp status
            ] [text "⬆️"]
          , button
            [ class "navigation-btn"
            , title "опустить"
            , onClick <| MsgMoveStatusDown status
            ] [text "⬇️"]
          , button
            [ class "navigation-btn"
            , title "в начало"
            , onClick <| MsgMoveStatusStart status
            ] [text "⏫"]
          , button
            [ class "navigation-btn"
            , title "в конец"
            , onClick <| MsgMoveStatusEnd status
            ] [text "⏬"]
          , button
            [ class "navigation-btn"
            , title "очистить"
            , onClick <| MsgClearStatus status
            ] [text "🗑"]
          , button
            [ class "navigation-btn"
            , title "удалить"
            , onClick <| MsgRemoveStatus status
            ] [text "🚽"]
          , button
            [ class "navigation-btn"
            , title "закрыть"
            , onClick <| MsgSelectTab TabStatuses
            ] [text "❎"]
          , button
            [ class "navigation-btn"
            , title "вперёд"
            , disabled <| lastStatus == Nothing
            , onClick <| MsgActivateStatus ( lastStatus |> Maybe.withDefault status )
            ] [text "🔼"]
          , button
            [ class "navigation-btn"
            , title "вконец"
            , disabled <| afterStatus == Nothing
            , onClick <| MsgActivateStatus ( afterStatus |> Maybe.withDefault status )
            ] [text "▶️"]
          ]
        ]
    Nothing -> div []
      [ h1 [] [text "Новый статус"]
      , br [] []
      , input
        [ onInput MsgChangeAddedStatus
        , type_ "text"
        ] []
      , button
        [ onClick (MsgAddStatus model.state.addedStatus)
        , disabled (model.state.addedStatus == "")
        ] [text "добавить"]
      ]
  TabChangeStatusImage -> case model.state.currentStatus of
    Just status -> div []
      [ h1 [] [text "Ввести ссылку на изображение"]
      , h2 [] [text status]
      , input
        [ onInput (\v -> MsgChangeImageInStatus status (Just v))
        , value (OrderedDict.get status model.value.statuses |> Maybe.andThen .image |> Maybe.withDefault "")
        ] []
      , button [onClick <| MsgSelectTab TabStatusData] [text "Ok"]
      ]
    Nothing -> h1 [] [text "Warning!!!"]
  TabRenameStatus -> case model.state.currentStatus of
    Just status -> div []
      [ h1 [] [text "Переименовать статус"]
      , h2 [] [text status]
      , input
        [ value model.state.addedStatus
        , onInput MsgChangeAddedStatus
        , type_ "text"
        ] []
      , button [ onClick <| MsgRenameStatus status model.state.addedStatus ] [text "Переименовать"]
      ]
    Nothing -> h1 [] [text "Warning!!!"]
  TabRoles -> div []
    [ div []
      [ h1 [] [text "Роли"]
      , button [onClick MsgAddNewRole] [text "добавить"]
      ]
    , div [] [ul [] (
      model.value.edgeDataInfo.roles
      |> OrderedDict.keys
      |> List.map (drawRole model)
    )]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedRoles
      , onClick MsgMoveSelectedRolesStart
      ] [text "🔼"]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedRoles
      , onClick MsgMoveSelectedRolesEnd
      ] [text "🔽"]
    , button
      [ class "navigation-btn"
      , title "поднять"
      , disabled <| (model.state.currentRole == Nothing) || (model.state.currentRole == List.head (OrderedDict.keys model.value.edgeDataInfo.roles))
      , onClick <| (
        case model.state.currentRole of
          Just role -> MsgMoveRoleUp role
          Nothing -> MsgNone
      )
      ] [text "⬆️"]
    , button
      [ class "navigation-btn"
      , title "опустить"
      , disabled <| (model.state.currentRole == Nothing) || (model.state.currentRole == List.head (List.reverse (OrderedDict.keys model.value.edgeDataInfo.roles)))
      , onClick <| (
        case model.state.currentRole of
          Just role -> MsgMoveRoleDown role
          Nothing -> MsgNone
      )
      ] [text "⬇️"]
    , button
      [ class "navigation-btn"
      , title "в начало"
      , disabled <| (model.state.currentRole == Nothing) || (model.state.currentRole == List.head (OrderedDict.keys model.value.edgeDataInfo.roles))
      , onClick <| (
        case model.state.currentRole of
          Just role -> MsgMoveRoleStart role
          Nothing -> MsgNone
      )
      ] [text "⏫"]
    , button
      [ class "navigation-btn"
      , title "в конец"
      , disabled <| (model.state.currentRole == Nothing) || (model.state.currentRole == List.head (List.reverse (OrderedDict.keys model.value.edgeDataInfo.roles)))
      , onClick <| (
        case model.state.currentRole of
          Just role -> MsgMoveRoleEnd role
          Nothing -> MsgNone
      )
      ] [text "⏬"]
    , button
      [ class "navigation-btn"
      , title "очистить"
      , disabled <| model.state.currentRole == Nothing
      , onClick <| (
        case model.state.currentRole of
          Just role -> MsgResetCurrentRole
          Nothing -> MsgNone
      )
      ] [text "❎"]
    , button
      [ class "navigation-btn"
      , title "удалить"
      , disabled <| model.state.currentRole == Nothing
      , onClick <| (
        case model.state.currentRole of
          Just role -> MsgRemoveRole role
          Nothing -> MsgNone
      )
      ] [text "🚽"]
    ]
  TabRoleData -> case model.state.currentRole of
    Just role ->
      let
        roleData =
          OrderedDict.get role model.value.edgeDataInfo.roles
          |> Maybe.withDefault emptyRoleData
        roleList = OrderedDict.keys model.value.edgeDataInfo.roles
        roleArray = Array.fromList roleList
        roleId = roleArray
          |> Array.toIndexedList
          |> List.filter (\(_, v)-> v == role)
          |> List.head
          |> Maybe.map Tuple.first
        beforeRole = roleId |> Maybe.andThen (\id->Array.get (id - 1) roleArray)
        afterRole = roleId |> Maybe.andThen (\id->Array.get (id + 1) roleArray)
        firstRole = List.head roleList |> Maybe.andThen ( \ro ->
          if ro == role
          then Nothing
          else Just ro )
        lastRole = (List.head <| List.reverse roleList) |> Maybe.andThen ( \ro ->
          if ro == role
          then Nothing
          else Just ro )
      in
        div []
          [ h1 [] [text "Роль:"]
          , h1 [] [text role]
          , button [onClick <| MsgStartRenameRole role] [text "переименовать"]
          , div []
            [ div [style "display" "inline-block", style "vertical-align" "top"]
              [ h2 [] [text "Название:"]
              , br [] []
              , input
                [ onInput (MsgChangeLabelInRole role << Just)
                , value (roleData.label |> Maybe.withDefault "")
                ] []
              , h2 [] [text "Описание:"]
              , textarea
                [ onInput <| MsgChangeDescriptionInRole role
                , value roleData.description
                ] []
              ]
            , div [style "display" "inline-block", style "vertical-align" "top"]
              [ h2 [] [text "Испольхуются в:"]
              , ul []
                <| List.map (drawEdge model)
                <| List.filter
                  (
                    \e -> OrderedDict.get e model.value.edges
                      |> Maybe.map .roles
                      |> Maybe.map (Set.member role)
                      |> Maybe.withDefault False
                  )
                <| OrderedDict.keys model.value.edges
              ]
            ]
          , div []
            [ button
              [ class "navigation-btn"
              , title "взад"
              , disabled <| beforeRole == Nothing
              , onClick <| MsgActivateRole ( beforeRole |> Maybe.withDefault role )
              ] [text "◀️"]
            , button
              [ class "navigation-btn"
              , title "вначало"
              , disabled <| firstRole == Nothing
              , onClick <| MsgActivateRole ( firstRole |> Maybe.withDefault role )
              ] [text "🔽"]
            , button
              [ class "navigation-btn"
              , title "поднять"
              , onClick <| MsgMoveRoleUp role
              ] [text "⬆️"]
            , button
              [ class "navigation-btn"
              , title "опустить"
              , onClick <| MsgMoveRoleDown role
              ] [text "⬇️"]
            , button
              [ class "navigation-btn"
              , title "в начало"
              , onClick <| MsgMoveRoleStart role
              ] [text "⏫"]
            , button
              [ class "navigation-btn"
              , title "в конец"
              , onClick <| MsgMoveRoleEnd role
              ] [text "⏬"]
            , button
              [ class "navigation-btn"
              , title "очистить"
              , onClick <| MsgClearRole role
              ] [text "🗑"]
            , button
              [ class "navigation-btn"
              , title "удалить"
              , onClick <| MsgRemoveRole role
              ] [text "🚽"]
            , button
              [ class "navigation-btn"
              , title "закрыть"
              , onClick <| MsgSelectTab TabRoles
              ] [text "❎"]
            , button
              [ class "navigation-btn"
              , title "вперёд"
              , disabled <| lastRole == Nothing
              , onClick <| MsgActivateRole ( lastRole |> Maybe.withDefault role )
              ] [text "🔼"]
            , button
              [ class "navigation-btn"
              , title "вконец"
              , disabled <| afterRole == Nothing
              , onClick <| MsgActivateRole ( afterRole |> Maybe.withDefault role )
              ] [text "▶️"]
            ]

          ]
    Nothing -> div []
      [ h1 [] [text "Новая роль"]
      , br [] []
      , input
        [ onInput MsgChangeAddedRole
        , type_ "text"
        ] []
      , button
        [ onClick (MsgAddRole model.state.addedRole)
        , disabled (model.state.addedRole == "")
        ] [text "добавить"]
      ]
  TabRenameRole -> case model.state.currentRole of
    Just role -> div []
      [ h1 [] [text "Переименовать роль"]
      , h2 [] [text role]
      , input
        [ value model.state.addedRole
        , onInput MsgChangeAddedRole
        , type_ "text"
        ] []
      , button [ onClick <| MsgRenameRole role model.state.addedRole ] [text "Переименовать"]
      ]
    Nothing -> h1 [] [text "Warning!!!"]
  TabValidators -> div []
    [ div []
      [ h1 [] [text "Валидаторы"]
      , button [onClick MsgAddNewValidator] [text "добавить"]
      ]
    , div [] [ul [] (
      OrderedDict.keys model.value.edgeDataInfo.validators
      |> List.map (drawValidator model)
    )]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedValidators
      , onClick MsgMoveSelectedValidatorsStart
      ] [text "🔼"]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedValidators
      , onClick MsgMoveSelectedValidatorsEnd
      ] [text "🔽"]
    , button
      [ class "navigation-btn"
      , title "поднять"
      , disabled <| (model.state.currentValidator == Nothing) || (model.state.currentValidator == List.head (OrderedDict.keys model.value.edgeDataInfo.roles))
      , onClick <| (
        case model.state.currentValidator of
          Just validator -> MsgMoveValidatorUp validator
          Nothing -> MsgNone
      )
      ] [text "⬆️"]
    , button
      [ class "navigation-btn"
      , title "опустить"
      , disabled <| (model.state.currentValidator == Nothing) || (model.state.currentValidator == List.head (List.reverse (OrderedDict.keys model.value.edgeDataInfo.roles)))
      , onClick <| (
        case model.state.currentValidator of
          Just validator -> MsgMoveValidatorDown validator
          Nothing -> MsgNone
      )
      ] [text "⬇️"]
    , button
      [ class "navigation-btn"
      , title "в начало"
      , disabled <| (model.state.currentValidator == Nothing) || (model.state.currentValidator == List.head (OrderedDict.keys model.value.edgeDataInfo.roles))
      , onClick <| (
        case model.state.currentValidator of
          Just validator -> MsgMoveValidatorStart validator
          Nothing -> MsgNone
      )
      ] [text "⏫"]
    , button
      [ class "navigation-btn"
      , title "в конец"
      , disabled <| (model.state.currentValidator == Nothing) || (model.state.currentValidator == List.head (List.reverse (OrderedDict.keys model.value.edgeDataInfo.roles)))
      , onClick <| (
        case model.state.currentValidator of
          Just validator -> MsgMoveValidatorEnd validator
          Nothing -> MsgNone
      )
      ] [text "⏬"]
    , button
      [ class "navigation-btn"
      , title "очистить"
      , disabled <| model.state.currentValidator == Nothing
      , onClick <| (
        case model.state.currentValidator of
          Just validator -> MsgResetCurrentValidator
          Nothing -> MsgNone
      )
      ] [text "❎"]
    , button
      [ class "navigation-btn"
      , title "удалить"
      , disabled <| model.state.currentValidator == Nothing
      , onClick <| (
        case model.state.currentValidator of
          Just validator -> MsgRemoveValidator validator
          Nothing -> MsgNone
      )
      ] [text "🚽"]
    ]
  TabValidatorData -> case model.state.currentValidator of
    Just validator ->
      let
        validatorData =
          OrderedDict.get validator model.value.edgeDataInfo.validators
          |> Maybe.withDefault emptyValidatorData
        validatorList = OrderedDict.keys model.value.edgeDataInfo.validators
        validatorArray = Array.fromList validatorList
        validatorId = validatorArray
          |> Array.toIndexedList
          |> List.filter (\(_, v)-> v == validator)
          |> List.head
          |> Maybe.map Tuple.first
        beforeValidator = validatorId |> Maybe.andThen (\id->Array.get (id - 1) validatorArray)
        afterValidator = validatorId |> Maybe.andThen (\id->Array.get (id + 1) validatorArray)
        firstValidator = List.head validatorList |> Maybe.andThen ( \va ->
          if va == validator
          then Nothing
          else Just va )
        lastValidator = ( List.head <| List.reverse validatorList) |> Maybe.andThen ( \va ->
          if va == validator
          then Nothing
          else Just va )
      in
        div []
          [ h1 [] [text validator]
          , br [] []
          , button [onClick <| MsgStartRenameValidator validator] [text "переименовать"]
          , div []
            [ div [style "display" "inline-block", style "vertical-align" "top"]
              [ h2 [] [text "Название:"]
              , input
                [ onInput (\l->MsgChangeLabelInValidator validator (Just l))
                , value (validatorData.label |> Maybe.withDefault "")
                ] []
              , h2 [] [text "Описание:"]
              , textarea
                [ onInput <| MsgChangeDescriptionInValidator validator
                , value validatorData.description
                ] []
              ]
            , div [style "display" "inline-block", style "vertical-align" "top"]
              [ h2 [] [text "Испольхуются в:"]
              , ul []
                <| List.map (drawEdge model)
                <| List.filter
                  (
                    \v ->
                      OrderedDict.get v model.value.edges
                      |> Maybe.andThen .validator
                      |> (==) (Just validator)
                  )
                <| OrderedDict.keys model.value.edges
              ]
            ]
          , div[]
            [ button
              [ class "navigation-btn"
              , title "взад"
              , disabled <| beforeValidator == Nothing
              , onClick <| MsgActivateValidator ( beforeValidator |> Maybe.withDefault validator )
              ] [text "◀️"]
            , button
              [ class "navigation-btn"
              , title "вначало"
              , disabled <| firstValidator == Nothing
              , onClick <| MsgActivateValidator ( firstValidator |> Maybe.withDefault validator )
              ] [text "🔽"]
            , button
              [ class "navigation-btn"
              , title "поднять"
              , onClick <| MsgMoveValidatorUp validator
              ] [text "⬆️"]
            , button
              [ class "navigation-btn"
              , title "опустить"
              , onClick <| MsgMoveValidatorDown validator
              ] [text "⬇️"]
            , button
              [ class "navigation-btn"
              , title "в начало"
              , onClick <| MsgMoveValidatorStart validator
              ] [text "⏫"]
            , button
              [ class "navigation-btn"
              , title "в конец"
              , onClick <| MsgMoveValidatorEnd validator
              ] [text "⏬"]
            , button
              [ class "navigation-btn"
              , title "очистить"
              , onClick <| MsgClearValidator validator
              ] [text "🗑"]
            , button
              [ class "navigation-btn"
              , title "удалить"
              , onClick <| MsgRemoveValidator validator
              ] [text "🚽"]
            , button
              [ class "navigation-btn"
              , title "закрыть"
              , onClick <| MsgSelectTab TabValidators
              ] [text "❎"]
            , button
              [ class "navigation-btn"
              , title "вперёд"
              , disabled <| lastValidator == Nothing
              , onClick <| MsgActivateValidator ( lastValidator |> Maybe.withDefault validator )
              ] [text "🔼"]
            , button
              [ class "navigation-btn"
              , title "вконец"
              , disabled <| afterValidator == Nothing
              , onClick <| MsgActivateValidator ( afterValidator |> Maybe.withDefault validator )
              ] [text "▶️"]
            ]
          ]
    Nothing -> div []
      [ h1 [] [text "Новый валидатор"]
      , br [] []
      , input
        [ onInput MsgChangeAddedValidator
        , type_ "text"
        ] []
      , button
        [ onClick (MsgAddValidator model.state.addedValidator)
        , disabled (model.state.addedValidator == "")
        ] [text "добавить"]
      ]
  TabRenameValidator -> case model.state.currentValidator of
    Just validator -> div []
      [ h1 [] [text "Переименовать валидатор"]
      , h2 [] [text validator]
      , input
        [ value model.state.addedValidator
        , onInput MsgChangeAddedValidator
        , type_ "text"
        ] []
      , button [ onClick <| MsgRenameValidator validator model.state.addedValidator ] [text "Переименовать"]
      ]
    Nothing -> h1 [] [text "Warning!!!"]
  TabModificatirs -> div []
    [ div []
      [ h1 [] [text "Модификаторы"]
      , button [onClick MsgAddNewModificator] [text "добавить"]
      ]
    , div [] [ul [] (
      model.value.edgeDataInfo.modificators
      |> OrderedDict.keys
      |> List.map (drawModificator model)
    )]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedModificators
      , onClick MsgMoveSelectedModificatorsStart
      ] [text "🔼"]
    , button
      [ class "navigation-btn"
      , title "вначало"
      , disabled <| Set.isEmpty model.state.selectedModificators
      , onClick MsgMoveSelectedModificatorsEnd
      ] [text "🔽"]
    , button
      [ class "navigation-btn"
      , title "поднять"
      , disabled <| (model.state.currentModificator == Nothing) || (model.state.currentModificator == List.head (OrderedDict.keys model.value.edgeDataInfo.modificators))
      , onClick <| (
        case model.state.currentModificator of
          Just modificator -> MsgMoveModificatorUp modificator
          Nothing -> MsgNone
      )
      ] [text "⬆️"]
    , button
      [ class "navigation-btn"
      , title "опустить"
      , disabled <| (model.state.currentModificator == Nothing) || (model.state.currentModificator == List.head (List.reverse (OrderedDict.keys model.value.edgeDataInfo.modificators)))
      , onClick <| (
        case model.state.currentModificator of
          Just modificator -> MsgMoveModificatorDown modificator
          Nothing -> MsgNone
      )
      ] [text "⬇️"]
    , button
      [ class "navigation-btn"
      , title "в начало"
      , disabled <| (model.state.currentModificator == Nothing) || (model.state.currentModificator == List.head (OrderedDict.keys model.value.edgeDataInfo.modificators))
      , onClick <| (
        case model.state.currentModificator of
          Just modificator -> MsgMoveModificatorStart modificator
          Nothing -> MsgNone
      )
      ] [text "⏫"]
    , button
      [ class "navigation-btn"
      , title "в конец"
      , disabled <| (model.state.currentModificator == Nothing) || (model.state.currentModificator == List.head (List.reverse (OrderedDict.keys model.value.edgeDataInfo.modificators)))
      , onClick <| (
        case model.state.currentModificator of
          Just modificator -> MsgMoveModificatorEnd modificator
          Nothing -> MsgNone
      )
      ] [text "⏬"]
    , button
      [ class "navigation-btn"
      , title "очистить"
      , disabled <| model.state.currentModificator == Nothing
      , onClick <| (
        case model.state.currentModificator of
          Just modificator -> MsgResetCurrentModificator
          Nothing -> MsgNone
      )
      ] [text "❎"]
    , button
      [ class "navigation-btn"
      , title "удалить"
      , disabled <| model.state.currentModificator == Nothing
      , onClick <| (
        case model.state.currentModificator of
          Just modificator -> MsgRemoveModificator modificator
          Nothing -> MsgNone
      )
      ] [text "🚽"]
    ]
  TabModificatorData -> case model.state.currentModificator of
    Just modificator ->
      let
        modificatorData =
          OrderedDict.get modificator model.value.edgeDataInfo.modificators
          |> Maybe.withDefault emptyModificatorData
        modificatorList = OrderedDict.keys model.value.edgeDataInfo.modificators
        modificatorArray = Array.fromList modificatorList
        modificatorId = modificatorArray
          |> Array.toIndexedList
          |> List.filter (\(_, v)-> v == modificator)
          |> List.head
          |> Maybe.map Tuple.first
        beforeModificator = modificatorId |> Maybe.andThen (\id->Array.get (id - 1) modificatorArray)
        afterModificator = modificatorId |> Maybe.andThen (\id->Array.get (id + 1) modificatorArray)
        firstModificator = List.head modificatorList |> Maybe.andThen ( \mo ->
          if mo == modificator
          then Nothing
          else Just mo )
        lastModificator = (List.head <| List.reverse modificatorList) |> Maybe.andThen ( \mo ->
          if mo == modificator
          then Nothing
          else Just mo )
      in
        div []
          [ h1 [] [text modificator]
          , br [] []
          , button [onClick <| MsgStartRenameModificator modificator] [text "переименовать"]
          , div []
            [ div [style "display" "inline-block", style "vertical-align" "top"]
              [ h2 [] [text "Название:"]
              , input
                [ onInput (\l->MsgChangeLabelInModificator modificator (Just l))
                , value (modificatorData.label |> Maybe.withDefault "")
                ] []
              , h2 [] [text "Описание:"]
              , textarea
                [ onInput <| MsgChangeDescriptionInModificator modificator
                , value modificatorData.description
                ] []
              ]
            , div [style "display" "inline-block", style "vertical-align" "top"]
              [ h2 [] [text "Испольхуются в:"]
              , ul []
                <| List.map (drawEdge model)
                <| List.filter
                  (
                    \v ->
                      OrderedDict.get v model.value.edges
                      |> Maybe.andThen .modificator
                      |> (==) (Just modificator)
                  )
                <| OrderedDict.keys model.value.edges
              ]
            ]
          , div[]
            [ button
              [ class "navigation-btn"
              , title "взад"
              , disabled <| beforeModificator == Nothing
              , onClick <| MsgActivateModificator ( beforeModificator |> Maybe.withDefault modificator )
              ] [text "◀️"]
            , button
              [ class "navigation-btn"
              , title "вначало"
              , disabled <| firstModificator == Nothing
              , onClick <| MsgActivateModificator ( firstModificator |> Maybe.withDefault modificator )
              ] [text "🔽"]
            , button
              [ class "navigation-btn"
              , title "поднять"
              , onClick <| MsgMoveModificatorUp modificator
              ] [text "⬆️"]
            , button
              [ class "navigation-btn"
              , title "опустить"
              , onClick <| MsgMoveModificatorDown modificator
              ] [text "⬇️"]
            , button
              [ class "navigation-btn"
              , title "в начало"
              , onClick <| MsgMoveModificatorStart modificator
              ] [text "⏫"]
            , button
              [ class "navigation-btn"
              , title "в конец"
              , onClick <| MsgMoveModificatorEnd modificator
              ] [text "⏬"]
            , button
              [ class "navigation-btn"
              , title "очистить"
              , onClick <| MsgClearModificator modificator
              ] [text "🗑"]
            , button
              [ class "navigation-btn"
              , title "удалить"
              , onClick <| MsgRemoveModificator modificator
              ] [text "🚽"]
            , button
              [ class "navigation-btn"
              , title "закрыть"
              , onClick <| MsgSelectTab TabModificatirs
              ] [text "❎"]
            , button
              [ class "navigation-btn"
              , title "вперёд"
              , disabled <| lastModificator == Nothing
              , onClick <| MsgActivateModificator ( lastModificator |> Maybe.withDefault modificator )
              ] [text "🔼"]
            , button
              [ class "navigation-btn"
              , title "вконец"
              , disabled <| afterModificator == Nothing
              , onClick <| MsgActivateModificator ( afterModificator |> Maybe.withDefault modificator )
              ] [text "▶️"]
            ]
          ]
    Nothing -> div []
      [ h1 [] [text "Новый модификатор"]
      , br [] []
      , input
        [ onInput MsgChangeAddedModificator
        , type_ "text"
        ] []
      , button
        [ onClick (MsgAddModificator model.state.addedModificator)
        , disabled (model.state.addedModificator == "")
        ] [text "добавить"]
      ]
  TabRenameModificator -> case model.state.currentModificator of
    Just modificator -> div []
      [ h1 [] [text "Переименовать модификатор"]
      , h2 [] [text modificator]
      , input
        [ value model.state.addedModificator
        , onInput MsgChangeAddedModificator
        , type_ "text"
        ] []
      , button [ onClick <| MsgRenameModificator modificator model.state.addedModificator ] [text "Переименовать"]
      ]
    Nothing -> h1 [] [text "Warning!!!"]
  -- TabAnalytics -> div [] []
  TabConfig -> div []
    [ h1 [] [text "Настройки"]
    , h2 [] [text "Сохранять в афйл:"]
    , input
      [ value model.config.filename
      , onInput MsgChangeDefaultFileName
      ] []
    , br [] []
    , h2 [] [text "Отступы"]
    , pre [] [text <| JE.encode 0 <| JE.int model.config.ident]
    , select
      [ onInput
        (
          MsgChangeIdent
          << Result.withDefault 0
          << JD.decodeString JD.int
        )
      , value <| JE.encode 0 <| JE.int model.config.ident
      ]
      [ option [value "0", selected <| model.config.ident == 0] [text "0"]
      , option [value "1", selected <| model.config.ident == 1] [text "1"]
      , option [value "2", selected <| model.config.ident == 2] [text "2"]
      , option [value "4", selected <| model.config.ident == 4] [text "4"]
      , option [value "8", selected <| model.config.ident == 8] [text "8"]
      ]
    , h2 [] [text "Отображать в первую очередь метки"]
    , input
      [ type_ "checkbox"
      , checked model.config.preferLabel
      , onCheck MsgChangePreferLabel
      ] []
    ]
  TabLoadedUrl -> div []
    [ h1 [] [text "Загрузить из ссылки"]
    , input [onInput MsgChangeLoadedUrl, value model.state.loadedUrl] []
    , button [onClick <| MsgLoadFromUrl model.state.loadedUrl] [text "загрузить"]
    ]
  TabHelp -> Help.help
  _ -> h1 [] [text "не реализовано"]
css = node "link" [rel "stylesheet", href "style.css"] []
view model = div [] 
  [ css
  , drawTabs model
  , drawContent model
  ]