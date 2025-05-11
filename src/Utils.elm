module Utils exposing (..)

import Set
import Dict
import OrderedDict
import Json.Decode as JD
import Json.Encode as JE
import Types exposing (..)
import AbstractValue

type PairItem key value
  = Key key
  | Value value
prepare (f, t) = [Key f, Value t]

decode = JD.map5
  (
    \edges statuses roles validators modificators->
      { edges = OrderedDict.fromList edges
      , statuses = OrderedDict.fromList statuses
      , edgeDataInfo =
        { roles = OrderedDict.fromList roles
        , validators = OrderedDict.fromList validators
        , modificators = OrderedDict.fromList modificators
        }
      }
  )
  (JD.field "edges"
    (JD.list <| JD.map2 Tuple.pair
      (JD.index 0 <| JD.map2 Tuple.pair
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
      )
      (JD.index 1 <| JD.map4
        (
          \roles validator modificator description ->
            { roles = roles
              |> Maybe.map Set.fromList
              |> Maybe.withDefault Set.empty
            , validator = validator
            , modificator = modificator
            , description = description |> Maybe.withDefault ""
            }
        )
        (JD.maybe <| JD.field "roles" (JD.list JD.string))
        (JD.maybe <| JD.field "validator" JD.string)
        (JD.maybe <| JD.field "modificator" JD.string)
        (JD.maybe <| JD.field "description" JD.string)
      )
    )
  )
  (JD.field "statuses"
    (JD.list <| JD.map2 Tuple.pair
      (JD.index 0 JD.string)
      (JD.index 1 <| JD.map3
        (
          \description label image ->
            { description = description |> Maybe.withDefault ""
            , label = label
            , image = image
            }
        ) 
        (JD.maybe <| JD.field "description" JD.string)
        (JD.maybe <| JD.field "label" JD.string)
        (JD.maybe <| JD.field "image" JD.string)
      )
    )
  )
  (JD.field "roles"
    (JD.list <| JD.map2 Tuple.pair
      (JD.index 0 JD.string)
      (JD.index 1 <| JD.map2
        (
          \description label ->
            { description = description |> Maybe.withDefault ""
            , label = label
            }
        ) 
        (JD.maybe <| JD.field "description" JD.string)
        (JD.maybe <| JD.field "label" JD.string)
      )
    )
  )
  (JD.field "validators"
    (JD.list <| JD.map2 Tuple.pair
      (JD.index 0 JD.string)
      (JD.index 1 <| JD.map2
        (
          \description label ->
            { description = description |> Maybe.withDefault ""
            , label = label
            }
        ) 
        (JD.maybe <| JD.field "description" JD.string)
        (JD.maybe <| JD.field "label" JD.string)
      )
    )
  )
  (JD.field "modificators"
    (JD.list <| JD.map2 Tuple.pair
      (JD.index 0 JD.string)
      (JD.index 1 <| JD.map2
        (
          \description label ->
            { description = description |> Maybe.withDefault ""
            , label = label
            }
        ) 
        (JD.maybe <| JD.field "description" JD.string)
        (JD.maybe <| JD.field "label" JD.string)
      )
    )
  )

statusDataToJSON data = JE.object <| List.filterMap identity
  [ Just ("description", JE.string data.description)
  , Maybe.map (\v -> ("image", JE.string v)) data.image
  , Maybe.map (\v -> ("label", JE.string v)) data.label
  ]
dataToJSON data = JE.object <| List.filterMap identity
  [ Just ("description", JE.string data.description)
  , Maybe.map (\v -> ("label", JE.string v)) data.label
  ]
edgeDataToJSON data = JE.object <| List.filterMap identity
  [ Just ("roles", JE.set JE.string data.roles)
  , Maybe.map (\v -> ("validator", JE.string v)) data.validator
  , Maybe.map (\v -> ("modificator", JE.string v)) data.modificator
  ]
modelToJSON model = JE.object 
  [ ("edges", JE.list (
    \p->
      JE.list (
        \item -> case item of
          Key (f, t) -> JE.list JE.string [f, t]
          Value v -> edgeDataToJSON v
      ) <| prepare p
  ) (OrderedDict.toList model.value.edges))  
  , ("statuses", JE.list (
    \p->
      JE.list (
        \item -> case item of
          Key k -> JE.string k
          Value v -> statusDataToJSON v
      ) <| prepare p
  ) (OrderedDict.toList model.value.statuses))
  , ("roles", JE.list (
    \p->
      JE.list (
        \item -> case item of
          Key k -> JE.string k
          Value v -> dataToJSON v
      ) <| prepare p
  ) (OrderedDict.toList model.value.edgeDataInfo.roles))
  , ("validators", JE.list (
    \p->
      JE.list (
        \item -> case item of
          Key k -> JE.string k
          Value v -> dataToJSON v
      ) <| prepare p
  ) (OrderedDict.toList model.value.edgeDataInfo.validators))
  , ("modificators", JE.list (
    \p->
      JE.list (
        \item -> case item of
          Key k -> JE.string k
          Value v -> dataToJSON v
      ) <| prepare p
  ) (OrderedDict.toList model.value.edgeDataInfo.modificators))
  ]
generateJSON = JE.encode 2 << modelToJSON
parseJSON = JD.decodeString decode