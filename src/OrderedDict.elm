module OrderedDict exposing (..)
import Dict
import List
import Maybe
import Tuple
import Set

type alias OrderedDict key value =
  { keys: List key
  , dict: Dict.Dict key (Maybe value)
  }
empty =
  { keys = []
  , dict = Dict.empty
  }
prepend key ordered =
  { keys = key :: List.filter ((/=) key) ordered.keys
  , dict =
    if Dict.member key ordered.dict
    then ordered.dict
    else Dict.insert key Nothing ordered.dict
  }
append key ordered =
  { keys = List.filter ((/=) key) ordered.keys ++ [key]
  , dict =
    if Dict.member key ordered.dict
    then ordered.dict
    else Dict.insert key Nothing ordered.dict
  }
remove key ordered =
  { keys = List.filter ((/=) key) ordered.keys
  , dict = Dict.remove key ordered.dict
  }
drop key ordered =
  { keys = List.filter ((/=) key) ordered.keys
  , dict = ordered.dict
  }
setValue key value ordered =
  { keys = ordered.keys
  , dict = Dict.insert key (Just value) ordered.dict
  }
filterKeys callback ordered =
  { keys = List.filter callback ordered.keys
  , dict =
    ordered.dict |>
    Dict.toList |>
    List.filter (\(key, _)->callback key) |>
    Dict.fromList
  }
get key ordered =
  Dict.get key ordered.dict
  |> Maybe.andThen identity
fromList : List (comparable, value) -> OrderedDict comparable value
fromList list =
  { keys = List.foldl (
       \(k, _) (l, s)->
         if Set.member k s
         then (l, s)
         else (k::l, Set.insert k s)
       ) ([], Set.empty) list
     |> Tuple.first
  , dict = Dict.fromList (list |> List.map (\a -> Tuple.mapSecond Just a))
  }
toList ordered =
  ordered.keys
  |> List.map (\key -> (key, get key ordered))
  --|> List.filter (\(_, val) -> val /= Nothing)
  |> List.filterMap (
    \(k, mv)-> case mv of
      Just v -> Just (k, v)
      Nothing -> Nothing
  )
member key ordered = Dict.member key ordered.dict
keys ordered = ordered.keys
split value list =
  let
    pos =
      list
      |> List.indexedMap (
        \i v->
          if v == value
          then Just i
          else Nothing
      )
      |> List.filter (\p->p /= Nothing)
      |> List.head
      |> Maybe.andThen identity
  in 
    case pos of
      Just p -> Just (List.take p list, List.drop (p + 1) list)
      _ -> Nothing
moveUp key ordered =
  { ordered
  | keys = case split key ordered.keys of
      Just (s, e) ->
        let
          start = s |> List.reverse |> List.tail |> Maybe.withDefault [] |> List.reverse
          before = s |> List.reverse |> List.head |> Maybe.map List.singleton |> Maybe.withDefault []
        in
          start ++ [key] ++ before ++ e
      _ -> [key] ++ ordered.keys
  }
moveDown key ordered =
  { ordered
  | keys = case split key ordered.keys of
      Just (s, e) ->
        let
          after = e |> List.head |> Maybe.map List.singleton |> Maybe.withDefault []
          end = e |> List.tail |> Maybe.withDefault []
        in
          s ++ after ++ [key] ++ end
      _ -> ordered.keys ++ [key]
  }
replaceKey from to ordered =
  let
    val = Dict.get from ordered.dict
    removed = Dict.remove from ordered.dict
    dict = case val of
      Just value -> Dict.insert to value removed
      Nothing -> removed
  in
    { keys = case split from ordered.keys of
      Just (start, end) -> start ++ [to] ++ end
      Nothing -> ordered.keys
    , dict = dict
    }
map: (k -> a -> b) -> OrderedDict k a -> OrderedDict k b
map fn ordered =
  { keys = ordered.keys
  , dict =
    Dict.map ( \k -> Maybe.map (fn k) ) ordered.dict
  }