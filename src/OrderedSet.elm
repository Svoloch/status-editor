module OrderedSet exposing (..)

import Set
import List

type alias OrderedSet value =
  { set: Set.Set value
  , list: List value
  }
empty = 
  { set = Set.empty
  , list = []
  }
append item ordered =
  { set = Set.insert item ordered.set
  , list =
    if Set.member item ordered.set
    then ordered.list
    else ordered.list ++ [item]
  }
prepend item ordered =
  { set = Set.insert item ordered.set
  , list =
    if Set.member item ordered.set
    then ordered.list
    else ordered.list ++ [item]
  }
push item ordered =
  { set = Set.insert item ordered.set
  , list = List.filter ((/=) item) ordered.list ++ [item]
  }
unshift item ordered =
  { set = Set.insert item ordered.set
  , list = List.filter ((/=) item) ordered.list ++ [item]
  }
remove item ordered =
  { set = Set.remove item ordered.set
  , list = List.filter ((/=) item) ordered.list
  }
split1 value list = 
  let
    split_ item pair =
      case pair of
        Just (start, rest) ->
          case rest of
            head::tail ->
              if head == item
              then Just (start, tail)
              else split_ item (Just (start ++ [head], tail))
            _ -> Nothing
        Nothing -> Nothing
  in
    split_ value (Just ([], list))
split2 value list = 
  let
    split_ item pair =
      case pair of
        Just (start, rest) ->
          case rest of
            head::tail ->
              if head == item
              then Just (start, tail)
              else split_ item (Just (head::start, tail))
            _ -> Nothing
        Nothing -> Nothing
  in
    case split_ value (Just ([], list)) of
      Just (start, rest)-> Just (List.reverse start, rest) 
      Nothing -> Nothing
split = split1
insertBefore item before ordered =
  { set = Set.insert item ordered.set
  , list =
    if Set.member item ordered.set
    then
      case split item ordered.list of
        Just (start, rest) -> start ++ [item, before] ++ rest
        Nothing -> item :: ordered.list
    else item :: ordered.list
  }
insertAfter item after ordered =
  { set = Set.insert item ordered.set
  , list =
    if Set.member item ordered.set
    then
      case split item ordered.list of
        Just (start, rest) -> start ++ [after, item] ++ rest
        Nothing -> ordered.list ++ [item]
    else item :: ordered.list
  }
moveUp item ordered =
  { set = ordered.set
  , list =
    case split item ordered.list of
      Just (start, next::rest) -> start ++ [next, item] ++ rest
      _ -> ordered.list
  }
moveDown item ordered =
  { set = ordered.set
  , list =
    case split item <| List.reverse ordered.list of
      Just (start, next::rest) -> List.reverse (start ++ [next, item] ++ rest)
      _ -> ordered.list
  }
fromList list = List.foldr append empty
fromSet set =
  { set = set
  , list = Set.toList set
  }
toList ordered = ordered.list
