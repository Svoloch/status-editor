module AbstractValue exposing (..)

import List
import Set
import Maybe
import Tuple
import Dict
import Array
import OrderedSet
import OrderedDict

type alias Edge status = (status, status)
type alias Value status description edgeData edgeDataInfo =
  { statuses: OrderedDict.OrderedDict status description
  , edges: OrderedDict.OrderedDict (Edge status) edgeData
  , edgeDataInfo: edgeDataInfo
  }
