module Types exposing (..)

import List
import Set
import Maybe
import Tuple
import Dict
import Array
import Http
import File
import File.Select
import File.Download
import AbstractValue
import OrderedSet
import OrderedDict

type alias EdgeData =
  { roles: Set.Set String
  , validator: Maybe String
  , modificator: Maybe String
  }
type alias Edge = AbstractValue.Edge String
type alias EdgeDataInfo =
  { roles: OrderedDict.OrderedDict String String
  , validators: OrderedDict.OrderedDict String String
  , modificators: OrderedDict.OrderedDict String String
  , description: String
  }
type alias StatusDataInfo =
  { description: String
  , label: Maybe String
  , image: Maybe String
  }
type alias RoleDataInfo =
  { description: String
  , label: Maybe String
  }
type alias ValidatorDataInfo =
  { description: String
  , label: Maybe String
  }
type alias ModificatorDataInfo =
  { description: String
  , label: Maybe String
  }
type alias Value = AbstractValue.Value String String EdgeData EdgeDataInfo
type Msg
  = MsgChangeAddedEdgeFrom String 
  | MsgChangeAddedEdgeTo String
  | MsgChangeAddedStatus String
  | MsgChangeAddedRole String
  | MsgChangeAddedValidator String
  | MsgChangeAddedModificator String
  | MsgSelectEdge Edge Bool
  | MsgSelectStatus String Bool
  | MsgSelectRole String Bool
  | MsgSelectValidator String Bool
  | MsgSelectModificator String Bool
  | MsgSwitchEdge Edge
  | MsgAddNewEdge
  | MsgAddEdge Edge
  | MsgAddNewStatus
  | MsgAddStatus String
  | MsgAddNewRole
  | MsgAddRole String
  | MsgAddNewValidator
  | MsgAddValidator String
  | MsgAddNewModificator
  | MsgAddModificator String
  | MsgActivateEdge Edge
  | MsgActivateStatus String
  | MsgActivateRole String
  | MsgActivateValidator String
  | MsgActivateModificator String
  | MsgEditEdge Edge
  | MsgEditStatus String
  | MsgEditRole String
  | MsgEditValidator String
  | MsgEditModificator String
  | MsgChangeRoleInEdge Edge String Bool
  | MsgChangeValidatorInEdge Edge (Maybe String)
  | MsgChangeModificatorInEdge Edge (Maybe String)
  | MsgChangeDescriptionInEdge Edge String
  | MsgChangeLabelInStatus String (Maybe String)
  | MsgChangeDescriptionInStatus String  String
  | MsgChangeImageInStatus String (Maybe String)
  | MsgLoadImageInStatusFromFile String
  | MsgSelectImageFileInStatus String File.File
  | MsgChangeLabelInRole String (Maybe String)
  | MsgChangeDescriptionInRole String String
  | MsgChangeLabelInValidator String (Maybe String)
  | MsgChangeDescriptionInValidator String String
  | MsgChangeLabelInModificator String (Maybe String)
  | MsgChangeDescriptionInModificator String String
  | MsgMoveEdgeStart Edge
  | MsgMoveEdgeEnd Edge
  | MsgMoveEdgeUp Edge
  | MsgMoveEdgeDown Edge
  | MsgClearEdge Edge
  | MsgRemoveEdge Edge
  | MsgResetCurrentEdge
  | MsgMoveStatusStart String
  | MsgMoveStatusEnd String
  | MsgMoveStatusUp String
  | MsgMoveStatusDown String
  | MsgClearStatus String
  | MsgRemoveStatus String
  | MsgStartRenameStatus String
  | MsgRenameStatus String String
  | MsgResetCurrentStatus
  | MsgMoveRoleStart String
  | MsgMoveRoleEnd String
  | MsgMoveRoleUp String
  | MsgMoveRoleDown String
  | MsgClearRole String
  | MsgRemoveRole String
  | MsgStartRenameRole String
  | MsgRenameRole String String
  | MsgResetCurrentRole
  | MsgMoveValidatorStart String
  | MsgMoveValidatorEnd String
  | MsgMoveValidatorUp String
  | MsgMoveValidatorDown String
  | MsgClearValidator String
  | MsgRemoveValidator String
  | MsgStartRenameValidator String
  | MsgRenameValidator String String
  | MsgResetCurrentValidator
  | MsgMoveModificatorStart String
  | MsgMoveModificatorEnd String
  | MsgMoveModificatorUp String
  | MsgMoveModificatorDown String
  | MsgClearModificator String
  | MsgRemoveModificator String
  | MsgStartRenameModificator String
  | MsgRenameModificator String String
  | MsgResetCurrentModificator
  | MsgLoadFromFile
  | MsgLoadFromUrl String
  | MsgUrlLoaded (Result Http.Error String)
  | MsgFileSelected File.File
  | MsgFileLoaded String
  | MsgSeveToFile
  | MsgSelectTab Tab
  | MsgMoveSelectedEdgesStart
  | MsgMoveSelectedEdgesEnd
  | MsgMoveSelectedStatusesStart
  | MsgMoveSelectedStatusesEnd
  | MsgMoveSelectedRolesStart
  | MsgMoveSelectedRolesEnd
  | MsgMoveSelectedValidatorsStart
  | MsgMoveSelectedValidatorsEnd
  | MsgMoveSelectedModificatorsStart
  | MsgMoveSelectedModificatorsEnd
  | MsgShowJSON
  | MsgClearJSON
  | MsgClearValue
  | MsgChangeLoadedUrl String
  | MsgChangeIdent Int
  | MsgChangeDefaultFileName String
  | MsgChangePreferLabel Bool
  | MsgNone
type alias AddedEdge =
  { from: Maybe String
  , to: Maybe String
  }
type alias Config =
  { ident: Int
  , filename: String
  , preferLabel: Bool
  }  
type alias Model =
  { value: Value
  , json: String
  , tab: Tab
  , state:
    { currentEdge: Maybe.Maybe Edge
    , currentStatus: Maybe String
    , currentRole: Maybe String
    , currentValidator: Maybe String
    , currentModificator: Maybe String
    , addedEdge: AddedEdge
    , addedStatus: String
    , addedRole: String
    , addedValidator: String
    , addedModificator: String
    , selectedEdges: Set.Set Edge
    , selectedStatuses: Set.Set String
    , selectedRoles: Set.Set String
    , selectedValidators: Set.Set String
    , selectedModificators: Set.Set String
    , loadedUrl: String
    }
  , config: Config
  }
type Tab
  = TabUtils
  | TabEdges
  | TabEdgeData
  | TabStatuses
  | TabStatusData
  | TabRenameStatus
  | TabChangeStatusImage
  | TabRoles
  | TabRoleData
  | TabRenameRole
  | TabValidators
  | TabValidatorData
  | TabRenameValidator
  | TabModificatirs
  | TabModificatorData
  | TabRenameModificator
  | TabAnalytics
  | TabConfig
  | TabLoadedUrl
  | TabHelp

emptyEdgeData =
  { roles = Set.empty
  , validator = Nothing
  , modificator = Nothing
  , description = ""
  }
emptyStatusData =
  { description = ""
  , label = Nothing
  , image = Nothing
  }
emptyDataInfo =
  { description = ""
  , label = Nothing
  }
emptyRoleData = emptyDataInfo
emptyValidatorData = emptyDataInfo
emptyModificatorData = emptyDataInfo
emptyValue =
  { statuses = OrderedDict.empty
  , edges = OrderedDict.empty
  , edgeDataInfo =
    { roles = OrderedDict.empty
    , validators = OrderedDict.empty
    , modificators = OrderedDict.empty
    }
  }
defaultConfig =
  { ident = 0
  , filename = "statuses.json"
  , preferLabel = False
  }
init =
  { value = emptyValue
  , json = ""
  , tab = TabUtils
  , state =
    { currentEdge = Nothing
    , currentStatus = Nothing
    , currentRole = Nothing
    , currentValidator = Nothing
    , currentModificator = Nothing
    , addedEdge =
      { from = Nothing
      , to = Nothing
      }
    , addedStatus = ""
    , addedRole = ""
    , addedValidator = ""
    , addedModificator = ""
    , selectedEdges = Set.empty
    , selectedStatuses = Set.empty
    , selectedRoles = Set.empty
    , selectedValidators = Set.empty
    , selectedModificators = Set.empty
    , loadedUrl = ""
    }
  , config = defaultConfig
  }
