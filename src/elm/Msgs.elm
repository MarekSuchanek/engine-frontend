module Msgs exposing (..)

import Auth.Msgs
import Navigation exposing (Location)
import UserManagement.Delete.Msgs
import UserManagement.Index.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | UserManagementIndexMsg UserManagement.Index.Msgs.Msg
    | UserManagementDeleteMsg UserManagement.Delete.Msgs.Msg
