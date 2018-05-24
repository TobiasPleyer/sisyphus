module Sisyphus.Language.C.Util where


import Sisyphus.Types (Event, State(..))
import Sisyphus.Util (char, str, interleave_shows)


outputSimpleEnum :: Maybe String  -- ^ optional name put at the end of the enum
                 -> String        -- ^ the name of the enum type
                 -> [String]      -- ^ the enum elements
                 -> ShowS         -- ^ the final output function
outputSimpleEnum counter_name enum_name ss =
    str "typedef enum {"
  . str "\n    "
  . (interleave_shows
        (str ",\n    ")
        (map str ss))
  . (maybe id (\cname -> str "\n    "
                       . str cname) counter_name)
  . str ",\n} "
  . str enum_name
  . char ';'


outputStateEnum :: [State] -> ShowS
outputStateEnum = outputSimpleEnum (Just "MAX_NUM_STATES") "state_t" . map stName


outputEventEnum :: [Event] -> ShowS
outputEventEnum = outputSimpleEnum (Just "MAX_NUM_EVENTS") "event_t"


outputTransitionTable = undefined
