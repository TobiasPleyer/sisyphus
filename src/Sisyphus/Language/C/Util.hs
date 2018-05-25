module Sisyphus.Language.C.Util where


import Sisyphus.Types
import Sisyphus.Util


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
