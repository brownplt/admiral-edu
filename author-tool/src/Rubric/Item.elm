module Rubric.Item (Item(..), style, script) where

import Rubric.Item.Checkbox as Checkbox
import Rubric.Item.FreeForm as FreeForm
import Rubric.Item.Likert as Likert
import Rubric.Item.Select as Select
import Rubric.Item.Utils as Utils

type Item = Checkbox Checkbox.Type
          | FreeForm FreeForm.Type
          | Likert Likert.Type
          | Select Select.Type

style = Checkbox.style ++ 
        FreeForm.style ++ 
        Likert.style ++
        Select.style

script = Utils.script

