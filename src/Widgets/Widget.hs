module Widgets.Widget(
    Widget(..),
    Widget_(..),
    getWidgetsMap,
    getColor3f
) where

import Data.Map

import Widgets.Objects.ForceBar
import Widgets.Objects.PointsBar
import Widgets.WidgetObject

-- Smart constructors

forceBar :: Float -> Widget
forceBar v = Widget (ForceBar v)

pointsBar :: Float -> Widget
pointsBar v = Widget (PointsBar v)

getWidgetsMap :: Map Int Widget
getWidgetsMap = fromList [(1, forceBar 1), 
                          (2, pointsBar 7)]