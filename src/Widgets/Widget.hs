module Widgets.Widget(
    WidgetType(..),
    Widget(..),
    Widget_(..),
    getWidgetsMap,
    getColor3f
) where

import Data.Map

import Widgets.Objects.ForceBar
import Widgets.Objects.PointsBar
import Widgets.WidgetObject

data WidgetType = ForceBarType | PointsBarType
    deriving (Eq, Ord)

-- Smart constructors

forceBar :: Int -> Widget
forceBar v = Widget (ForceBar v)

pointsBar :: Int -> Widget
pointsBar v = Widget (PointsBar v)

getWidgetsMap :: Map WidgetType Widget
getWidgetsMap = fromList [(ForceBarType,  forceBar 1), 
                          (PointsBarType, pointsBar 7)]