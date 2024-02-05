module SDFs
  ( circleSDF,
    ellipseSDF,
  )
where

import Linear.Metric
import Linear.V2

-- rectangleSDF :: V2 Double -> V2 Double -> V2 Double -> Double
-- rectangleSDF uv p size = max (abs (uv - p) - size / 2) 0 & quadrance - 0.5 * min (size ^. _x) (size ^. _y)

-- roundedRectangleSDF :: V2 Double -> V2 Double -> V2 Double -> Double -> Double
-- roundedRectangleSDF uv p size r = rectangleSDF uv p size + r

-- lineSegmentSDF :: V2 Double -> V2 Double -> V2 Double -> Double -> Double
-- lineSegmentSDF uv a b width = distance uv (project uv a b) - width / 2
--   where
--     project v a b = a + dot (v - a) (b - a) / quadrance (b - a) *^ (b - a)

ellipseSDF :: V2 Double -> V2 Double -> V2 Double -> Double
ellipseSDF uv p r = quadrance ((uv - p) / r) - 1

circleSDF :: V2 Double -> V2 Double -> Double -> Double
circleSDF uv p r = distance uv p - r