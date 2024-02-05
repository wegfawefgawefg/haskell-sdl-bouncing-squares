module Utils
  ( clamp,
    clampV3,
    smoothstep,
    normf2char,
    v3ToPixelRGB8,
  )
where

import Codec.Picture
import Linear.Metric
import Linear.V2
import Linear.V3

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

-- version of clamp for v3
clampV3 :: Ord a => a -> a -> V3 a -> V3 a
clampV3 lo hi (V3 x y z) = V3 (clamp lo hi x) (clamp lo hi y) (clamp lo hi z)

smoothstep :: Double -> Double -> Double -> Double
smoothstep a b x = t * t * (3 - 2 * t)
  where
    t = clamp ((x - a) / (b - a)) 0.0 1.0

normf2char :: Double -> Pixel8
normf2char f = floor (clamp 0 255 f * 255)

v3ToPixelRGB8 :: V3 Double -> PixelRGB8
v3ToPixelRGB8 (V3 r g b) = PixelRGB8 (normf2char r) (normf2char g) (normf2char b)