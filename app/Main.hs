module Main where

import Codec.Picture
import Debug.Trace
import Linear.Metric
import Linear.V2
import Linear.V3
import SDFs
import Utils

scale = 4

width = 1920 * scale

height = 1080 * scale

-- width = 480

-- height = 270

res = V2 (fromIntegral width) (fromIntegral height)

circle :: V2 Double -> V2 Double -> Double -> V3 Double
circle uv p r = V3 w w w
  where
    sdf = circleSDF uv p r
    edge = 0.005
    w = 1 - clamp 0 1 (1 - smoothstep (-edge) edge sdf)

ics off d = 1 - clamp 0 1 (1 - smoothstep (-off) off d)

genimage :: Int -> Int -> PixelRGB8
genimage ix iy =
  do
    let coord = V2 (fromIntegral ix) (fromIntegral iy)
    let x = fromIntegral ix
    let y = fromIntegral iy
    let aspectRatio = fromIntegral width / fromIntegral height
    let uv = coord / V2 (fromIntegral width) (fromIntegral height) * V2 aspectRatio 1

    -- Mickey Mouse head and ears
    let headPos = 0.5
    let earHeight = 0.25
    let earCenterOffset = 0.15
    let c1 = circleSDF uv (V2 (headPos * aspectRatio) 0.5) 0.25
    let c2 = circleSDF uv (V2 ((headPos - earCenterOffset) * aspectRatio) earHeight) 0.15
    let c3 = circleSDF uv (V2 ((headPos + earCenterOffset) * aspectRatio) earHeight) 0.15

    -- Eyes
    let eyeCentralOffset = 0.05
    let eyeLevel = 0.45
    let leftEyePos = V2 ((headPos - eyeCentralOffset) * aspectRatio) eyeLevel
    let rightEyePos = V2 ((headPos + eyeCentralOffset) * aspectRatio) eyeLevel
    let eyeWidth = 0.03
    let eyeHeight = 0.065
    let eye1 = ellipseSDF uv leftEyePos (V2 eyeWidth eyeHeight)
    let eye2 = ellipseSDF uv rightEyePos (V2 eyeWidth eyeHeight)

    -- Pupils
    let pupilHeight = eyeLevel + 0.03
    let pupilcWidth = 0.01
    let pupilcHeight = 0.02
    let pupil1 = ellipseSDF uv (V2 (0.45 * aspectRatio) pupilHeight) (V2 pupilcWidth pupilcHeight)
    let pupil2 = ellipseSDF uv (V2 (0.55 * aspectRatio) pupilHeight) (V2 pupilcWidth pupilcHeight)

    -- Smile
    let lowerMouth = ellipseSDF uv (V2 (0.5 * aspectRatio) 0.65) (V2 0.14 0.05)
    let upperMouth = ellipseSDF uv (V2 (0.5 * aspectRatio) 0.62) (V2 0.13 0.05)

    -- Combine shapes
    let headRaw = c1 `min` c2 `min` c3
    let head = ics 0.005 headRaw
    let eyesRaw = eye1 `min` eye2
    let eyes = ics 0.2 eyesRaw
    let pupilsRaw = pupil1 `min` pupil2
    let pupils = ics 1.4 pupilsRaw
    let noseRaw = ellipseSDF uv (V2 (0.5 * aspectRatio) 0.57) (V2 0.08 0.035)
    let nose = ics 0.09 noseRaw
    let mouthRaw = lowerMouth `max` (-upperMouth)
    let fullMouth = ics 0.1 mouthRaw
    let face = head + eyes + pupils + nose + fullMouth

    let w = face
    let col = V3 w w w

    v3ToPixelRGB8 col

image :: Image PixelRGB8
image = generateImage genimage width height

main :: IO ()
main = writePng "output.png" image
