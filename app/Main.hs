module Main where

import SDL
import Data.Text (pack)
import Linear (V4(..))
import Control.Monad (unless)
import System.Random (randomRIO)


data State = State { 
    world_dims :: (Int, Int),
    entities :: [Entity], 
    input :: Input 
}
data Input = Input { left :: Bool, right :: Bool, up :: Bool, down :: Bool, action :: Bool }
data Entity = Entity { 
    pos :: (Float, Float),
    vel :: (Float, Float),
    acc :: (Float, Float),
    size :: (Int, Int)
}


--------------------- UTILS -------------------
-- clamp bounds of entity to world bounds
clampEntity :: (Int, Int) -> Entity -> Entity
clampEntity (w, h) entity = entity { pos = (x, y) }
    where
        x = max 0 (min (fromIntegral w) (fst (pos entity)))
        y = max 0 (min (fromIntegral h) (snd (pos entity)))

-- move entity based on physics
moveEntity :: Entity -> Entity
moveEntity entity = entity { 
    pos = (x + vx, y + vy),
    vel = (vx + ax, vy + ay)
}
    where
        (x, y) = pos entity
        (vx, vy) = vel entity
        (ax, ay) = acc entity

-- zero out acceleration
zeroAcc :: Entity -> Entity
zeroAcc entity = entity { acc = (0, 0) }


-- gen random velocity
genRandomVel :: IO (Float, Float)
genRandomVel = do
    let m = 0.02
    x <- randomRIO (-m, m)
    y <- randomRIO (-m, m)
    return (x, y)

-- reverse entity velocity if touching a wall
-- depends on the wall being touched
-- if hit left wall, set vel to positive of current vel
-- if hit right wall, set vel to negative of current vel
-- same for y
-- take in the resolution of the world
bounceOnWalls :: Entity -> (Int, Int) -> Entity
bounceOnWalls entity (w, h) = entity { vel = (vx', vy') }
    where
        (x, y) = pos entity
        (vx, vy) = vel entity
        (w', h') = size entity
        vx' = if x <= 0 || x + fromIntegral w' >= fromIntegral w then -vx else vx
        vy' = if y <= 0 || y + fromIntegral h' >= fromIntegral h then -vy else vy



--------------------- INITIALIZATION -------------------
blankInput :: Input
blankInput = Input { left = False, right = False, up = False, down = False, action = False }


-- Helper function to generate a single random entity
makeRandomEntity :: (Int, Int) -> IO Entity
makeRandomEntity (w, h) = do
    size <- randomRIO (1, 3)
    x <- randomRIO (0, w - size)
    y <- randomRIO (0, h - size)
    vel <- genRandomVel
    return Entity { 
        pos = (fromIntegral x, fromIntegral y),
        vel = vel,
        acc = (0, 0),
        size = (size, size)
    }

-- Generate n randomly positioned entities within bounds
makeEntities :: (Int, Int) -> Int -> IO [Entity]
makeEntities bounds n = sequence [makeRandomEntity bounds | _ <- [1..n]]



--------------------- INPUTS -------------------
-- take in state, get inputs, return new state
inputs :: State -> IO State
inputs state = do
    events <- pollEvents
    let input = Input
            { left = any (\event -> case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeA
                _ -> False) events
            , right = any (\event -> case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeD
                _ -> False) events
            , up = any (\event -> case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeW
                _ -> False) events
            , down = any (\event -> case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeS
                _ -> False) events
            , action = any (\event -> case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeSpace
                _ -> False) events
            }
    return state { input = input }


--------------------- UPDATE -------------------
step :: State -> IO State
step state = do
    -- let entities' = map moveEntity (entities state)

    -- we want to move, then zero out acceleration, do bounce, then clamp position
    let moved_entities = map (moveEntity . zeroAcc) (entities state)
    let bounced_entities = map (\entity -> bounceOnWalls entity (world_dims state)) moved_entities
    let next_entities = map (clampEntity (world_dims state)) bounced_entities
    let newState = state { entities = next_entities }

    return newState


--------------------- DRAWING -------------------
drawEntity :: Entity -> Renderer -> IO ()
drawEntity entity renderer = do
    rendererDrawColor renderer $= V4 255 255 255 255
    let (x, y) = pos entity
        (w, h) = size entity
        -- Convert Int to CInt
        rect = Rectangle (P (V2 (fromIntegral $ round x) (fromIntegral $ round y))) (V2 (fromIntegral w) (fromIntegral h))
    fillRect renderer (Just rect)


-- draw all the entities
drawEntities :: State -> Renderer -> IO ()
drawEntities state renderer = do
    mapM_ (\entity -> drawEntity entity renderer) (entities state)


draw :: State -> Renderer -> IO ()
draw state renderer = do
    drawEntities state renderer

--------------------- MAIN LOOP -------------------
appLoop :: State -> Renderer -> IO ()
appLoop state renderer = do

    -- poll for events
    events <- pollEvents
    let quitEvent event = case eventPayload event of
            KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ ||
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
            _ -> False
        quit = any quitEvent events

    state <- inputs state
    state <- step state
    
    ---- DRAWING ----
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    draw state renderer

    present renderer
    unless quit (appLoop state renderer)


main :: IO ()
main = do
    initializeAll

    let resolution = (640, 480)

    window <- createWindow (pack "bouncing_shit") defaultWindow { windowInitialSize = V2 (fromIntegral (fst resolution)) (fromIntegral (snd resolution)) }
    renderer <- createRenderer window (-1) defaultRenderer

    let world_dims = resolution
    entities <- makeEntities world_dims 32
    let state = State { 
        world_dims = world_dims,
        entities = entities, 
        input = blankInput 
    }

    appLoop state renderer

    destroyWindow window
    print "ok"