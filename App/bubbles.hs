{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bubbles
  ( Cell
  , Matrix(..)
  , Neighborhood(..)
  , World(..)
  , ColorMode(..)
  , generateMatrix
  , update
  , initialWorld
  , handleEvent
  , drawWorld
  , drawWorldWithColor
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Parallel.Strategies
import Control.Monad (forM_, when)

data ColorMode = Red | Green | Blue | White | Orange | Rainbow deriving (Show, Eq)



type Cell = Float
data Matrix = Matrix {
    matWidth :: !Int,
    matHeight :: !Int,
    matData :: !(V.Vector Cell)
} deriving (Show)

data Neighborhood  = Neighborhood {
    north ::  Cell,
    south ::  Cell,
    east ::  Cell,
    west ::  Cell,
    northEast ::  Cell,
    northWest ::  Cell,
    southEast ::  Cell,
    southWest ::  Cell
}


--north and south dont really matter
flowBiasPro :: Neighborhood
flowBiasPro = Neighborhood {
    north = 1,
    south = 1,
    east = 1.1,
    west = 0.8,
    northEast = 1.1,
    northWest = 0.8,
    southEast = 0.5,
    southWest = 0.2
}

flowBiasCon :: Neighborhood
flowBiasCon = Neighborhood {
    north = 1, 
    south = 1,
    east = 0.8,
    west = 1.1,
    northEast = 0.8,
    northWest = 1.1,
    southEast = 0.2,
    southWest = 0.5
}


{-# INLINE lambda #-}
lambda :: (Float, Float, Float, Float, Float, Float) -> Cell -> Neighborhood -> Cell
lambda (eWeight, wWeight, neWeight, nwWeight, seWeight, swWeight) cell (Neighborhood n s e w ne nw se sw) =
        let

        !neighborSum = n + s + e + w + ne + nw + se + sw
        !maxNeighbor = max n (max s (max e (max w (max ne (max nw (max se sw))))))
        !aliveCount = (if n > 0.5 then 1 else 0 :: Int) + (if s > 0.5 then 1 else 0) +
                     (if e > 0.5 then 1 else 0) + (if w > 0.5 then 1 else 0) +
                     (if ne > 0.5 then 1 else 0) + (if nw > 0.5 then 1 else 0) +
                     (if se > 0.5 then 1 else 0) + (if sw > 0.5 then 1 else 0)

        -- RULE 1: GROWTH 
        growth | maxNeighbor > 0.5 =  maxNeighbor * 0.86
                |otherwise = 0

        -- RULE 2: MERGING 
        weightedNeighborSum = (n * 1.25) + (e * eWeight) + (ne * neWeight) +
                              (nw * nwWeight) + (se * seWeight) +
                              (s * 0.3) + (w * wWeight) + (sw * swWeight)
        weightedAvg = weightedNeighborSum / 6.0  
        mergeBoost = if aliveCount >= 2 && weightedAvg > 1.0
                    then (cell + weightedAvg) * 0.55
                    else cell



        -- RULE 3: PRESSURE 
        pressure | neighborSum > 20.0 = 1.02  -- expand when crowded
                 | neighborSum > 10.0 =  1.0   -- neutral
                 | otherwise = 0.96  -- sparse

        -- RULE 4: DECAY
        decayRate | aliveCount == 0 = 0.89     -- isolated
                  | aliveCount <= 2 = 0.92  -- lonely
                  | aliveCount >= 7 = 0.885  -- overcrowded
                  | otherwise = 0.95                          -- active

        -- combine rules 
        combined = max growth mergeBoost * pressure * decayRate
        in min 10.0 combined




{-# INLINE getNeighborhood #-}
getNeighborhood :: Matrix -> Int -> Int -> Neighborhood
getNeighborhood (Matrix w h vec) x y = Neighborhood {
    north = getCellUnsafe (x, y - 1),
    south = getCellUnsafe (x, y + 1),
    east = getCellUnsafe (x + 1, y),
    west = getCellUnsafe (x - 1, y),
    northEast = getCellUnsafe (x + 1, y - 1),
    northWest = getCellUnsafe (x - 1, y - 1),
    southEast = getCellUnsafe (x + 1, y + 1),
    southWest = getCellUnsafe (x - 1, y + 1)
}
  where
    {-# INLINE getCellUnsafe #-}
    getCellUnsafe (i, j) =
      let !i' = ((i `mod` w) + w) `mod` w
          !j' = ((j `mod` h) + h) `mod` h
      in V.unsafeIndex vec (j' * w + i')

generateMatrix :: Int -> Int -> IO Matrix
generateMatrix width height = do
  let emptyMatrix = Matrix width height (V.replicate (width * height) 0)
  fst . addRandomBubbles 10 emptyMatrix <$> newStdGen

{-# INLINE updateMatrix #-}
updateMatrix :: Float -> ((Float, Float, Float, Float, Float, Float) -> Cell -> Neighborhood -> Cell) -> Matrix -> Matrix
updateMatrix time lam matrix@(Matrix w h vec) =
  let !swayFactor = (sin (time * 2 * pi / 15) + 1) / 2 - 0.5
      
      proWeights = (east flowBiasPro, west flowBiasPro, northEast flowBiasPro,
                    northWest flowBiasPro, southEast flowBiasPro, southWest flowBiasPro)
      conWeights = (east flowBiasCon, west flowBiasCon, northEast flowBiasCon,
                    northWest flowBiasCon, southEast flowBiasCon, southWest flowBiasCon)
      
      interpolate a b t = a + (b - a) * t
      
      !computedWeights =
        let (eL, wL, neL, nwL, seL, swL) = proWeights
            (eR, wR, neR, nwR, seR, swR) = conWeights
        in ( interpolate eL eR swayFactor
           , interpolate wL wR swayFactor
           , interpolate neL neR swayFactor
           , interpolate nwL nwR swayFactor
           , interpolate seL seR swayFactor
           , interpolate swL swR swayFactor
           )


      rowResults = [computeRow rowIdx | rowIdx <- [0 .. h - 1]] `using` parList rdeepseq

      computeRow rowIdx =
        let start = rowIdx * w
        in V.fromListN w [computeCell idx | idx <- [start .. start + w - 1]]


      newVec = V.concat rowResults

      {-# INLINE computeCell #-}
      computeCell idx =
          let (!y, !x) = idx `divMod` w
              !cell = V.unsafeIndex vec idx
              !neighborhood = getNeighborhood matrix x y
          in lam computedWeights cell neighborhood
  in Matrix w h newVec



addRandomBubbles :: Int -> Matrix -> StdGen -> (Matrix, StdGen)
addRandomBubbles count (Matrix w h vec) gen =
  let matrixSize = fromIntegral (min w h) :: Float
      minRadius = matrixSize * 0.02
      maxRadius = matrixSize * 0.07
      
      generateBubbles 0 generator accumulator = (accumulator, generator)
      generateBubbles remaining generator accumulator =
        let (centerX, gen1) = randomR (5, w - 6) generator
            (centerY, gen2) = randomR (5, h - 6) gen1
            (radius, gen3) = randomR (minRadius, maxRadius) gen2
            bubble = (centerX, centerY, radius)
        in generateBubbles (remaining - 1) gen3 (bubble : accumulator)
      
      (bubbles, finalGen) = generateBubbles count gen []
      
      updateCells mutableVec = forM_ bubbles $ \(centerX, centerY, seedRadius) -> do
        let radiusInt = ceiling seedRadius
            boundsMinX = max 0 (centerX - radiusInt)
            boundsMaxX = min (w - 1) (centerX + radiusInt)
            boundsMinY = max 0 (centerY - radiusInt)
            boundsMaxY = min (h - 1) (centerY + radiusInt)
            effectRadius = seedRadius * 1.5
            invEffectRadius = if effectRadius == 0 then 0 else 1 / effectRadius
        
        forM_ [boundsMinY .. boundsMaxY] $ \y ->
          forM_ [boundsMinX .. boundsMaxX] $ \x -> do
            let cellIndex = y * w + x
                deltaX = fromIntegral (x - centerX) :: Float
                deltaY = fromIntegral (y - centerY) :: Float
                distance = sqrt (deltaX * deltaX + deltaY * deltaY)
            
            when (distance <= effectRadius) $ do
              currentValue <- MV.unsafeRead mutableVec cellIndex
              let falloff = max 0 (1 - distance * invEffectRadius)
                  bubbleValue = 10.0 * falloff
                  newValue = min 10.0 (currentValue + bubbleValue)
              MV.unsafeWrite mutableVec cellIndex newValue
      
      newVec = V.modify updateCells vec
  in (Matrix w h newVec, finalGen)

data World = World
    !Matrix        -- worldMatrix
    !Matrix        -- worldPrevMatrix 
    !StdGen        -- worldGen
    !Float         -- worldPhysicsCounter
    !Float         -- worldSpawnCounter
    !(Maybe (Float, Float, Float)) -- mouse position and duration
    !Float         -- time elapsed


update :: Float -> Float -> Float -> World -> World
update dt spawnInterval fps (World matrix prevMatrix gen physicsCounter spawnCounter mouseDown globalTime) =
  let

      newPhysicsCounter = physicsCounter + dt
      newSpawnCounter = spawnCounter + dt
      newGlobalTime = globalTime + dt
      timeStep = 1/fps 


      (matrixAfterSpawn, gen1, finalSpawnCounter) =
        if newSpawnCounter >= spawnInterval
        then let (numBubbles, g1) = randomR (3, 6) gen
                 (newMat, g2) = addRandomBubbles numBubbles matrix g1
             in (newMat, g2, newSpawnCounter - spawnInterval)
        else (matrix, gen, newSpawnCounter)


      processPhysics :: Matrix -> Matrix -> Float -> (Matrix, Matrix, Float)
      processPhysics prev mat accTime
        | accTime >= timeStep =
            let newMat = updateMatrix newGlobalTime lambda mat  
            in processPhysics mat newMat (accTime - timeStep)
        | otherwise = (prev, mat, accTime)

      (finalPrevMatrix, finalMatrix, finalPhysicsCounter) = processPhysics prevMatrix matrixAfterSpawn newPhysicsCounter

      newMouseDown = case mouseDown of
                      Just (x, y, duration) -> Just (x, y, duration + dt)
                      Nothing -> Nothing
  in World finalMatrix finalPrevMatrix gen1 finalPhysicsCounter finalSpawnCounter newMouseDown newGlobalTime

initialWorld :: (Int, Int) -> IO World
initialWorld (width, height) = do
  matrix <- generateMatrix width height
  gen <- newStdGen
  return $ World matrix matrix gen 0 0 Nothing 0

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) (World matrix@(Matrix w h _) prevMatrix gen physCounter spCounter _ globalTime) =

  let spacing = 4 :: Float
      halfWidth = fromIntegral w * spacing / 2
      halfHeight = fromIntegral h * spacing / 2
      gridX = round ((mx + halfWidth) / spacing) :: Int
      gridY = round ((my + halfHeight) / spacing) :: Int

      deleteRadius = round (fromIntegral (min w h) * 0.04 :: Float)
      newMatrix = deleteCells matrix gridX gridY deleteRadius
  in World newMatrix prevMatrix gen physCounter spCounter (Just (mx, my, 0)) globalTime

handleEvent (EventKey (MouseButton LeftButton) Up _ _) (World matrix prevMatrix gen physCounter spCounter _ globalTime) =
  World matrix prevMatrix gen physCounter spCounter Nothing globalTime

handleEvent (EventMotion (mx, my)) (World matrix@(Matrix w h _) prevMatrix gen physCounter spCounter mouseDown globalTime) =
  case mouseDown of
    Just (oldX, oldY, duration) ->

      let spacing = 4 :: Float
          halfWidth = fromIntegral w * spacing / 2
          halfHeight = fromIntegral h * spacing / 2
          gridX = round ((mx + halfWidth) / spacing) :: Int
          gridY = round ((my + halfHeight) / spacing) :: Int
          oldGridX = round ((oldX + halfWidth) / spacing) :: Int
          oldGridY = round ((oldY + halfHeight) / spacing) :: Int

          -- smooth trail 
          steps = max (abs (gridX - oldGridX)) (abs (gridY - oldGridY))
          positions = [(oldGridX + (gridX - oldGridX) * i `div` max 1 steps,
                        oldGridY + (gridY - oldGridY) * i `div` max 1 steps)
                       | i <- [0..steps]]

          deleteRadius = round (fromIntegral (min w h) * 0.04 :: Float)
          newMatrix = foldl (\m (x, y) -> deleteCells m x y deleteRadius) matrix positions
      in World newMatrix prevMatrix gen physCounter spCounter (Just (mx, my, duration)) globalTime
    Nothing -> World matrix prevMatrix gen physCounter spCounter Nothing globalTime

handleEvent _ world = world


deleteCells :: Matrix -> Int -> Int -> Int -> Matrix
deleteCells (Matrix w h vec) centerX centerY radius =
  let radiusFloat = fromIntegral radius :: Float
      innerRadius = radiusFloat * 0.5
      
      calculateNewValue cellIndex =
        let (y, x) = cellIndex `divMod` w
            !deltaX = fromIntegral (x - centerX) :: Float
            !deltaY = fromIntegral (y - centerY) :: Float
            !distance = sqrt (deltaX * deltaX + deltaY * deltaY)
            !currentValue = V.unsafeIndex vec cellIndex
        in if distance <= innerRadius
           then 0
           else if distance <= radiusFloat
           then let !falloff = (radiusFloat - distance) / (radiusFloat - innerRadius)
                in currentValue * (1 - falloff * 0.8)
           else currentValue
      
      newVec = V.generate (w * h) calculateNewValue
  in Matrix w h newVec


drawWithTime :: Float -> Matrix -> Picture
drawWithTime time = drawWithTimeAndColor time Rainbow

drawWithTimeAndColor :: Float -> ColorMode -> Matrix -> Picture
drawWithTimeAndColor time colorMode (Matrix width height vec) =
  let spacing = 4 :: Int
      halfWidth = width * spacing `div` 2
      halfHeight = height * spacing `div` 2
      intensityScale = 0.1
      visibilityThreshold = 2.0
      cellSize = 2.8
      quantizeLevels = 8 :: Float
      
      (baseR, baseG, baseB) = getBaseColor colorMode time
      
      getBaseColor Rainbow t =
        let cycle = t * 2 * pi / 20
        in ( (sin cycle + 1) / 2
           , (sin (cycle + 2 * pi / 3) + 1) / 2
           , (sin (cycle + 4 * pi / 3) + 1) / 2
           )
      getBaseColor Red    _ = (1.0, 0.0, 0.0)
      getBaseColor Green  _ = (0.0, 1.0, 0.0)
      getBaseColor Blue   _ = (0.0, 1.0, 1.0)
      getBaseColor White  _ = (1.0, 1.0, 1.0)
      getBaseColor Orange _ = (1.0, 0.5, 0.0)
      
      renderCell cellIndex cellValue accumulator
        | cellValue <= visibilityThreshold = accumulator
        | otherwise =
            let !rawIntensity = cellValue * intensityScale
                !quantizedLevel = floor (rawIntensity * quantizeLevels) :: Int
                !intensity = fromIntegral (min (floor quantizeLevels - 1) quantizedLevel) / quantizeLevels
                (!row, !col) = cellIndex `divMod` width
                !posX = fromIntegral (col * spacing - halfWidth)
                !posY = fromIntegral (row * spacing - halfHeight)
                !red = baseR * intensity
                !green = baseG * intensity
                !blue = baseB * intensity
                !color = makeColor red green blue (intensity * 0.9)
                cellPicture = Color color (translate posX posY (rectangleSolid cellSize cellSize))
            in cellPicture : accumulator
      
      cellPictures = V.ifoldr' renderCell [] vec
  in Pictures cellPictures

drawWorld :: World -> Picture
drawWorld  = drawWorldWithColor Rainbow 

drawWorldWithColor :: ColorMode -> World -> Picture
drawWorldWithColor colorMode (World matrix prevMatrix _ physicsCounter _ _ globalTime) =
  let timeStep = 0.016
      alpha = min 1.0 (physicsCounter / timeStep)
      interpolatedMatrix = interpolateMatrices prevMatrix matrix alpha
  in drawWithTimeAndColor globalTime colorMode interpolatedMatrix

interpolateMatrices :: Matrix -> Matrix -> Float -> Matrix
interpolateMatrices (Matrix w h vec1) (Matrix _ _ vec2) alpha =
  let newVec = V.generate (w * h) (\idx ->
        let !prev = V.unsafeIndex vec1 idx
            !curr = V.unsafeIndex vec2 idx
        in prev * (1.0 - alpha) + curr * alpha)
  in Matrix w h newVec


