{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Color -> Double -> Picture
lightCircle c y = translated 0 y (colored c (solidCircle 1))
-- | Green traffic light.
greenLight :: Picture
greenLight = lightCircle green (-2.5)
redLight :: Picture
redLight = lightCircle red 2.5

yellowLight :: Picture
yellowLight = lightCircle yellow 0.0

-- | Frame for traffic lights.
frame :: Picture
frame = rectangle 2.5 7.5
-- | Simple traffic lights with two states. --
-- * 'True' — green light
-- * 'False' — red light
trafficLights :: Int -> Picture 
trafficLights 0 = frame <> greenLight <> lightCircle grey 2.5 <> lightCircle grey 0.0
trafficLights 1 = frame <> yellowLight <> lightCircle grey 2.5 <> lightCircle grey (-2.5)
trafficLights 2 = frame <> redLight <> lightCircle grey (-2.5) <> lightCircle grey 0.0
trafficLights 3 = frame <> redLight <> yellowLight <> lightCircle grey (-2.5) 


trafficController ::  Double -> Picture
trafficController t
  |(floor (t) `mod` 8) <= 2  = trafficLights 0 
  |(floor (t) `mod` 8 <= 3)  = trafficLights 1
  |(floor (t) `mod` 8) <= 6  = trafficLights 2
  |(floor (t) `mod` 8) > 6 = trafficLights 3

main :: IO()
main = animationOf trafficController