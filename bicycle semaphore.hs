{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

import Data.Text (Text)
import CodeWorld
-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Color -> Double -> Picture
lightCircle c y = translated 0 y (colored c (solidCircle 1))
-- | Green traffic light.
greenLight :: Picture
greenLight = lightCircle green (-1.2) 

redLight :: Picture
redLight = lightCircle red 1.2

-- | Frame for traffic lights.
frame :: Picture
frame = rectangle 2.5 5.0

drawByke :: Text ->Double -> Picture
drawByke x y = translated 0 y (lettering x)

-- | Simple traffic lights with two states. --
-- * 'True' — green light
-- * 'False' — red light
 
  
-- <> drawByke "\x1F6B6" (-1.2)
trafficLights :: Int -> Picture 

trafficLights 0 = frame <> drawByke "\x1F6B6" (-1.2) <> greenLight 
trafficLights 2 = frame <> redLight
trafficLights 1 = frame <> greenLight

round1dp :: Double -> Double
round1dp x = fromIntegral (round $ x * 1e1) / 1e1

diff1dp :: Double -> Int
diff1dp t = floor(10*round1dp(t - fromIntegral(floor (t))))

trafficController :: Double -> Picture
trafficController t
  |(floor (t) `mod` 8) <= 2  = trafficLights 0
  |(floor (t) `mod` 8 <= 3) && diff1dp t `mod` 2==1 = trafficLights 1
  |(floor (t) `mod` 8 <= 3) && diff1dp t `mod` 2==0 = trafficLights 0
  |(floor (t) `mod` 8) >3  = trafficLights 2
  

main :: IO()
main = animationOf trafficController