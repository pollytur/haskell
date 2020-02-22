{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
-- | A fractal tree of a given rank.
lightCircle :: Color -> Double -> Double -> Picture
lightCircle c y r = translated 0 y (colored c (solidCircle r))

tree :: Int -> Double -> Double -> Double -> Picture 
tree 0 m i w = solidPolygon [(0, 0), (w, 0), (w/2, m), (w/2, m)]  <> lightCircle green m (i)
   
tree n m i w= segment <> 
  translated 0 1 (leftBranch <> rightBranch) 
  where
    segment = solidPolygon [(0, 0), (w, 0), (w, 1), (0, 1)]
    leftBranch = translated 0 ((-w)/4) (rotated (pi/8) (tree (n - 1) m (i+0.07) (w/(2))))
    rightBranch = translated (w/2) ((-w)/4) (rotated (-pi/8) (tree (n - 1) m (i+0.07) (w/(2))) )

round1dp :: Double -> Double
round1dp x = fromIntegral (round $ x * 1e1) / 1e1

diff1dp :: Double -> Int
diff1dp t = floor(10*round1dp(t - fromIntegral(floor (t))))

drawTree :: Int -> Double -> Picture
drawTree n t 
  |floor(t)<n && diff1dp t `mod` 10==0 = tree (floor(t)) (0.0) (0.0) (0.6) 
  |floor(t)<n && diff1dp t `mod` 10==1 = tree (floor(t)) (0.1) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==2 = tree (floor(t)) (0.2) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==3 = tree (floor(t)) (0.3) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==4 = tree (floor(t)) (0.4) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==5 = tree (floor(t)) (0.5) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==6 = tree (floor(t)) (0.6) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==7 = tree (floor(t)) (0.7) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==8 = tree (floor(t)) (0.8) (0.0) (0.6)
  |floor(t)<n && diff1dp t `mod` 10==9 = tree (floor(t)) (0.9) (0.0) (0.6)
  |floor(t)>=n=tree (n) (0.0) (0.0) (0.6)

main :: IO ()
main = animationOf (drawTree 5)