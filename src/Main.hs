module Main where

import Control.Lens
import Linear.Metric
import Linear.V3
import Linear.Vector

main :: IO ()
main = do
  let ppm = render 200 100 100
  printPPM ppm

data PPM = PPM
  { width  :: Int
  , height :: Int
  , dat    :: [V3 Int]
  }

data Ray = Ray
  { origin :: V3 Float
  , direction :: V3 Float
  }

data Sphere = Sphere
  { center :: V3 Float
  , radius :: Float
  }

data HitRecord = Hit Float (V3 Float) (V3 Float) | Miss -- Hit t p normal

data Camera = Camera
  { lowerLeftC  :: V3 Float
  , horizontalC :: V3 Float
  , verticalC   :: V3 Float
  , originC     :: V3 Float
  }


--Functions for PPM
printPPM :: PPM -> IO ()
printPPM (PPM w h d) = do
  putStrLn "P3"
  putStrLn $ show  w <> " " <> show h
  putStrLn "255"
  let strDat = map prettyPrintV3 d
  (putStrLn . unlines) strDat

prettyPrintV3 :: Show a => V3 a -> String
prettyPrintV3 (V3 x y z) = show x <> " " <> show y <> " " <> show z

--Functions for Ray
pointAtParam :: Ray -> Float -> V3 Float
pointAtParam (Ray o d) t = o + ((*t) <$> d)

color :: Ray -> [Sphere] -> V3 Float
color ray@(Ray _ d) spheres = case hitRecord of
  Hit t _ _ -> normalColor
    where norm' = normalize (pointAtParam ray t) ^-^ V3 0.0 0.0 (-1.0)
          normalColor = 0.5 *^ V3 (norm' ^. _x + 1.0) (norm' ^. _y + 1.0) (norm' ^. _z + 1.0)
  Miss         -> fallback
    where unitDir = normalize d
          t' = 0.5 * unitDir ^. _y + 1.0
          fallback = (1.0 - t') *^ V3 1.0 1.0 1.0 + t' *^ V3 0.5 0.7 1.0
  where hitRecord = hitSpheres'' spheres ray 0.0 maxFloat
        maxFloat = 100000.0 --This isn't right

--Functions for Camera
getRay :: Camera -> Float -> Float -> Ray --Camera u v ray
getRay (Camera llc horz vert o) u v = Ray o (llc + u *^ horz + v*^vert - o)

--Standalone Things
render :: Int -> Int -> Int -> PPM --x y s
render x y s = PPM x y $ renderData x y s

renderData :: Int -> Int -> Int -> [V3 Int] --x y s
renderData x y s = do
  let lowerLeft = V3 (-2.0) (-1.0) (-1.0)
  let horizontal = V3 4.0 0.0 0.0
  let vertical = V3 0.0 2.0 0.0
  let orgn = V3 0.0 0.0 0.0
  let camera = Camera lowerLeft horizontal vertical orgn
  let spheres =
        [Sphere (V3 0.0 0.0 (-1.0)) 0.5
        ,Sphere (V3 0.0 (-100.5) (-1.0)) 100
        ]
  y' <- [y-1,y-2..0]
  x' <- [0..x-1]
 
  let col = antialiasCol camera spheres x y s x' y'
  let r = truncate $ 255.99 * col ^. _x
  let g = truncate $ 255.99 * col ^. _y
  let b = truncate $ 255.99 * col ^. _z
  return $ V3 r g b

antialiasColOld :: Camera -> [Sphere] -> Int -> Int -> Int -> Int -> Int -> V3 Float -- x y s x' y' -> col
antialiasColOld (Camera lowerLeft horizontal vertical orgn) spheres x y s x' y' = color ray spheres
  where u = fromIntegral x' / fromIntegral x
        v = fromIntegral y' / fromIntegral y
        ray = Ray orgn (lowerLeft + u *^ horizontal + v *^ vertical)

antialiasCol :: Camera -> [Sphere] -> Int -> Int -> Int -> Int -> Int -> V3 Float -- x y s x' y' -> col
antialiasCol cam spheres x y s x' y' = (antialiasCol' cam spheres x y s x' y' s (V3 0.0 0.0 0.0)) ^/ fromIntegral s

antialiasCol' :: Camera -> [Sphere] -> Int -> Int -> Int -> Int -> Int -> Int -> V3 Float -> V3 Float -- x y s x' y' s' curCol -> col
antialiasCol' _ _ _ _ _ _ _ 0 curCol = curCol
antialiasCol' cam spheres x y s x' y' s' curCol = antialiasCol' cam spheres x y s x' y' (s' - 1) newCol
  where u = fromIntegral x' / fromIntegral x --TODO Add random here
        v = fromIntegral y' / fromIntegral y --TODO Add random here
        r = getRay cam u v
        newCol = color r spheres + curCol

hitSphere :: Sphere -> Ray -> Float -> Float -> HitRecord
hitSphere sphere@(Sphere centerArg radiusArg) ray tMin tMax =
  if discriminant > 0
  then if negAnswer < tMax && negAnswer > tMin then mkHitRecord ray sphere negAnswer
  else if posAnswer < tMax && posAnswer > tMin then mkHitRecord ray sphere posAnswer else Miss
  else Miss
  where negAnswer = (-b - sqrt discriminant) / a
        posAnswer = (-b + sqrt discriminant) / a
        discriminant = b*b - 4*a*c
        oc = origin ray ^-^ centerArg
        a = direction ray `dot` direction ray
        b = 2.0 * (oc `dot` direction ray)
        c = oc `dot` oc - radiusArg * radiusArg

mkHitRecord :: Ray -> Sphere -> Float -> HitRecord
mkHitRecord ray (Sphere c r) t = Hit t p normal
  where p = pointAtParam ray t
        normal = (p ^-^ c) ^/ r

hitSpheres'' :: [Sphere] -> Ray -> Float -> Float -> HitRecord
hitSpheres'' spheres ray tMin tMax = go spheres ray tMin tMax Miss

go :: [Sphere] -> Ray -> Float -> Float -> HitRecord -> HitRecord
go (sphere:spheres) ray tMin csf hr = case hit of
    Hit t p n -> go spheres ray tMin t (Hit t p n)
    Miss      -> go spheres ray tMin csf hr
  where hit = hitSphere sphere ray tMin csf
go [] _ _ _ hr = hr
