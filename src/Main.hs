{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO
import qualified RIO.Text as T

import Types

--import Control.Lens
import Linear.Metric
import Linear.V3
import Linear.Vector
import System.Random

main :: IO ()
main = do
  let screen = Screen 200 100 100
  let lowerLeft = V3 (-2.0) (-1.0) (-1.0)
  let horizontal = V3 4.0 0.0 0.0
  let vertical = V3 0.0 2.0 0.0
  let orgn = V3 0.0 0.0 0.0
  let camera = Camera lowerLeft horizontal vertical orgn
  let world =
        [Sphere (V3 0.0 0.0 (-1.0)) 0.5
        ,Sphere (V3 0.0 (-100.5) (-1.0)) 100
        ]
  let env = Env screen camera world
  runRIO env app

--app :: Env -> IO ()
app :: RIO Env ()
app = do
  ppm <- render
  printPPM ppm

--Random Functions
getRand :: RIO Env Float
getRand = liftIO $ randomRIO (0.0,1.0)

--Functions for PPM
printPPM :: PPM -> RIO Env ()
printPPM (PPM w h d) = do
  let header = "P3\n" <> textDisplay w <> " " <> textDisplay h <> "\n255\n"
  let strDat = map prettyPrintV3 d
  let fileDat = header <> T.unlines strDat
  writeFileUtf8 "./Testing.ppm" fileDat

prettyPrintV3 :: V3 Int -> Text
prettyPrintV3 (V3 x y z) = textDisplay x <> " " <> textDisplay y <> " " <> textDisplay z

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
render :: RIO Env PPM
render = do
  env <- ask
  let Screen x y _samples = envScreen env
  PPM x y <$> renderData

screenPixels :: Int -> Int -> [(Int,Int)]
screenPixels w h = do
  y <- [h-1,h-2..0]
  x <- [0..w-1]
  return (x,y)

renderData ::  RIO Env [V3 Int]
renderData = do
  Screen x y _s <- view screenL
  let pixels = screenPixels x y

  cols <- sequence $ antialiasCol <$> pixels
  let rgb = pixelToRGB <$> cols
  return rgb

pixelToRGB :: V3 Float -> V3 Int
pixelToRGB col = V3 r g b
  where r = truncate $ 255.99 * col ^. _x
        g = truncate $ 255.99 * col ^. _y
        b = truncate $ 255.99 * col ^. _z

antialiasCol ::  (Int,Int) -> RIO Env (V3 Float) --(x, y) -> col
antialiasCol (x',y') = do
  Screen _w _h s <- view screenL
  antialiasCol' x' y' s (V3 0.0 0.0 0.0) ^/ fromIntegral s

antialiasCol' :: Int -> Int -> Int -> V3 Float -> RIO Env (V3 Float) --x y s curCol -> col
antialiasCol' _ _ 0 curCol = pure curCol
antialiasCol' x' y' s' curCol = do
  Screen x y _s <- view screenL
  cam <- view cameraL
  Env _s _c world <- ask
  randX <- getRand
  randY <- getRand
  let u = (fromIntegral x' + randX) / fromIntegral x
  let v = (fromIntegral y' + randY) / fromIntegral y
  let r = getRay cam u v
  let newCol = color r world + curCol
  antialiasCol' x' y' (s' - 1) newCol

hitSphere :: Sphere -> Ray -> Float -> Float -> HitRecord
hitSphere sphere@(Sphere centerArg radiusArg) ray tMin tMax =
  if discriminant > 0
  then if negAnswer < tMax && negAnswer > tMin then mkHitRecord ray sphere negAnswer
  else if posAnswer < tMax && posAnswer > tMin then mkHitRecord ray sphere posAnswer else Miss
  else Miss
  where negAnswer = (-b - sqrt discriminant) / a
        posAnswer = (-b + sqrt discriminant) / a
        discriminant = b*b - 4*a*c
        oc = rayOrigin ray ^-^ centerArg
        a = rayDirection ray `dot` rayDirection ray
        b = 2.0 * (oc `dot` rayDirection ray)
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
