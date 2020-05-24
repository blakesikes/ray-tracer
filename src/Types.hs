{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO

import Linear.V3

data Env = Env
  { envScreen   :: !Screen
  , envCamera   :: !Camera
  , envWorld    :: !World
  }

data Screen = Screen
  { screenHeight   :: Int
  , screenWidth    :: Int
  , screenSamples  :: Int
  }

data Camera = Camera
  { cameraLowerLeft  :: V3 Float
  , cameraHorizontal :: V3 Float
  , cameraVertical   :: V3 Float
  , cameraOrigin     :: V3 Float
  }

type World = [Sphere]

class HasScreen env where
  screenL :: Lens' env Screen
instance HasScreen Screen where
  screenL = id
instance HasScreen Env where
  screenL = lens envScreen (\x y -> x { envScreen = y })

class HasCamera env where
  cameraL :: Lens' env Camera
instance HasCamera Camera where
  cameraL = id
instance HasCamera Env where
  cameraL = lens envCamera (\x y -> x { envCamera = y })



data PPM = PPM
  { ppmWidth  :: Int
  , ppmHeight :: Int
  , ppmDat    :: [V3 Int]
  }

data Ray = Ray
  { rayOrigin :: V3 Float
  , rayDirection :: V3 Float
  }

data Sphere = Sphere
  { sphereCenter :: V3 Float
  , sphereRadius :: Float
  }

data HitRecord = Hit Float (V3 Float) (V3 Float) | Miss -- Hit t p normal
