{-# LANGUAGE TupleSections #-}
module GenArt.SmoothLife (Config (Config), radii, step, initWeights) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.FFT (Mode (Forward, Inverse), fft2dP)
import Data.Array.Repa.Eval as RE
import Data.Array.Repa.Index ((:.) ((:.)), DIM2, Z (Z))

data Config = Config
  { alphas :: (Double, Double)
  , radii  :: (Double, Double)
  , birth  :: (Double, Double)
  , death  :: (Double, Double)
  }

{-# INLINE toComplex #-}
toComplex = R.map (, 0)

bessel n radius = R.fromFunction (Z :. s :. s :: DIM2) f
  where
    s = 2 ^ n
    h = 2 ^ (n - 1)
    f (Z :. j :. i) = 1 / (1 + exp ((fromIntegral n) * (r - radius)))
      where
        i' = mod (i + h) s - h
        j' = mod (j + h) s - h
        r = sqrt $ fromIntegral (i' * i') + fromIntegral (j' * j')

initWeights n (innerRadius, outerRadius) = (innerW, outerW)
  where
    innerW = R.map (/ (innerSum, 0)) inner
    outerW = R.map (/ (outerSum - innerSum, 0)) $ R.zipWith (-) outer inner
    innerBessel = bessel n innerRadius
    outerBessel = bessel n outerRadius
    innerSum = R.sumAllS innerBessel
    outerSum = R.sumAllS outerBessel
    inner = head . fft2dP Forward $ toComplex innerBessel
    outer = head . fft2dP Forward $ toComplex outerBessel

-- Our state transition function.
{-# INLINE s #-}
s (Config (innerAlpha, outerAlpha) _ (b1, b2) (d1, d2)) inner outer =
  sigma2 outerAlpha outer (lerp b1 d1 alive) (lerp b2 d2 alive)
  where
    alive = sigma innerAlpha inner 0.5 
    lerp a b t = a * (1 - t) + b * t
    sigma alpha x a = 1 / (1 + exp (4 / alpha * (a - x)))
    sigma2 alpha x a b = sigma alpha x a * (1 - sigma alpha x b)

{-# INLINE step #-}
step config innerW outerW f = head $ do
  f' <- fft2dP Forward $ toComplex f
  inner <- fft2dP Inverse $ R.zipWith (*) f' innerW
  outer <- fft2dP Inverse $ R.zipWith (*) f' outerW
  return $ R.zipWith (\i o -> s config (fst i) (fst o)) inner outer

