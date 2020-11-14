{-# LANGUAGE BangPatterns, FlexibleContexts, TupleSections #-}
module GenArt.SmoothLife (Config (Config), radii, step, initWeights) where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.FFT (Mode (Forward, Inverse), fft2dP)
import Data.Array.Repa.Index ((:.) ((:.)), DIM2, Z (Z))

data Config = Config
  { alphas :: (Double, Double)
  , radii  :: (Double, Double)
  , birth  :: (Double, Double)
  , death  :: (Double, Double)
  }


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

initWeights n (innerRadius, outerRadius) = do
  let innerBessel = bessel n innerRadius
  let outerBessel = bessel n outerRadius
  inner <- fft2dP Forward $ toComplex innerBessel
  outer <- fft2dP Forward $ toComplex outerBessel
  let diff = R.zipWith (-) outer inner
  let innerSum = R.sumAllS innerBessel
  let outerSum = R.sumAllS outerBessel
  innerW <- R.computeUnboxedP $ R.map (/ (innerSum, 0)) inner
  outerW <- R.computeUnboxedP $ R.map (/ (outerSum - innerSum, 0)) diff
  return (innerW, outerW)

-- Our state transition function.

s (Config (innerAlpha, outerAlpha) _ (b1, b2) (d1, d2)) inner outer =
  sigma2 outerAlpha outer (lerp b1 d1 alive) (lerp b2 d2 alive)
  where
    alive = sigma innerAlpha inner 0.5

    lerp !a !b !t = a * (1 - t) + b * t

    sigma alpha !x !a = 1 / (1 + exp (4 / alpha * (a - x)))

    sigma2 alpha !x !a !b = sigma alpha x a * (1 - sigma alpha x b)


step config innerW outerW f = do
  f' <- fft2dP Forward $ toComplex f
  inner <- fft2dP Inverse $ R.zipWith (*) f' innerW
  outer <- fft2dP Inverse $ R.zipWith (*) f' outerW
  return $ R.zipWith (\(!i, _) (!o, _) -> s config i o) inner outer

