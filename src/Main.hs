{-# LANGUAGE DeriveDataTypeable, NoMonadFailDesugaring, TupleSections #-}
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Loops (iterateM_)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Index ((:.) ((:.)), DIM2, Z (Z))
import Data.ConfigFile
import GenArt.SmoothLife
import Graphics.Image (RPU (RPU), readImageY,  writeImage)
import Graphics.Image.Interface (getPxC)
import Graphics.Image.Interface.Repa (fromRepaArrayP, toRepaArray)
import Graphics.Image.Types (Pixel (PixelY), Y (LumaY))
import System.Console.CmdArgs
import System.FilePath ((</>))
import System.Random (getStdGen, randoms)
import Text.Printf

data SmoothLife = SmoothLife
  { config :: FilePath
  , outDir :: FilePath
  , input  :: Maybe FilePath
  , power2 :: Int
  } deriving (Show, Data, Typeable)

smoothLifeMode = SmoothLife
  { config = "config.txt" &= name "c" &= typFile &= help "Config file"
  , outDir = "output" &= name "o" &= typDir &= help "Output directory"
  , input  = def &= name "i" &= typFile &= help "Input image"
  , power2 = 8 &= name "n" &= help "If no input, output will be 2 ^ n square"
  }
  &= summary "SmoothLife in Haskell v0.0.0, (C) Travis Brown"
  &= verbosity &= details
  [ "A very simple implementation of Stephan Rafler's SmoothLife."
  , "Based on an implementation by Mikola Lysenko."
  ]

randomF n = R.fromListUnboxed (Z :. s :. s :: DIM2) . take (s * s) . randoms
  where
    s = 2 ^ n

readGrey :: FilePath -> IO (R.Array R.U DIM2 Double)
readGrey path = R.computeS . R.map (\p -> getPxC p LumaY) . toRepaArray <$> readImageY RPU path

writeGrey :: FilePath -> R.Array R.D DIM2 Double -> IO ()
writeGrey path arr =
  writeImage path . fromRepaArrayP . R.map (PixelY) $ arr

processConfig :: ConfigParser -> Either CPError Config
processConfig c = do
  [ ai, ao ] <- map read . words <$> simpleAccess c "DEFAULT" "alphas"
  [ ri, ro ] <- map read . words <$> simpleAccess c "DEFAULT" "radii"
  [ b1, b2 ] <- map read . words <$> simpleAccess c "DEFAULT" "birth"
  [ d1, d2 ] <- map read . words <$> simpleAccess c "DEFAULT" "death"
  return $ Config (ai, ao) (ri, ro) (b1, b2) (d1, d2)

main = do
  SmoothLife configFile outDir input n <- cmdArgs smoothLifeMode
  Right conf <- (>>= processConfig) <$> readfile emptyCP configFile

  start <- maybe (randomF n <$> getStdGen) readGrey input

  (innerW, outerW) <- initWeights n $ radii conf
  let writeAndStep (f, i) =
        writeGrey (outDir </> (printf "%06d.png" i)) f >>
        (, i + 1) <$> step conf innerW outerW f

  iterateM_ writeAndStep (R.delay $ start, 0 :: Int)

