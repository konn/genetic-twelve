{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (join)
import Control.Monad.Random (MonadRandom, Rand, StdGen, mkStdGen, uniform, weighted)
import Control.Monad.Random.Lazy (evalRand, evalRandIO, runRand)
import Control.Monad.Trans.Random (RandGen (RandGen))
import Data.Foldable (foldl')
import Data.List (foldl1')
import qualified Data.Vector as V
import Euterpea
import GHC.Generics (Generic)
import System.Random.MWC (Variate (uniformR))
import System.Random.MWC.Distributions (normal, standard)
import System.Random.Shuffle (shuffleM)

pitches :: [PitchClass]
pitches = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, Bf, B]

newtype V12 a = V12 [a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype V11 a = V11 [a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pattern Vec12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V12 a
pattern Vec12 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
  V12 [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12]

{-# COMPLETE Vec12 :: V12 #-}

pattern Vec11 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V11 a
pattern Vec11 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
  V11 [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{-# COMPLETE Vec11 :: V11 #-}

normalOcts :: Rand StdGen Octave
normalOcts = floor <$> normal 4.0 1.5 (RandGen @StdGen)

data Timing = Seq | Par
  deriving (Show, Eq, Ord, Generic)

data Phrase = MkPhrase
  { octaves :: V12 Octave
  , durs :: V12 Dur
  , timings :: V11 Timing
  }
  deriving (Show, Eq, Ord, Generic)

type ToneRow = V12 PitchClass

phraseM :: Rand StdGen Phrase
phraseM =
  MkPhrase <$> sequence (pure normalOcts)
    <*> sequence (pure $ realToFrac . abs <$> normal 0.5 0.5 (RandGen @StdGen))
    <*> sequence (pure $ weighted [(Seq, 0.7), (Par, 0.3)])

fromPhrase :: ToneRow -> Phrase -> Music Pitch
fromPhrase tones MkPhrase {timings = V11 seps, ..} =
  let V12 notes = note <$> durs <*> ((,) <$> tones <*> octaves)
   in go seps notes
  where
    go (Seq : seps) (n : ns) = n :+: go seps ns
    go (Par : seps) (n : ns) =
      let (pars, seps') = span (== Par) seps
          (chs, ns') = splitAt (length pars) ns
       in chord (n : chs) :+: go seps' ns'
    go [] ns = line ns
    go _ _ = rest 0

data TwelveTones
  = Trans !Int TwelveTones
  | Inv TwelveTones
  | Ret TwelveTones
  | Then TwelveTones TwelveTones
  | Repeat !Int TwelveTones
  | Single Phrase
  | None
  deriving (Show, Eq, Ord, Generic)

fromTwelveTones :: ToneRow -> TwelveTones -> Music Pitch
fromTwelveTones tones (Trans n tt) =
  transpose n $ fromTwelveTones tones tt
fromTwelveTones tones (Inv tt) =
  invert $ fromTwelveTones tones tt
fromTwelveTones tones (Ret tt) =
  let orig = fromTwelveTones tones tt
   in orig :+: retro orig
fromTwelveTones tones (Then l r) =
  fromTwelveTones tones l :+: fromTwelveTones tones r
fromTwelveTones tones (Repeat n tt) = line $ replicate n $ fromTwelveTones tones tt
fromTwelveTones tones (Single ph) = fromPhrase tones ph
fromTwelveTones tones None = rest 0

type Potential = Rational

twelveToneWithM :: Potential -> Rand StdGen TwelveTones
twelveToneWithM = go Nothing
  where
    go prev p
      | p <= 0 = Single <$> phraseM
      | otherwise = do
        join $
          weighted
            [ (mkTrans, if prev == Just 0 then 0 else 15)
            , (mkInv, if prev == Just 1 then 0 else 15)
            , (mkRet, if prev == Just 2 then 0 else 15)
            , (mkThen, 25)
            , (mkRepeat, if prev == Just 4 then 0 else 5)
            ]
      where
        mkTrans = do
          i <- uniform [0 .. 11]
          Trans i <$> go (Just 0) (p - 1)
        mkInv = Inv <$> go (Just 1) (p - 1)
        mkRet = Ret <$> go (Just 2) ((p - 1) / 2)
        mkThen = Then <$> go (Just 3) ((p - 1) / 2) <*> go (Just 4) ((p - 1) / 2)
        mkRepeat = do
          rep <- floor <$> normal 2.5 0.75 (RandGen @StdGen)
          Repeat rep <$> go (Just 4) ((p - fromIntegral rep) / 2)

instance Applicative V12 where
  pure x = V12 $ replicate 12 x
  V12 fs <*> V12 xs = V12 $ zipWith ($) fs xs

instance Applicative V11 where
  pure x = V11 $ replicate 11 x
  V11 fs <*> V11 xs = V11 $ zipWith ($) fs xs

main :: IO ()
main = do
  pitches <- V12 <$> shuffleM pitches
  print pitches
  dodeca <- evalRandIO $ twelveToneWithM 25
  print dodeca
  let music = tempo 5 $ fromTwelveTones pitches dodeca
  -- play music
  let performance = perform $ instrument AcousticGrandPiano music
  exportMidiFile "generated.mid" $ toMidi performance
  pure ()
