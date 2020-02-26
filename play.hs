import Data.List
import Euterpea

cscale = [C, D, E, F, G, A, B]

addLine :: [Music Pitch] -> [Music Pitch] -> [Music Pitch]
addLine xs ys = zipWith (\x y -> x :=: y) xs ys

setOct :: Int -> PitchClass -> (PitchClass, Int)
setOct octave pitch = (pitch, octave)

setDur :: Dur -> (PitchClass, Int) -> Music Pitch
setDur dur = note dur

shift :: Int -> [PitchClass] -> (PitchClass, Int) -> (PitchClass, Int)
shift amt scale (pitch, oct) = (newPitch, newOct)
  where newIdx = case (elemIndex pitch scale) of
                   Just a -> a + amt
                   Nothing -> 0
        newPitch = scale !! (newIdx `mod` (length scale))
        newOct = if newIdx >= length scale then oct + 1 else oct

genScale :: Int -> Dur -> [PitchClass] -> [Music Pitch]
genScale int dur = map (\pitch -> setDur dur $ shift int cscale $ setOct 4 pitch) 

triadScale :: Dur -> [Music Pitch]
triadScale dur = addLine base (addLine third fifth)
  where base = genScale 0 dur cscale
        third = genScale 2 dur cscale
        fifth = genScale 4 dur cscale

