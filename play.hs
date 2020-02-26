import Data.List
import Euterpea

-- C scale
cscale = [C, D, E, F, G, A, B]

-- Shift an input pitch by an integer number of steps.
-- amt: The number of steps to shift by
-- scale: The scale which specifies what a step is
-- (pitch, oct): Input pitch with an octave to be scaled
shift :: Int -> [PitchClass] -> (PitchClass, Int) -> (PitchClass, Int)
shift amt scale (pitch, oct) = (newPitch, newOct)
  where newIdx = case (elemIndex pitch scale) of
                   Just a -> a + amt
                   Nothing -> 0
        newPitch = scale !! (newIdx `mod` (length scale))
        newOct = if newIdx >= length scale then oct + 1 else oct

-- Generates a triad from an input list of notes. triad is with respect to C Major.
triad :: [(PitchClass, Int)] -> [[(PitchClass, Int)]]
triad base = zipWith (\x y -> x:y) base $ zipWith (\x y -> [x,y]) third fifth
  where third = map (shift 2 cscale) base
        fifth = map (shift 4 cscale) base

-- generate a C scale of arbitrary length and starting octave. Always starts with C.
-- len: The number of notes in the scale
-- octave: the octave of the first note
genCScaleClimb :: Int -> Int -> [(PitchClass, Int)]
genCScaleClimb octave len = take len $ zip scaleNotes octProg
  where scaleNotes = cycle cscale
        octProg = concatMap (replicate 7) [octave..]

-- Converts a 2-D list to an array of notes
toMusicList :: Dur -> [[(PitchClass, Int)]] -> [Music Pitch]
toMusicList dur notes = map (chord) noteList
  where noteList = map ( \x -> map (note dur) x ) notes
     
updown :: Int -> Int -> [(PitchClass, Int)]
updown octave n = take (15 * n) $ cycle (scale ++ (tail $ reverse $ tail scale))
  where scale = genCScaleClimb octave 8
