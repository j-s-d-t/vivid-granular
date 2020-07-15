{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid
import Vivid.SynthDef.FromUA as FromUA


dur_' :: ToSig s as => s -> UA "dur" as
dur_' = UA . toSig

trig freq = 
    impulse (freq_ freq) ? AR

buff ::  IO BufferId
buff = makeBufferFromFile "/Users/joes/Code/Haskell/vivid-granular/sound/flute-melody-scale.aif"

bufGrain :: Args '["buf"] '["trigger", "dur", "rate", "pos", "interp", "mul", "add"] a => a -> SDBody a Signal
bufGrain = makeUGen
   "BufGrain" AR
   (Vs::Vs '["buf", "trigger", "dur", "rate", "pos", "interp", "mul", "add"])
   (trigger_ (trig 20), dur_ 1, rate_ 1.0, pos_ 0, interp_ 2, mul_ 1, add_ 0)

grain = sd (1 ::I "buf") $ do
    a <- bufGrain (buf_ (V::V "buf"))
    b <- a ~* 0.1
    out 0 [b, b]


-- Shorthand for freeAll

fa :: IO ()
fa =
    freeAll

main = do
    b <- buff
    s <- synth grain (toI (_unBufferId b) ::I "buf")
    set s (toI (_unBufferId b) ::I "buf")