{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid
import Vivid.SynthDef.FromUA as FromUA

buff = makeBufferFromFile "sound/50B-1GA5-D3.aif"

sndbuf_ :: ToSig s as => s -> FromUA.UA "sndbuf" as
sndbuf_ = UA . toSig

dur_' :: ToSig s as => s -> UA "dur" as
dur_' = UA . toSig

trig freq = 
    impulse (freq_ freq) ? AR

bufGrain :: Args '["trigger", "dur", "sndbuf", "rate", "pos", "interp", "mul"] '[] a => a -> SDBody a Signal
bufGrain = makeUGen
   "BufGrain" AR
   (Vs::Vs '["trigger", "dur", "sndbuf", "rate", "pos", "interp", "mul"])
   NoDefaults


grain = sd (0::I "buffer") $ do
   a <- bufGrain (trigger_ (trig 20), dur_' buffer, sndbuf_ 1, rate_ 1, pos_ 0, interp_ 2, mul_ 1)
   b <- 0.1 ~* a
   out 0 [b, b]


-- Shorthand for freeAll

fa :: IO ()
fa =
    freeAll

main = do
    fa
    b <- buff
    s <- synth grain (45::I "buffer")
    set s (b :: I "buffer")
        