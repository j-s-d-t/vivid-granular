{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid
import Vivid.SynthDef.FromUA as FromUA


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


grain = sd (0::I "note") $ do
   a <- bufGrain (trigger_ (trig 20), dur_' 1, sndbuf_ (-1), rate_ 1, pos_ 0, interp_ 2, mul_ 1)
   b <- 0.1 ~* a
   out 0 [b, b]


-- Shorthand for freeAll

fa :: IO ()
fa =
    freeAll

main = do
    fa
    s <- synth grain (45::I "note")
    forever $ forM_ [45, 57, 64, 55] $ \freq -> do
        set s (freq :: I "note")
        wait 2.5
        