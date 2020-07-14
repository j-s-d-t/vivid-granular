{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid
import Vivid.SynthDef.FromUA as FromUA

-- buff = makeBufferFromFile "/Users/joes/Code/Haskell/vivid-granular/sound/flute-melody-scale.aif"

sndbuf_ :: ToSig s as => s -> FromUA.UA "sndbuf" as
sndbuf_ = UA . toSig

dur_' :: ToSig s as => s -> UA "dur" as
dur_' = UA . toSig

trig freq = 
    impulse (freq_ freq) ? AR

buff :: IO BufferId
buff = newBufferFromFile "/Users/joes/Code/Haskell/vivid-granular/sound/50B-1GA5-D3.aif"


bufGrain :: Args '["trigger", "dur", "sndbuf", "rate", "pos", "interp", "mul", "add"] '[] a => a -> SDBody a Signal
bufGrain = makeUGen
   "BufGrain" AR
   (Vs::Vs '["trigger", "dur", "sndbuf", "rate", "pos", "interp", "mul", "add"])
   NoDefaults


grain = sd () $ do
    b <- buff
    print b
    a <- bufGrain (trigger_ (trig 20), dur_' 1, sndbuf_ (-1), rate_ 1, pos_ 0, interp_ 2, mul_ 1, add_ 0)
    c <- a ~* 0.1
    out 0 [c, c]


-- Shorthand for freeAll

fa :: IO ()
fa =
    freeAll

main = do
    fa
    s <- synth grain ()
    set s ()