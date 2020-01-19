module PlaySound
    (
    ) where
import System.Process
playM :: IO ()
playM = callCommand "paplay Gloss/Music/bgmusic.wav"
