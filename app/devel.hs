{-# LANGUAGE PackageImports #-}
import "hsync-server" HSync.Server.Application (develMain)
import Prelude (IO)

main :: IO ()
main = develMain
