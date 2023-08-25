module IOUtils
    ( printEncoding
    ) where

import GHC.IO.Encoding

printEncoding :: IO ()
printEncoding = getLocaleEncoding >>= print
