
module Manifesto.Hash
    ( SHA1, _SHA1
    , hashFile
    , hashProducer
    ) where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Crypto.Hash.SHA1 as SHA1
import Path
import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import Pipes.Safe
import qualified Pipes.Safe.Prelude as PS
import qualified System.IO as IO

-- TODO: Use a better type
newtype SHA1 = SHA1 { unSHA1 :: BS.ByteString } deriving (Eq, Show)

hashFile :: MonadSafe m => Path Abs File -> m SHA1
hashFile path =
    PS.withFile (toFilePath path) IO.ReadMode (hashProducer . PB.fromHandle)

hashProducer :: Monad m => Producer BS.ByteString m () -> m SHA1
hashProducer = P.fold SHA1.update SHA1.init (SHA1 . SHA1.finalize)

_SHA1 :: Prism' T.Text SHA1
_SHA1 = prism' pp parse
  where
    pp = T.decodeUtf8 . BS16.encode . unSHA1
    -- TODO: Check the length
    parse txt =
        let (valid, invalid) = BS16.decode $ T.encodeUtf8 txt
        in if BS.null invalid then Just (SHA1 valid) else Nothing
