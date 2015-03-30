module Network.AWS.ServiceManagement
  ( CloudService
  , cloudServiceName
  , cloudServiceVMs
  , VirtualMachine
  , vmName
  , vmIpAddress
  , vmInputEndpoints
  , Endpoint
  , endpointName
  , endpointPort
  , endpointVip
  , vmSshEndpoint
  , AWSSetup(..)
  , awsSetup
    -- * High-level API
--  , cloudServices
--  , createService
--  , addVM
--  , shutdownVM

  ) where


import Prelude hiding (id, (.))
import Control.Category (id, (.))
import Control.Arrow (arr)
import Control.Monad (forM)
import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary (Binary(get, put))
import Data.Binary.Put (runPut)
import Data.Binary.Get (Get, runGet)
import Data.CaseInsensitive as CI (mk)
import Data.Either
import Data.PEM (PEM(..), pemParseBS)
import Data.Certificate.X509
import Crypto.Types.PubKey.RSA
import qualified Data.Certificate.KeyRSA as KeyRSA
import Network.TLS


data HostedService = HostedService {
    hostedServiceName :: String
  }

data CloudService = CloudService {
    cloudServiceName :: String
  , cloudServiceVMs  :: [VirtualMachine]
  }

-- | Virtual machine
data VirtualMachine = VirtualMachine {
    vmName           :: String
  , vmIpAddress      :: String
  , vmInputEndpoints :: [Endpoint]
  }

data Endpoint = Endpoint {
    endpointName :: String
  , endpointPort :: String
  , endpointVip  :: String
  }


-- | Find the endpoint with name @SSH@.
vmSshEndpoint :: VirtualMachine -> Maybe Endpoint
vmSshEndpoint vm = listToMaybe
  [ ep
  | ep <- vmInputEndpoints vm
  , endpointName ep == "SSH"
  ]

data AWSSetup = AWSSetup
  {
    certificate :: X509
  , privateKey :: PrivateKey
  }

encodePrivateKey :: PrivateKey -> BL.ByteString
encodePrivateKey (PrivateKey pub d p q dP dQ qinv) = runPut $ do
  put (pub)
  put (d)
  put (p)
  put (q)
  put (dP)
  put (dQ)
  put (qinv)

decodePrivateKey :: BL.ByteString -> PrivateKey
decodePrivateKey = runGet getPrivateKey
    where
        getPrivateKey :: Get PrivateKey
        getPrivateKey =
            PrivateKey <$> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Binary PublicKey where
    put (PublicKey psize n e) = do
        put psize
        put n
        put e
    get = do
        psize <- get
        n <- get
        e <- get
        return $ PublicKey psize n e

instance Binary AWSSetup where
  put (AWSSetup cert pkey) = do
    put (encodeCertificate cert)
    put (encodePrivateKey pkey)
  get = do
    Right cert <- decodeCertificate <$> get
    pkey <- decodePrivateKey <$> get
    return $ AWSSetup cert pkey

awsSetup :: String
         -> String
         -> IO AWSSetup
awsSetup certPath pkeyPath = do
  cert <- fileReadCertificate certPath
  pkey <- fileReadPrivateKey pkeyPath
  return AWSSetup {
      certificate    = cert
    , privateKey     = pkey
    }

fileReadCertificate :: FilePath -> IO X509
fileReadCertificate filepath = do
    certs <- rights . parseCerts . pemParseBS <$> B.readFile filepath
    case certs of
        []    -> error "no valid certificate found"
        (x:_) -> return x
    where parseCerts (Right pems) = map (decodeCertificate . BL.fromChunks . (:[]) . pemContent)
                                  $ filter (flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . pemName) pems
          parseCerts (Left err) = error ("cannot parse PEM file " ++ show err)

fileReadPrivateKey :: FilePath -> IO PrivateKey
fileReadPrivateKey filepath = do
    pk <- rights . parseKey . pemParseBS <$> B.readFile filepath
    case pk of
        []    -> error "no valid RSA key found"
        (x:_) -> return x

    where parseKey (Right pems) = map (fmap (snd) . KeyRSA.decodePrivate . BL.fromChunks . (:[]) . pemContent)
                                $ filter ((== "RSA PRIVATE KEY") . pemName) pems
          parseKey (Left err) = error ("Cannot parse PEM file " ++ show err)

-- cloudServices :: IO [CloudService]
{--
createService :: String -> IO CloudService
createService tag = do
    return ()

addVM :: CloudService -> IO ()
addVM cserv = do
    return ()
--}
-- shutdownVM :: CloudService -> IO ()

-- createAWSVM :: String -> IO VirtualMachine
