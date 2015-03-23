{-# LANGUAGE Arrows #-}
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
  , endpointVi
  , vmSshEndpoint
    -- * Setup
  , AWSSetup(..)
  , awsSetup
    -- * High-level API
  , cloudServices
  ) where


import Prelude hiding (id, (.))
import Control.Category (id, (.))
import Control.Arrow (arr)
import Control.Monad (forM)
import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 as BSC (pack)
import Data.ByteString.Lazy.Char8 as BSLC (unpack)
import Data.Binary (Binary(get, put))
import Data.Binary.Put (runPut)
import Data.Binary.Get (Get, runGet)
import Network.TLS (PrivateKey(PrivRSA))
import Network.TLS.Extra (fileReadCertificate, fileReadPrivateKey)
import Data.Certificate.X509 (X509, encodeCertificate, decodeCertificate)
import qualified Crypto.Types.PubKey.RSA as RSA (PrivateKey(..))
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow.ArrowList (listA, arr2A)
import Data.CaseInsensitive as CI (mk)

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
    subscriptionId :: String
  , certificate :: X509
  , privateKey :: PrivateKey
  , baseUrl :: String
  }

encodePrivateKey :: PrivateKey -> ByteString
encodePrivateKey (PrivRSA pkey) = runPut $ do
  put (RSA.private_size pkey)
  put (RSA.private_n pkey)
  put (RSA.private_d pkey)
  put (RSA.private_p pkey)
  put (RSA.private_q pkey)
  put (RSA.private_dP pkey)
  put (RSA.private_dQ pkey)
  put (RSA.private_qinv pkey)

decodePrivateKey :: ByteString -> PrivateKey
decodePrivateKey = PrivRSA . runGet getPrivateKey
  where
    getPrivateKey :: Get RSA.PrivateKey
    getPrivateKey =
      RSA.PrivateKey <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Binary AWSSetup where
  put (AWSSetup sid cert pkey url) = do
    put sid
    put (encodeCertificate cert)
    put (encodePrivateKey pkey)
    put url
  get = do
    sid  <- get
    Right cert <- decodeCertificate <$> get
    pkey <- decodePrivateKey <$> get
    url  <- get
    return $ AWSSetup sid cert pkey url

awsSetup :: String        
         -> String       
         -> String      
         -> IO AWSSetup
awsSetup sid certPath pkeyPath = do
  cert <- fileReadCertificate certPath
  pkey <- fileReadPrivateKey pkeyPath
  return AWSSetup {
      subscriptionId = sid
    , certificate    = cert
    , privateKey     = pkey
    , baseUrl        = "https://ec2.amazonaws.com/"
    }


{--
cloudServices :: AWSSetup -> IO [CloudService]
cloudServices setup = awsExecute setup $ \exec -> do
  services <- exec hostedServicesRequest
  forM services $ \service -> do
    roles <- exec AWSRequest {
        relativeUrl = "/services/hostedservices/" ++ hostedServiceName service
                   ++ "?embed-detail=true"
      , apiVersion  = "2012-03-01"
      , parser      = proc xml -> do
          role      <- getXPathTrees "//Role[@type='PersistentVMRole']" -< xml
          name      <- getText . getXPathTrees "/Role/RoleName/text()" -< role
          roleInst  <- arr2A getXPathTrees -< ("//RoleInstance[RoleName='" ++ name ++ "']", xml)
          ip        <- getText . getXPathTrees "/RoleInstance/IpAddress/text()" -< roleInst
          endpoints <- listA (parseEndpoint . getXPathTrees "//InputEndpoint") -< role
          id -< VirtualMachine name ip endpoints
      }
    return $ CloudService (hostedServiceName service) roles
--}
