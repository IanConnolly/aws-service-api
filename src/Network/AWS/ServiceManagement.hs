{-# LANGUAGE OverloadedStrings #-}
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
  , cloudServices
--  , createService
--  , addVM
--  , shutdownVM

  ) where

import Data.Maybe
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary (Binary(get, put))
import Data.Binary.Put (runPut)
import Data.Binary.Get (Get, runGet)
import Data.Either
import Data.PEM (PEM(..), pemParseBS)
import Data.Certificate.X509
import Crypto.Types.PubKey.RSA
import qualified Data.Certificate.KeyRSA as KeyRSA
import Network.TLS

import Data.Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)

import AWS
import AWS.EC2
import AWS.EC2.Types
import qualified AWS.EC2.Util as Util

import Data.Text hiding (map, filter, concat, head)
import Data.Map (Map(..), fromListWith, toList)

data HostedService = HostedService {
    hostedServiceName :: String
  }

data CloudService = CloudService {
    cloudServiceName :: String
  , cloudServiceVMs  :: [VirtualMachine]
  } deriving (Show)

data VirtualMachine = VirtualMachine {
    vmName           :: String
  , vmIpAddress      :: String
  , vmInputEndpoints :: [Endpoint]
  } deriving (Show)

data Endpoint = Endpoint {
    endpointName :: String
  , endpointPort :: String
  , endpointVip  :: String
  } deriving (Show)

data AWSSetup = AWSSetup {
    awsConf :: B.ByteString
}

instance Binary AWSSetup where
    put (AWSSetup awsconf) = put awsconf
    get = do
       conf <- get
       return $ AWSSetup conf

awsSetup :: FilePath -> IO AWSSetup
awsSetup awsConfPath = do
    contents <- B.readFile awsConfPath
    return AWSSetup {
        awsConf = contents
    }

-- | Find the endpoint with name @SSH@.
vmSshEndpoint :: VirtualMachine -> Maybe Endpoint
vmSshEndpoint vm = listToMaybe
  [ ep
  | ep <- vmInputEndpoints vm
  , endpointName ep == "SSH"
  ]


cloudServices :: IO [CloudService]
cloudServices = do
    ins <- getInstances
    let services = groupServices ins
    return $ parseServices services

getInstances :: IO [Instance]
getInstances = do
    cred <- loadCredential
    reservations <- runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        Util.list $ describeInstances [] []
    return $ concat $ map reservationInstanceSet reservations

getTagValue :: String -> Instance -> String
getTagValue key ins = head $ [unpack . fromJust $ resourceTagValue resTag | resTag <- instanceTagSet ins, unpack (resourceTagKey resTag) == key]

groupServices :: [Instance] -> Map String [VirtualMachine]
groupServices instances = fromListWith (++) serviceTuplesWithLists
    where serviceTuplesWithLists = map makeLists serviceTuples
          makeLists (service, ins) = (service, [ins])
          serviceTuples = map makeServiceTuple instances
          makeServiceTuple ins = (getTagValue "service" ins, parseInstance ins)

parseInstance :: Instance -> VirtualMachine
parseInstance ins = VirtualMachine {
    vmName = getTagValue "Name" ins
  , vmIpAddress = unpack $ fromJust $ instanceDnsName ins
  , vmInputEndpoints = [Endpoint {
        endpointName = "SSH"
      , endpointVip = unpack $ fromJust $ instanceDnsName ins
      , endpointPort = "22"
  }]
 }

parseServices :: Map String [VirtualMachine] -> [CloudService]
parseServices = map parseTuple . toList
    where parseTuple (name, vms) = CloudService {
            cloudServiceName = name
          , cloudServiceVMs = vms
 }

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
-- createAWSVM name = do

