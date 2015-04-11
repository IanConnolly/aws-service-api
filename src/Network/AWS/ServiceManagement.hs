{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
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
    -- * High-level API
  , cloudServices
  , addVM
  , destroyVM

  ) where

import Data.Maybe
import Data.Either

import Data.Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL

import AWS
import AWS.EC2
import AWS.EC2.Types
import qualified AWS.EC2.Util as Util

import Data.Text hiding (map, filter, concat, head, concatMap)
import Data.Map (Map(..), fromListWith, toList)

data CloudService = CloudService {
    cloudServiceName :: String
  , cloudServiceVMs  :: [VirtualMachine]
  } deriving (Show)

data VirtualMachine = VirtualMachine {
    vmName           :: String
  , vmInstanceId     :: String
  , vmIpAddress      :: String
  , vmInputEndpoints :: [Endpoint]
  } deriving (Show)

data Endpoint = Endpoint {
    endpointName :: String
  , endpointPort :: String
  , endpointVip  :: String
  } deriving (Show)

vmSshEndpoint :: VirtualMachine -> Maybe Endpoint
vmSshEndpoint vm = listToMaybe
  [ ep
  | ep <- vmInputEndpoints vm
  , endpointName ep == "SSH"
  ]

cloudServices :: IO [CloudService]
cloudServices = do
    ins <- getInstances
    let ins' = filter (\x -> instanceState x == InstanceStateRunning) ins
    let services = groupServices ins'
    return $ parseServices services

addVM :: String -> CloudService -> IO CloudService
addVM name cserv = do
    vm <- createVM name (cloudServiceName cserv)
    cred <- loadCredential
    runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        waitForInstanceState InstanceStateRunning (pack $ vmInstanceId vm)
    return $ cserv {
        cloudServiceVMs = cloudServiceVMs cserv ++ [vm]
    }

destroyVM :: String -> CloudService -> IO CloudService
destroyVM name cs = do
    let vm = head $ filter (\v -> vmName v == name) (cloudServiceVMs cs)
    terminateVM vm
    return $ cs {
        cloudServiceVMs = filter (\v -> vmName v /= name) (cloudServiceVMs cs)
    }

getInstances :: IO [Instance]
getInstances = do
    cred <- loadCredential
    reservations <- runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        Util.list $ describeInstances [] []
    return $ concatMap reservationInstanceSet reservations

createVM :: String -> String -> IO VirtualMachine
createVM name service = do
    cred <- loadCredential
    reservations <- runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        let request = defaultRunInstancesRequest "ami-c7a73db0" 1 1
        -- request defaults to t1.micro which doesn't support hvm virtualisation
        let request' = request {
            runInstancesRequestInstanceType = Just "t2.micro"
        }
        runInstances request'
    let ins = head $ reservationInstanceSet reservations
    success <- runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        createTags [instanceId ins] [("Name", pack name)
                                    ,("service", pack service)]
    reservations' <- runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        Util.list $ describeInstances [instanceId ins] []
    let ins' = head $ concatMap reservationInstanceSet reservations'
    return $ parseInstance ins'

terminateVM :: VirtualMachine -> IO ()
terminateVM vm = do
    cred <- loadCredential
    let id = [pack $ vmInstanceId vm]
    runResourceT $ runEC2 cred $ do
        setRegion "eu-west-1"
        Util.list $ terminateInstances id
    return ()

getTagValue :: String -> Instance -> String
getTagValue key ins = head [unpack . fromJust $ resourceTagValue resTag
                           | resTag <- instanceTagSet ins
                           , unpack (resourceTagKey resTag) == key]

groupServices :: [Instance] -> Map String [VirtualMachine]
groupServices instances = fromListWith (++) serviceTuplesWithLists
    where serviceTuplesWithLists = map makeLists serviceTuples
          makeLists (service, ins) = (service, [ins])
          serviceTuples = map makeServiceTuple instances
          makeServiceTuple ins = (getTagValue "service" ins, parseInstance ins)

parseInstance :: Instance -> VirtualMachine
parseInstance ins = VirtualMachine {
    vmName = getTagValue "Name" ins
  , vmInstanceId = unpack $ instanceId ins
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

-- AWS.EC2Tests.Util
waitForInstanceState :: (MonadBaseControl IO m, MonadResource m) =>
                     InstanceState -> Text -> EC2 m Reservation
waitForInstanceState s = Util.wait p desc
  where
    p r = (instanceState . head . reservationInstanceSet) r == s
    desc inst = Util.list $ describeInstances [inst] []
