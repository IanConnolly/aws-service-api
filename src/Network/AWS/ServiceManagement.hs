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
  , awsSetup
  , AWSSetup
    -- * High-level API
  , cloudServices
  , addVM
  , destroyVM
  , createService
  , scaleUpService
  , scaleDownService
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

import Data.Text hiding (map, filter, concat, head, concatMap, length)
import Data.Map (Map(..), fromListWith, toList)

import System.Random
import Data.UUID

import Network.AWS.Config as Conf

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

awsSetup = loadSetupFromFile

cloudServices :: AWSSetup -> IO [CloudService]
cloudServices conf = do
    ins <- getInstances conf
    let ins' = filter (\x -> instanceState x == InstanceStateRunning) ins
    let services = groupServices ins'
    return $ parseServices services

addVM :: AWSSetup -> String -> CloudService -> IO CloudService
addVM conf name cserv = do
    vm <- createVM conf name (cloudServiceName cserv)
    let cred = newCredential (accessKey conf) (secretAccessKey conf)
    runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
        waitForInstanceState InstanceStateRunning (pack $ vmInstanceId vm)
    return $ cserv {
        cloudServiceVMs = cloudServiceVMs cserv ++ [vm]
    }

destroyVM :: AWSSetup -> String -> CloudService -> IO CloudService
destroyVM conf name cs = do
    let vm = head $ filter (\v -> vmName v == name) (cloudServiceVMs cs)
    terminateVM conf vm
    return $ cs {
        cloudServiceVMs = filter (\v -> vmName v /= name) (cloudServiceVMs cs)
    }

scaleUpService :: AWSSetup -> CloudService -> Int -> IO CloudService
scaleUpService conf cserv 0 = return cserv
scaleUpService conf cserv n = do
    name <- newInstanceName $ cloudServiceName cserv
    cserv' <- addVM conf name cserv
    scaleUpService conf cserv' (n - 1)

scaleDownService :: AWSSetup -> CloudService -> Int -> IO CloudService
scaleDownService conf cserv 0 = return cserv
scaleDownService conf cserv n = do
    vm <- randomVM cserv
    cserv' <- destroyVM conf (vmName vm) cserv
    scaleDownService conf cserv' (n - 1)

createService :: AWSSetup -> String -> Int -> IO CloudService
createService conf service num = do
    name <- newInstanceName service
    vm <- createVM conf name service
    let cred = newCredential (accessKey conf) (secretAccessKey conf)
    runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
        waitForInstanceState InstanceStateRunning (pack $ vmInstanceId vm)
    let cserv = CloudService {
        cloudServiceName = service
      , cloudServiceVMs = [vm]
    }
    scaleUpService conf cserv (num - 1)

randomVM :: CloudService -> IO VirtualMachine
randomVM cserv = do
    g <- newStdGen
    let vms = cloudServiceVMs cserv
    let (index, _) = randomR (0, length vms - 1) g
    return $ vms !! index

newInstanceName :: String -> IO String
newInstanceName cserv = do
    g <- newStdGen
    let (uuid, _) = random g
    return $ cserv ++ "-" ++ toString uuid

getInstances :: AWSSetup -> IO [Instance]
getInstances conf = do
    let cred = newCredential (accessKey conf) (secretAccessKey conf)
    reservations <- runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
        Util.list $ describeInstances [] []
    return $ concatMap reservationInstanceSet reservations

createVM :: AWSSetup -> String -> String -> IO VirtualMachine
createVM conf name service = do
    let cred = newCredential (accessKey conf) (secretAccessKey conf)
    reservations <- runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
        let request = defaultRunInstancesRequest (pack $ image conf) 1 1
        let request' = request {
            runInstancesRequestInstanceType =
                Just . pack . Conf.instanceType $ conf
        }
        runInstances request'
    let ins = head $ reservationInstanceSet reservations
    success <- runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
        createTags [instanceId ins] [("Name", pack name)
                                    ,("service", pack service)]
    reservations' <- runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
        Util.list $ describeInstances [instanceId ins] []
    let ins' = head $ concatMap reservationInstanceSet reservations'
    return $ parseInstance ins'

terminateVM :: AWSSetup -> VirtualMachine -> IO ()
terminateVM conf vm = do
    let cred = newCredential (accessKey conf) (secretAccessKey conf)
    let id = [pack $ vmInstanceId vm]
    runResourceT $ runEC2 cred $ do
        setRegion $ pack $ region conf
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

