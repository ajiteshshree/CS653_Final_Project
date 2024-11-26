{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.STM 
    ( TVar, atomically, newTVar, newTVarIO, readTVar, readTVarIO, modifyTVar', writeTVar )
import Control.Exception (finally)
import Control.Monad (forM_, forever, when)
import Data.Aeson
    ( FromJSON, ToJSON, decode, encode, object, parseJSON, withObject, (.:) )--(.:?)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.Wai
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types (status400)
import Network.WebSockets hiding (Request)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

-- Define message types
data ClientMessage = JoinRoom { room :: Text, clientUser :: Text }
                   | LeaveRoom { room :: Text, clientUser :: Text }
                   | ChatMessage { room :: Text, clientUser :: Text, clientMessage :: Text }
                   deriving (Show, Generic)

instance FromJSON ClientMessage where
    parseJSON = withObject "ClientMessage" $ \v -> do
        msgType <- v .: "type" :: Parser Text
        case msgType of
            "join" -> JoinRoom <$> v .: "room" <*> v .: "user"
            "leave" -> LeaveRoom <$> v .: "room" <*> v .: "user"
            "message" -> ChatMessage <$> v .: "room" <*> v .: "user" <*> v .: "message"
            _ -> fail "Unknown message type"

data ServerMessage = ServerMessage
    { serverUser :: Text
    , serverMessage :: Text
    } deriving (Show, Generic)

instance ToJSON ServerMessage

type Client = Connection
type ClientId = Text
type RoomName = Text

data ServerState = ServerState
    { rooms :: TVar (Map.Map RoomName (TVar (Map.Map ClientId Client)))
    }

initServerState :: IO ServerState
initServerState = do
    roomsVar <- newTVarIO Map.empty
    return ServerState { rooms = roomsVar }

main :: IO ()
main = do
    putStrLn "Starting Haskell Chat Server on port 8080..."
    state <- initServerState
    run 8080 (app state)

app :: ServerState -> Application
app state pendingReq respond =
    if isWebSocketRequest pendingReq
        then websocketsOr defaultConnectionOptions (wsApp state) serveStaticFiles pendingReq respond
        else serveStaticFiles pendingReq respond
  where
    -- Serve static files from the "static" directory
    serveStaticFiles :: Application
    serveStaticFiles = staticApp (defaultFileServerSettings "static")

backupApp :: Application
backupApp _ respond = respond $ responseLBS
    status400
    [("Content-Type", "text/plain")]
    "Not a WebSocket request"

isWebSocketRequest :: Request -> Bool
isWebSocketRequest req = 
    case requestHeaderHost req of
        Just _ -> True
        Nothing -> False

wsApp :: ServerState -> ServerApp
wsApp state pendingConn = do
    conn <- acceptRequest pendingConn
    -- Handle the connection
    talk conn state

-- Function to handle communication with a client
talk :: Connection -> ServerState -> IO ()
talk conn state = flip finally disconnect $ do
    sendTextData conn ("Welcome to the Haskell Chat Server!" :: Text)
    forever $ do
        msgData <- receiveData conn
        case decode msgData :: Maybe ClientMessage of
            Just (JoinRoom roomName userName) -> joinRoom conn state roomName userName
            Just (LeaveRoom roomName userName) -> leaveRoom conn state roomName userName
            Just (ChatMessage roomName userName message) -> sendMessage conn state roomName userName message
            Nothing -> sendTextData conn ("Invalid message format" :: Text)
  where
    disconnect = putStrLn "A client disconnected."

-- Join a room
joinRoom :: Connection -> ServerState -> RoomName -> Text -> IO ()
joinRoom conn ServerState{..} roomName userName = atomically $ do
    roomsMap <- readTVar rooms
    roomVar <- case Map.lookup roomName roomsMap of
        Just rv -> return rv
        Nothing -> do
            rv <- newTVar Map.empty
            modifyTVar' rooms (Map.insert roomName rv)
            return rv
    clientsMap <- readTVar roomVar
    let clientId = userName -- Ensure uniqueness in practice
    writeTVar roomVar (Map.insert clientId conn clientsMap)

-- Leave a room
leaveRoom :: Connection -> ServerState -> RoomName -> Text -> IO ()
leaveRoom conn ServerState{..} roomName userName = atomically $ do
    roomsMap <- readTVar rooms
    case Map.lookup roomName roomsMap of
        Just roomVar -> do
            clientsMap <- readTVar roomVar
            let updatedClients = Map.delete userName clientsMap
            writeTVar roomVar updatedClients
            -- Optionally, remove the room if empty
            when (Map.null updatedClients) $
                modifyTVar' rooms (Map.delete roomName)
        Nothing -> return ()

-- Send a message to a room
sendMessage :: Connection -> ServerState -> RoomName -> Text -> Text -> IO ()
sendMessage conn ServerState{..} roomName userName message = do
    currentRooms <- readTVarIO rooms
    case Map.lookup roomName currentRooms of
        Just roomVar -> do
            clientsMap <- readTVarIO roomVar
            let broadcastMsg = encode $ ServerMessage userName message
            putStrLn $ "Broadcasting message: " ++ T.unpack userName ++ ": " ++ T.unpack message -- Debug log
            forM_ (Map.elems clientsMap) $ \clientConn -> do
                sendTextData clientConn (BL.toStrict broadcastMsg)
        Nothing -> sendTextData conn ("Room does not exist" :: Text)
