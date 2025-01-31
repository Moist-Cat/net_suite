-- Import necessary modules for socket programming, byte string handling, and binary data processing
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary
import qualified Data.ByteString.Lazy as BL

import Data.Bits
import qualified Data.ByteString.Char8 as BChar8
import Control.Applicative
import Control.Monad (replicateM)
import Data.Maybe

import System.IO (readFile)
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Data.Char (isSpace)

-- Main entry point of the program
main :: IO ()
main = do 
    putStrLn "Starting DNS Server..."
    startListening "11338"

-- Function to set up a UDP socket and listen for incoming messages
startListening :: String -> IO ()
startListening port = withSocketsDo $ do
    putStrLn $ "Starting to listen on port " ++ port
    addrinfos <- getAddrInfo 
                 (Just defaultHints {addrFlags = [AI_PASSIVE]})  -- Passive mode for server socket
                 Nothing (Just port)
    
    let serveraddr = head (filter (\x -> (addrFamily x) == AF_INET) addrinfos)
    
    listenSocket <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind listenSocket (addrAddress serveraddr)
    putStrLn "Socket bound, waiting for messages..."

    procMessages listenSocket

  where 
    procMessages sock = do
        putStrLn "Receiving message..."
        (msg, addr) <- recvFrom sock 1024
        putStrLn "Message received"
        
        let packet = readDnsPacket msg
        handleDnsPacket packet sock addr
        procMessages sock

-- Function to handle DHCP packets (processing DISCOVER, OFFER, REQUEST, ACK)
handleDnsPacket :: DnsPacket -> Socket -> SockAddr -> IO ()
handleDnsPacket packet sock addr = do
    putStrLn $ "Processing packet from " ++ show addr
    sendDnsOffer packet sock addr

-- Convert a lazy ByteString to strict ByteString
serializeDnsPacket :: DnsPacket -> B.ByteString
serializeDnsPacket packet = B.concat (BL.toChunks (encode packet))

sendDnsOffer :: DnsPacket -> Socket -> SockAddr -> IO ()
sendDnsOffer packet sock addr = do
    putStrLn "Sending DNS response..."

    -- Step 1: Get the requested domain name from the packet
    let requestedDomain = (name packet) ++ "." ++ (domain packet)

    putStrLn (show packet)

    -- Step 2: Construct the file path
    let filePath = "dn/" ++ requestedDomain

    -- Step 3: Check if the file exists and read the IP address
    ipAddress <- ifM (doesFileExist filePath) 
                    (readFile filePath >>= return . trim) 
                    (return "127.0.0.1")

    -- Step 4: Prepare the response packet
    let responsePacket = DnsPacket {
            tx_id        = tx_id packet,       -- Keeping the same tx_id from the request
            flags        = 0x8180,             -- Standard query response, no error
            questions    = 0,                  -- No more questions
            answers      = 1,                  -- Number of answers in the response
            authority    = 0,                  -- No authority section
            additional   = 0,                  -- No additional section
            name_len     = fromIntegral (length ipAddress), -- Length of the IP string
            name         = ipAddress,           -- IP address read from file or default
            domain_len   = fromIntegral (length requestedDomain), -- Length of the domain string
            domain       = requestedDomain,     -- Domain from the original packet
            end          = 0,                   -- End byte for DNS string (could be adjusted)
            query_type   = 1,                   -- Type A (host address) for a DNS response
            query_class  = 1                    -- Class IN (Internet)
    }

    -- Send the DNS response packet using the 'sendTo' function
    sendTo sock (serializeDnsPacket responsePacket) addr
    putStrLn "DNS response sent"

-- Helper function to trim whitespace from IP address read from file
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Utility function to handle conditional actions
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond trueAction falseAction = do
    result <- cond
    if result then trueAction else falseAction

-- Function to read and deserialize a raw DHCP packet from a ByteString
readDnsPacket :: B.ByteString -> DnsPacket
readDnsPacket rawData = runGet getDnsPacket (BL.fromStrict rawData)
  where
    -- Define the function that uses `get` to decode a `DnsPacket`
    getDnsPacket :: Get DnsPacket
    getDnsPacket = get  -- or a custom decoding logic for DnsPacket

-- Convert ByteString into String by splitting at null terminators.
toString :: B.ByteString -> String
toString = BChar8.unpack . head . B.split 0

instance Binary DnsPacket where
    put packet = do
        putWord16le (tx_id packet)         -- 2 bytes
        putWord16le (flags packet)         -- 2 bytes
        putWord16le   (questions packet)      -- 2 bytes
        putWord16le (answers packet)       -- 2 bytes
        putWord16le (authority packet)     -- 2 bytes
        putWord16le (additional packet)    -- 2 bytes
        putWord8   (name_len packet)       -- 1 byte
        putString (name packet)            -- variable length
        putWord8   (domain_len packet)     -- 1 byte
        putString (domain packet)          -- variable length
        putWord8   (end packet)            -- 1 byte
        putWord16le (query_type packet)    -- 2 bytes
        putWord16le (query_class packet)   -- 2 bytes
    get = do
        tx_id'        <- getWord16le      -- 2 bytes
        flags'        <- getWord16le      -- 2 bytes
        questions'    <- getWord16le         -- 2 byte
        answers'      <- getWord16le      -- 2 bytes
        authority'    <- getWord16le      -- 2 bytes
        additional'   <- getWord16le      -- 2 bytes
        name_len'     <- getWord8         -- 1 byte (length of the name)
        name'         <- getStringOfLen name_len'  -- variable length based on name_len
        domain_len'   <- getWord8         -- 1 byte (length of the domain)
        domain'       <- getStringOfLen domain_len' -- variable length based on domain_len
        end'          <- getWord8         -- 1 byte
        query_type'   <- getWord16le      -- 2 bytes
        query_class'  <- getWord16le      -- 2 bytes
        return $ DnsPacket tx_id' flags' questions' answers' authority' additional' name_len' name' domain_len' domain' end' query_type' query_class'

-- Helper function to put a string
putString :: String -> Put
putString s = do
    putWord8 (fromIntegral (length s)) -- length of the string (1 byte)
    mapM_ putWord8 (map (fromIntegral . fromEnum) s)  -- write each character as a byte

-- Helper function to get a string of a specific length
getStringOfLen :: Word8 -> Get String
getStringOfLen len = do
    bytes <- replicateM (fromIntegral len) getWord8
    return (map (toEnum . fromIntegral) bytes)

-- Define the main structure representing a DHCPv4 packet with all its fields.
data DnsPacket = DnsPacket 
    { tx_id        :: Word16
    , flags        :: Word16
    , questions    :: Word16
    , answers      :: Word16
    , authority    :: Word16
    , additional   :: Word16
    , name_len     :: Word8
    , name         :: String
    , domain_len   :: Word8
    , domain       :: String
    , end          :: Word8
    , query_type   :: Word16
    , query_class  :: Word16
    } deriving Show
