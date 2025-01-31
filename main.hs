-- Import necessary modules for socket programming, byte string handling, and binary data processing
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Strict.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (writeFile)
import Data.Word (Word32)
import Numeric (readHex)
import Control.Monad (forM, msum)

import Data.Bits
import qualified Data.ByteString.Char8 as BChar8
import Control.Applicative
import Data.Maybe

-- Main entry point of the program
main :: IO ()
main = do 
    putStrLn "Starting DHCP Server..."
    startListening "11337"

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
        
        let dhcpPacket = readDhcpPacket msg
        case dhcpPacket of
            Just packet -> do
                putStrLn "DHCP packet parsed successfully"
                handleDhcpPacket packet sock addr
            Nothing -> putStrLn "Failed to parse DHCP packet"
        
        procMessages sock

-- Function to handle DHCP packets (processing DISCOVER, OFFER, REQUEST, ACK)
handleDhcpPacket :: DhcpV4Packet -> Socket -> SockAddr -> IO ()
handleDhcpPacket packet sock addr = do
    putStrLn $ "Processing packet from " ++ show addr
    case op packet of
        BOOTREQUEST -> do
            putStrLn "Received DHCPDISCOVER, sending DHCPOFFER"
            sendDhcpOffer packet sock addr
        BOOTREPLY -> do
            putStrLn "Received DHCPREQUEST, sending DHCPACK"
            sendDhcpAck packet sock addr
        _ -> putStrLn "Received unknown DHCP packet"

-- Function to convert a hexadecimal string to Word32
hexToWord32 :: String -> Word32
hexToWord32 hexStr = fst . head $ readHex hexStr

-- Function to check and create files, returning the first Word32 found
checkAndCreateFiles :: [String] -> IO Word32
checkAndCreateFiles [] = return 0xc0a82b02
checkAndCreateFiles (ip:ips) = do
    let filePath = "ips/" ++ ip ++ ".txt"
    exists <- doesFileExist filePath
    if exists 
        then checkAndCreateFiles ips 
        else do
            putStrLn $ show (hexToWord32 ip)
            let hexValue = hexToWord32 ip
            writeFile filePath (show hexValue)
            return hexValue

getyiaddr ::  IO Word32
getyiaddr = checkAndCreateFiles ["c0a82b02", "c0a82b03", "c0a82b04", "c0a82b05", 
                  "c0a82b06", "c0a82b07", "c0a82b08", "c0a82b09", 
                  "c0a82b0a", "c0a82b0b", "c0a82b0c", "c0a82b0d", 
                  "c0a82b0e", "c0a82b0f", "c0a82b10", "c0a82b11", 
                  "c0a82b12", "c0a82b13"]

-- Function to send a DHCPOFFER packet
sendDhcpOffer :: DhcpV4Packet -> Socket -> SockAddr -> IO ()
sendDhcpOffer packet sock addr = do
    putStrLn "Sending DHCPOFFER..."
    available_address <- getyiaddr
    let offerPacket = packet { 
            op = BOOTREPLY,
            htype = HTYPE_ETHER,
            hlen = 6,
            hops = 0,
            xid = 0,
            secs = 1,
            flags = [FLAG_BROADCAST],
            ciaddr = 0x00000000, 
            yiaddr = available_address,
            siaddr = 0xc0a82b01, 
            giaddr = 0xC0A80001, 
            chaddr = replicate 16 0x00, 
            sname = "blob                                                            ", 
            file = "doko                                                                                                                            ",
            options = [OPT_ROUTER [0xC0A80001], OPT_ROUTER [0xC0A80001]]
    }
    sendTo sock (serializeDhcpPacket offerPacket) addr
    putStrLn "DHCPOFFER sent"

-- Function to send a DHCPACK packet
sendDhcpAck :: DhcpV4Packet -> Socket -> SockAddr -> IO ()
sendDhcpAck packet sock addr = do
    putStrLn "Sending DHCPACK..."
    let ackPacket = packet {
            op = BOOTREPLY,  
            yiaddr = 0xC0A80002,  
            siaddr = 0xC0A80001,  
            options = [OPT_SUBNET_MASK 0xFFFFFF00, OPT_ROUTER [0xC0A80001]]
    }
    sendTo sock (serializeDhcpPacket ackPacket) addr
    putStrLn "DHCPACK sent"

-- Function to serialize a DHCP packet to ByteString for transmission
serializeDhcpPacket :: DhcpV4Packet -> B.ByteString
serializeDhcpPacket packet = B.concat . BL.toChunks $ runPut (putDhcpPacket packet)

instance Enum DhcpV4_op where
    toEnum 1 = BOOTREQUEST
    toEnum 2 = BOOTREPLY
    toEnum _ = error "Unknown DhcpV4_op"

    fromEnum BOOTREQUEST = 1
    fromEnum BOOTREPLY = 2

instance Enum DhcpV4_htype where
    toEnum 1 = HTYPE_ETHER
    toEnum 6 = HTYPE_IEEE802
    toEnum 8 = HTYPE_FDDI
    toEnum _ = error "Unknown DhcpV4_htype"

    fromEnum HTYPE_ETHER = 1
    fromEnum HTYPE_IEEE802 = 6
    fromEnum HTYPE_FDDI = 8


-- Function to serialize a DHCP packet (converts the DhcpV4Packet to bytes)
putDhcpPacket :: DhcpV4Packet -> Put
putDhcpPacket packet = do
    putWord8 (fromIntegral (fromEnum (op packet)))  -- Operation code (1 byte)
    putWord8 (fromIntegral (fromEnum (htype packet)))  -- Hardware type (1 byte)
    putWord8 (hlen packet)  -- Hardware address length (1 byte)
    putWord8 (hops packet)  -- Hops count (1 byte)
    putWord32be (xid packet)  -- Transaction ID (4 bytes)
    putWord16be (secs packet)  -- Seconds (2 bytes)
    putWord16be (toFlagsWord16 (flags packet)) -- Flags (2 bytes)
    putWord32be (ciaddr packet)  -- Client IP address (4 bytes)
    putWord32be (yiaddr packet)  -- Your IP address (4 bytes)
    putWord32be (giaddr packet)  -- Gateway IP address (4 bytes)
    putWord32be (siaddr packet)  -- Server IP address (4 bytes)
    putByteString (B.pack (chaddr packet))  -- Client hardware address (variable length)
    putByteString (BChar8.pack (sname packet))  -- Server name (64 bytes)
    putByteString (BChar8.pack (file packet))  -- Boot file name (128 bytes)
    putWord32be 0x63825363  -- Magic cookie
    putDhcpOptions (options packet)  -- DHCP options

toFlagsWord16 :: [DhcpV4_flag] -> Word16
-- toFlagsWord16 flags = if FLAG_BROADCAST `elem` flags then 0x8000 else 0
toFlagsWord16 flags = 0x0000

-- Function to serialize DHCP options (converts a list of options to bytes)
putDhcpOptions :: [DhcpV4_option] -> Put
putDhcpOptions [] = return ()  -- No more options
putDhcpOptions (opt:opts) = do
    case opt of
        OPT_SUBNET_MASK mask -> do
            putWord8 1  -- Option code
            putWord8 4  -- Option length (4 bytes for a mask)
            putWord32be mask
        OPT_ROUTER routers -> do
            putWord8 3  -- Option code
            putWord8 (fromIntegral (4 * length routers)) -- Length based on number of routers
            mapM_ putWord32be routers
        -- Add other options similarly...

    putDhcpOptions opts  -- Continue with the rest of the options


-- Function to read and deserialize a raw DHCP packet from a ByteString
readDhcpPacket :: B.ByteString -> Maybe DhcpV4Packet
readDhcpPacket rawData = readDhcpPacket' (runGet deserializePacket rawData)
    where 
        -- Unpack the result of deserialization; return Nothing on failure
        readDhcpPacket' (Right a, _) = a  
        readDhcpPacket' _ = Nothing       

-- Function to deserialize a DHCP packet from binary data using the Get monad
deserializePacket :: Get (Maybe DhcpV4Packet)
deserializePacket = do
    -- Read individual fields from the packet according to the DHCP protocol specification
    opCode <- deserializeOpCode <$> getWord8       -- Operation code (1 byte)
    htype <- deserializeHtype <$> getWord8          -- Hardware type (1 byte)
    hlen <- getWord8                                  -- Hardware address length (1 byte)
    hops <- getWord8                                  -- Hops count (1 byte)
    xid <- getWord32be                                -- Transaction ID (4 bytes)
    secs <- getWord16be                              -- Seconds elapsed since client started DHCP process (2 bytes)
    flags <- deserializeFlags <$> getWord16be       -- Flags field (2 bytes)
    
    ciaddr <- getWord32be                            -- Client IP address (4 bytes)
    yiaddr <- getWord32be                            -- Your IP address (4 bytes)
    giaddr <- getWord32be                            -- Gateway IP address (4 bytes)
    siaddr <- getWord32be                            -- Server IP address (4 bytes)

    chaddr <- B.unpack . B.take (fromIntegral hlen) <$> getByteString 16  -- Client hardware address (16 bytes)
    
    sname <- toString <$> getByteString 64          -- Server name (64 bytes)
    file <- toString <$> getByteString 128           -- Boot file name (128 bytes)

    magic <- getWord32be                              -- Magic cookie (4 bytes)

    options <- deserializeOptions                     -- Deserialize DHCP options
    
    return $ deserializePacket' opCode htype hlen hops xid secs flags ciaddr yiaddr giaddr siaddr chaddr sname file magic options

    where 
        -- Construct a DhcpV4Packet if all required fields are valid; otherwise return Nothing
        deserializePacket' (Just op') 
                           (Just htype') 
                           hlen' 
                           hops' 
                           xid' 
                           secs' 
                           flags' 
                           ciaddr' 
                           yiaddr' 
                           giaddr' 
                           siaddr' 
                           chaddr' 
                           sname' 
                           file' 
                           0x63825363  -- Check magic cookie value
                           options' = Just DhcpV4Packet { 
                op = op', 
                htype = htype',
                hlen = hlen',
                hops = hops',
                xid = xid',
                secs = secs',
                flags = flags',
                ciaddr = ciaddr', 
                yiaddr = yiaddr',
                giaddr = giaddr',
                siaddr = siaddr',
                chaddr = chaddr',
                sname = sname',
                file = file',
                options = options' }
        
        deserializePacket' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = Nothing 

-- Define data types for various DHCP options and their structures
data DhcpV4_option = OPT_SUBNET_MASK Word32 
                   | OPT_TIME_OFFSET Word32
                   | OPT_ROUTER [Word32]
                   | OPT_TIME_SERVER [Word32]
                   | OPT_NAME_SERVER [Word32]
                   | OPT_DOMAIN_NAME_SERVER [Word32]
                   | OPT_LOG_SERVER [Word32]
                   | OPT_UNKNOWN Word8 [Word8] deriving Show 

-- Function to deserialize DHCP options from binary data
deserializeOptions :: Get([DhcpV4_option])
deserializeOptions = do 
    optionCode <- getWord8  -- Read option code
    
    case optionCode of  
        0 -> deserializeOptions   -- Option code 0 indicates continuation; recurse again for more options
        255 -> return []          -- Option code 255 indicates end of options; return empty list
        
        _ -> do 
            optionLength <- getWord8   -- Read length of option data
            optionData <- B.unpack <$> getByteString (fromIntegral optionLength)  -- Read option data
            
            let option = deserializeOption optionCode optionData  -- Deserialize specific option based on its code
            
            options <- deserializeOptions   -- Recursively read more options
            
            return $ option : options         -- Return list of options

-- Function to convert raw option data into specific DhcpV4_option types based on their codes
-- looks like a fork bomb
deserializeOption :: Word8 -> [Word8] -> DhcpV4_option
deserializeOption 1 x@(_:_:_:_:[]) = OPT_SUBNET_MASK (toWord32 x)   -- Subnet mask option
deserializeOption 2 x@(_:_:_:_:[]) = OPT_TIME_OFFSET (toWord32 x)   -- Time offset option

-- Handle options with multiple Word32 values based on their codes (3-7)
deserializeOption optionCode optionData
    | optionCode >= 3 && optionCode <= 7 && isJust ctor = deserializeWord32Option optionCode optionData (fromJust ctor)
    where ctor = find4OctetsConstructor optionCode

-- Handle unknown or unsupported options by returning them as OPT_UNKNOWN type
deserializeOption optionCode optionData = OPT_UNKNOWN optionCode optionData

-- Helper function to map specific codes to constructors for multi-value options
find4OctetsConstructor :: Word8 -> Maybe ([Word32] -> DhcpV4_option)
find4OctetsConstructor 3 = Just OPT_ROUTER              -- Router option constructor
find4OctetsConstructor 4 = Just OPT_TIME_SERVER         -- Time server option constructor
find4OctetsConstructor 5 = Just OPT_NAME_SERVER         -- Name server option constructor
find4OctetsConstructor 6 = Just OPT_DOMAIN_NAME_SERVER   -- Domain name server constructor
find4OctetsConstructor 7 = Just OPT_LOG_SERVER          -- Log server constructor
find4OctetsConstructor _ = Nothing                       -- No constructor found

-- Function to handle deserialization of multi-value Word32 options based on their codes and content length checks.
deserializeWord32Option :: Word8 -> [Word8] -> ([Word32] -> DhcpV4_option) -> DhcpV4_option
deserializeWord32Option code content makeOption =
    if length content `mod` 4 == 0 then makeOption (toWords32 content) else OPT_UNKNOWN code content 

-- Function to interpret flag bits from the flags field in the DHCP packet.
deserializeFlags :: Word16 -> [DhcpV4_flag]
deserializeFlags f =
    if (f .&. 0x8000) /= 0 then [FLAG_BROADCAST] else []   -- Check if broadcast flag is set

-- Helper function to convert a list of bytes into multiple Word32 values.
toWords32 :: [Word8] -> [Word32]
toWords32 [] = []
toWords32 x =
    let word32Value = toWord32 $ take 4 x in      -- Take first four bytes and convert to Word32 value.
    word32Value : toWords32 (drop 4 x)             -- Recur with remaining bytes.

-- Convert a list of four Word8 values into a single Word32 value.
toWord32 :: [Word8] -> Word32
toWord32 = foldr1 (\b acc -> ((acc `shiftL` 8) .|. b)) . map fromIntegral

-- Convert ByteString into String by splitting at null terminators.
toString :: B.ByteString -> String
toString = BChar8.unpack . head . B.split 0

-- Define operation types for DHCP packets.
data DhcpV4_op = BOOTREQUEST | BOOTREPLY deriving Show

-- Deserialize operation codes from byte values into meaningful types.
deserializeOpCode :: Word8 -> Maybe DhcpV4_op
deserializeOpCode 1 = Just BOOTREQUEST   -- Code for BOOTREQUEST operation.
deserializeOpCode 2 = Just BOOTREPLY     -- Code for BOOTREPLY operation.
deserializeOpCode _ = Nothing             -- Return Nothing for unknown codes.

-- Define hardware types used in DHCP packets.
data DhcpV4_htype = HTYPE_ETHER | HTYPE_IEEE802 | HTYPE_FDDI deriving Show

-- Deserialize hardware types from byte values into meaningful types.
deserializeHtype :: Word8 -> Maybe DhcpV4_htype
deserializeHtype 1 = Just HTYPE_ETHER      -- Ethernet hardware type.
deserializeHtype 6 = Just HTYPE_IEEE802     -- IEEE802 hardware type.
deserializeHtype 8 = Just HTYPE_FDDI        -- FDDI hardware type.
deserializeHtype _ = Nothing                 -- Return Nothing for unknown types.

-- Define flag types used in DHCP packets.
data DhcpV4_flag = FLAG_BROADCAST deriving Show

-- Define the main structure representing a DHCPv4 packet with all its fields.
data DhcpV4Packet = DhcpV4Packet { op :: DhcpV4_op       -- Operation type: request or reply.
                                   , htype :: DhcpV4_htype   -- Hardware type: Ethernet, IEEE802, etc.
                                   , hlen :: Word8           -- Length of hardware address.
                                   , hops :: Word8           -- Number of hops taken by the packet.
                                   , xid :: Word32           -- Transaction ID for matching requests/replies.
                                   , secs :: Word16          -- Seconds elapsed since client started process.
                                   , flags :: [DhcpV4_flag]   -- Flags indicating special conditions like broadcast.
                                   , ciaddr :: Word32        -- Client IP address if already assigned.
                                   , yiaddr :: Word32        -- Your IP address assigned by server.
                                   , siaddr :: Word32        -- Server IP address for client use.
                                   , giaddr :: Word32        -- Gateway IP address used by relay agents.
                                   , chaddr :: [Word8]       -- Client hardware address as byte array.
                                   , sname :: String         -- Optional server name string.
                                   , file :: String          -- Optional boot file name string.
                                   , options :: [DhcpV4_option] } deriving Show   -- List of DHCP options.

