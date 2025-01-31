import logging
import socket
import time
import struct

from dhcppython.client import DHCPClient
from dhcppython.packet import DHCPPacket

logging.basicConfig(level=logging.DEBUG)

class DataStream(bytearray):

    def append(self, v, fmt='>B'):
        self.extend(struct.pack(fmt, v))

class DNSPackage:

    # head
    tx_id = 0x0000
    flags = 0x0100
    questions = 0x0001
    answers = 0x0000
    authority = 0x0000
    additional = 0x0000

    #name = b"kyun"
    name = b"fake"
    #domain = b"host"
    domain = b"site"

    end = 0x00

    query_type = 0x0001
    query_class = 0x0001


    def asbytes(self):
        packet_fmt = "!HHHHHHB" + f"{len(self.name)}s" + "B" + f"{len(self.name)}s" + "BHH"
        packet = [
            self.tx_id,
            self.flags,
            self.questions,
            self.answers,
            self.authority,
            self.additional,
            len(self.name),
            self.name,
            len(self.domain),
            self.domain,
            self.end,

            self.query_type,
            self.query_class,
        ]

        encoded_packet = struct.pack(packet_fmt, *packet)

        return encoded_packet

MAC = "1c:1b:b5:d0:85:72"

HOST = "127.0.0.1"
PORT = 11337

me = (HOST, PORT)

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

discover = DHCPPacket.Discover(MAC)

# send discover
sock.sendto(discover.asbytes, me)

# receive reply
res = sock.recv(1024)

# decode offer
offer = DHCPPacket.from_bytes(res)

print(offer)
print("My IP is", offer.yiaddr)


HOST = "127.0.0.1"
PORT = 11338

me = (HOST, PORT)

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

blob = DNSPackage()

sock.sendto(blob.asbytes(), me)

# receive reply
res = sock.recv(1024)

print(res)
