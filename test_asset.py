import socket
import random
import struct
import time

fmt = '>cii'
s = socket.socket()
s.connect(('127.0.0.1', 3000))

data = [(i, random.randint(0, 2**31)) for i in range(10)]

for item in data:
    msg = struct.pack(fmt, b'I', *item);
    s.sendall(msg)

for ts, value in data:
    msg = struct.pack(fmt, b'Q', ts, ts)
    s.sendall(msg)
    response = s.recv(4)
    result = struct.unpack('>i', response)[0]
    print('<<<', result, value)
    assert result == value
