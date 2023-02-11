import socket
import json

s = socket.socket()
s.connect(('127.0.0.1', 3000))

def is_prime(x):
    if x < 2:
        return False
    for i in range(2, int(x ** 0.5) + 1):
        if x % i == 0:
            return False
    return True

line = b''
for i in range(2, 128):
    request = json.dumps({'method': 'isPrime', 'number': i}).encode('utf8')
    request += b'\n'
    print('>>>', request)
    s.sendall(request)
    response = s.recv(8192)
    print('<<<', response)
    assert response.endswith(b'\n')
    result = json.loads(response.decode('utf8').rstrip('\n'))
    assert result['prime'] == is_prime(i)

for i in range(1, 128):
    request = json.dumps({'method': 'other', 'number': i}).encode('utf8')
    request += b'\n'
    print('>>>', request)
    s.sendall(request)
    response = s.recv(8192)
    print('<<<', response)
    assert response.endswith(b'\n')
    result = json.loads(response.decode('utf8').rstrip('\n'))

#for i in range(1, 128):
#    request = b'\n\r\n'
#    print('>>>', request)
#    s.sendall(request)
#    response = s.recv(8192)
#    print('<<<', response)
#    assert response.endswith(b'\n')
#    result = json.loads(response.decode('utf8').rstrip('\n'))

for i in range(-64, 0):
    request = json.dumps({'method': 'isPrime', 'number': i}).encode('utf8')
    request += b'\n'
    print('>>>', request)
    s.sendall(request)
    response = s.recv(8192)
    print('<<<', response)
    assert response.endswith(b'\n')
    result = json.loads(response.decode('utf8').rstrip('\n'))
    assert result['prime'] == is_prime(i)

request = json.dumps({'method': 'isPrime', 'number': 754114422844348429081623607423388729583841609498718054658, 'bignumber': True}).encode('utf8')
request += b'\n'
print('>>>', request)
s.sendall(request)
response = s.recv(8192)
print('<<<', response)
assert response.endswith(b'\n')
result = json.loads(response.decode('utf8').rstrip('\n'))
