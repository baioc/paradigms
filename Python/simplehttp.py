import socket
from pprint import pprint
import sys


s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)


def read_headers(file):
	while True:
		line = file.readline().strip()

		if line == b'':
			return

		key, val = line.decode().split(': ', maxsplit=1)
		yield key, val


def read_chunks(file):
	while True:
		line = file.readline().strip()
		chunk_size = int(line, 16)

		if chunk_size == 0:
			return

		yield file.read(chunk_size)
		file.readline()


def get_chunked_body(file):
	return b''.join(chunk for chunk in read_chunks(file))


def send_request(io, headers={}):
	io.write(b'GET / HTTP/1.1\n')

	for header, value in headers.items():
		io.write(f'{header}: {value}\n'.encode())

	io.write(b'\n')
	io.flush()


def fetch_body(file, headers):
	if 'Content-Length' in headers:
		length = int(headers['Content-Length'])
		return file.read(length)

	if 'Transfer-Encoding' in headers and headers['Transfer-Encoding'] == 'chunked':
		return get_chunked_body(file)

	return None


with socket.create_connection((sys.argv[1], 80)) as sock:
	sock_in = sock.makefile('rb')
	sock_out = sock.makefile('wb')

	send_request(
		sock_out,
		# headers={
		# 	'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.103 Safari/537.36 OPR/60.0.3255.151',
		# }
	)

	status = sock_in.readline()
	print(status)

	headers = dict(read_headers(sock_in))
	pprint(headers)

	body = fetch_body(sock_in, headers)
	print(body)
