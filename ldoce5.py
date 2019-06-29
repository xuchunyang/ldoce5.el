import lxml.etree as et
from idmreader import *
import os

DATA_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'ldoce5.data')
READER = ArchiveReader(DATA_DIR, 'fs')


def list():
    for _, _, location in list_files(DATA_DIR, 'fs'):
        data = READER.read(location).decode()
        root = et.fromstring(data)
        base = root.find('Head/HWD/BASE').text
        print(f'{base} | {location}')

# '(24596514, 9096, 60641, 963)' => (24596514, 9096, 60641, 963)
def str_to_location(s):
    return tuple(int(x) for x in s.strip('()').split(','))

def lookup(s):
    location = str_to_location(s)
    print(READER.read(location).decode())

def search(word):
    for _path, _name, location in list_files(DATA_DIR, 'fs'):
        data = READER.read(location).decode()
        root = et.fromstring(data)
        base = root.find('Head/HWD/BASE').text
        if word == base:
            print(data)
            return

    print(f'{word} is not found')
    exit(1)


if __name__ == '__main__':
    import sys
    action = sys.argv[1]
    if action == 'search':
        word = sys.argv[2]
        search(word)
    elif action == 'list':
        list()
    elif action == 'lookup':
        lookup(sys.argv[2])
