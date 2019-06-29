import lxml.etree as et
from idmreader import *
import os


def search(word):
    DATA_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'ldoce5.data')

    reader = ArchiveReader(DATA_DIR, 'fs')

    for _path, _name, location in list_files(DATA_DIR, 'fs'):
        data = reader.read(location).decode()
        root = et.fromstring(data)
        base = root.find('Head/HWD/BASE').text
        if word == base:
            return data


if __name__ == '__main__':
    import sys
    word = sys.argv[1]
    data = search(word)
    if data:
        print(data)
    else:
        print(f'{word} is not found')
        exit(1)
        
