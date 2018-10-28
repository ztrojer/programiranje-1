import requests
import re
import os
import csv
###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = (
    'http://www.bolha.com/zivali/male-zivali/'
    'macke/?sort=0&page=1'
)
# mapa, v katero bomo shranili podatke
cat_directory = '2-zajem-podatkov/vaje'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'macki.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'macki.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # some code here that may raise an exception
        r = requests.get(url)
        # some more code that won't be run if the exception occured
    except requests.exceptions.ConnectionError:
        # some error handling / recovery code here
        # we may just display an informative message and quit
        print('Napaka!')
        return None
    # continue with the non-exceptional code
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(url, directory, filename):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    text = download_url_to_string(url)
    return save_string_to_file(text, directory, filename)

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    path = os.path.join(directory, filename)
    with open(path, 'r', encoding='utf-8') as file_in:
        return file_in.read()

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(webpage):
    '''Split "page" to a list of advertisement blocks.'''
    sample1 = re.compile(
        r'<div class="coloumn image">'
    )
    sample2 = re.compile(
        r'<div class="clear">&nbsp;</div>'
    )
    list = re.split(sample1, webpage)
    last = list[-1]
    last = re.split(sample2, last)
    list[-1] = last[0]
    return list[1:]

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.

def get_dict_from_ad_block(block):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    sample = re.compile(
        r'<h3><a title="(?P<ime>.+?)" href=".*?'
        r'</a></h3>' r'\n\n' r'\s+(?P<informacije>.*?)\s+<div class=".*?'
        r'<div class="price">(<span>|)(?P<cena>.*?)(</span>|)</div>.*?'
        r'<a href="javascript:void\(0\);" data-id="(?P<id>\d+)" title=.*?',
        re.DOTALL
    )
#ta for zanka je brezveze, lahko samo poiscem brez for zanke
    for expression in sample.finditer(block):
        dict = expression.groupdict()
    return dict

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    webpage = read_file_to_string(directory, filename)
    list_of_ads = page_to_ads(webpage)
    list_of_dicts = []
    for i in range(0, len(list_of_ads)):
        list_of_dicts.append(get_dict_from_ad_block(list_of_ads[i]))
    return list_of_dicts

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.

def write_cat_ads_to_csv(list_of_dicts_ads, directory, filename):
    '''Write a CSV file containing one ad from "ads" on each row.'''
    fieldnames = ['id', 'ime', 'informacije', 'cena']
    write_csv(fieldnames, list_of_dicts_ads, directory, filename)
    return None