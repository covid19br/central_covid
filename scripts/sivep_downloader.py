import os
from datetime import date, timedelta
from lxml import html 
import requests 
import re
 
def check_for_new_file(index_page_address, data):
    page = requests.get(index_page_address, verify=False, timeout=1)
    tree = html.fromstring(page.content)
    l = tree.xpath('//a[starts-with(text(), "Banco de dados de 2020")]')[0]
    data_read = re.match(r'.*(\d\d/\d\d/\d\d\d\d)', l.text_content()).groups()[0]
    if data_read == data.strftime("%d/%m/%Y"):
        return index_page_address + l.attrib['href']
    return False

def get_file(download_address, output_file):
    r = requests.get(download_address, verify=False, allow_redirects=True, timeout=1)
    open(output_file, 'wb').write(r.content)

if __name__ == '__main__':
    data = date.today()

    index_page_address = "http://plataforma.saude.gov.br/coronavirus/dados-abertos/"
    output_folder = os.path.join(os.path.abspath(__file__), '../dados/SIVEP-Gripe')
    output_fname = "SRAGHospitalizado_{data}.csv".format(data=data.strftime("%Y_%m_%d"))
    outfile = os.path.join(output_folder, output_fname)
    gitUpdate = False

    newfile = check_for_new_file(index_page_address, data)
    if not os.path.exists(outfile) and newfile:
        print("Downloading new SIVEP database...")
        get_file(newfile, outfile)
        # add to git and let the other robots work
        if gitUpdate:
            os.system('''cd {folder} &&
                   git add {outfile} &&
                   git commit -m "[auto] base esus-ve de hoje" &&
                   git push'''.format(folder=output_folder,
                                    outfile=output_fname))


