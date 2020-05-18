import os
from datetime import date, timedelta
from lxml import html 
import requests 
import re
 
def check_for_new_file(index_page_address, data):
    page = requests.get(index_page_address, verify=False, timeout=1)
    tree = html.fromstring(page.content)
    resources = tree.xpath('//li[@class="resource-item"]')
    reg = re.compile(r".*dados-nacionais.*Arquivo gerado no dia (\d\d/\d\d/\d\d\d\d).*",
            re.DOTALL|re.MULTILINE|re.IGNORECASE)
    for item in resources:
        g = reg.match(item.text_content())
        if g:
            data_read = g.groups()[0]
            if data_read == data.strftime("%d/%m/%Y"):
                return True
    return False

def get_file(download_address, output_file):
    r = requests.get(download_address, verify=False, allow_redirects=True, timeout=1)
    open(output_file, 'wb').write(r.content)

if __name__ == '__main__':
    data = date.today()

    index_page_address = 'http://shiny.hmg.saude.gov.br/tr/dataset/casos-nacionais'
    download_address = 'http://ckan.saude.gov.br.s3.amazonaws.com/dados-nacional.csv'
    output_folder = os.path.join(os.path.abspath(__file__), '../dados/eSUS-VE')
    output_fname = 'esus-ve_{data}.csv'.format(data=data.strftime("%Y_%m_%d"))
    outfile = os.path.join(output_folder, output_fname)
    gitUpdate = False

    if not os.path.exists(outfile) and \
            check_for_new_file(index_page_address, data):
        print("Downloading new esus-ve database...")
        get_file(download_address, outfile)
        # add to git and let the other robots work
        if gitUpdate:
            os.system('''cd {folder} &&
                   git add {outfile} &&
                   git commit -m "[auto] base esus-ve de hoje" &&
                   git push'''.format(folder=output_folder,
                                    outfile=output_fname))

