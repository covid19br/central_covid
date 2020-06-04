import os, shutil
from datetime import date, timedelta, datetime
from lxml import html 
import requests 
import re
from glob import glob

def find_last_date(output_folder):
    filescsv = glob(output_folder + '/*.csv')
    fileszip = glob(output_folder + '/*.zip')
    files = filescsv + fileszip
    date_max = date(year=2020, month=1, day=1)
    for f in files:
        g = re.match(r'.*(\d\d\d\d_\d\d_\d\d).*', os.path.basename(f))
        if g:
            data = datetime.strptime(g.groups()[0], "%Y_%m_%d").date()
            date_max = max(data, date_max)
    return date_max

def check_for_new_file(index_page_address, last_date):
    page = requests.get(index_page_address, verify=False, timeout=10)
    tree = html.fromstring(page.content)
    resources = tree.xpath('//li[@class="resource-item"]')
    reg = re.compile(r".*SRAG (\d\d/\d\d/\d\d\d\d).*",
            re.DOTALL|re.MULTILINE|re.IGNORECASE)
    for item in resources:
        g = reg.match(item.text_content())
        if g:
            data_read = datetime.strptime(g.groups()[0], "%d/%m/%Y").date()
            if data_read > last_date:
                address = item.xpath('.//a[@class="resource-url-analytics"]')[0].attrib['href']
                return (data_read, address)
    return False

def get_file(download_address, output_file):
    r = requests.get(download_address, verify=False, allow_redirects=True, timeout=100)
    open(output_file, 'wb').write(r.content)
    if download_address[-4:] == 'xlsx':
        print("Converting xlsx to csv...")
        xlsfile = output_file[:-3] + 'xlsx'
        shutil.move(output_file, xlsfile)
        os.system('libreoffice --headless --convert-to csv:"Text - txt - csv (StarCalc)":59,34,0,1,1 {xlsfile} --outdir {output_dir}'.format(xlsfile=xlsfile, output_dir=os.path.dirname(output_file)))

if __name__ == '__main__':
    data = date.today()

    index_page_address = "https://shiny.hmg.saude.gov.br/tr/dataset/bd-srag-2020"
    output_folder = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../dados/SIVEP-Gripe')
    gitUpdate = True

    last_date = find_last_date(output_folder)
    newfile = check_for_new_file(index_page_address, last_date)
    if newfile:
        output_fname = "SRAGHospitalizado_{data}.csv".format(data=newfile[0].strftime("%Y_%m_%d"))
        outfile = os.path.join(output_folder, output_fname)
        print("Downloading new SIVEP database...")
        get_file(newfile[1], outfile)
        outzip = output_fname[:-3] + 'zip'
        os.system('cd {folder} && zip {outzip} {outfile}'.format(
            folder=output_folder, outfile=output_fname, outzip=outzip))
        # add to git and let the other robots work
        if gitUpdate:
            os.system('''cd {folder} &&
                   git add {outzip} &&
                   git commit -m "[auto] base SIVEP-Gripe de {data}" &&
                   rm {outfile} &&
                   git push'''.format(folder=output_folder,
                                    outzip=outzip,
                                    outfile=output_fname,
                                    data=newfile[0].strftime("%Y_%m_%d")))


