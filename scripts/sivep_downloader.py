import os, shutil
from datetime import date, timedelta, datetime
from lxml import html 
import requests 
import re
from glob import glob

def find_last_date(output_folder):
    filescsv = glob(output_folder + '/*.csv')
    fileszip = glob(output_folder + '/*.zip')
    filesxz = glob(output_folder + '/*.xz')
    files = filescsv + fileszip + filesxz
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

    index_page_address20 = "https://opendatasus.saude.gov.br/dataset/bd-srag-2020"
    index_page_address21 = "https://opendatasus.saude.gov.br/dataset/bd-srag-2021"
    output_folder = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../dados/SIVEP-Gripe')
    gitUpdate = True

    last_date = find_last_date(output_folder)
    newfile20 = check_for_new_file(index_page_address20, last_date)
    newfile21 = check_for_new_file(index_page_address21, last_date)
    if newfile20 and newfile21 and (newfile20[0] == newfile21[0]):
        output_fname = "SRAGHospitalizado_{data}.csv".format(data=newfile20[0].strftime("%Y_%m_%d"))
        outfile = os.path.join(output_folder, output_fname)

        print("Sending out e-mails")
        with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'emails.txt'), "r") as f:
            emails = f.read().strip("\n")
        os.system('''echo -e "Nova base SIVEP-Gripe atualizada.\n
        O relatório de integridade será disponibilizado em alguns minutos no {link}\n
        Atenciosamente,\nRobot mailer" | 
            mail -s "nova base SIVEP-Gripe de {data}" {emails}'''.format(
                        data=newfile20[0].strftime("%Y_%m_%d"),
                        link="https://github.com/covid19br/central_covid/blob/master/dados_processados/integridade_SIVEP/integridade_SIVEP_{data}.html".format(data=newfile20[0].strftime("%Y-%m-%d")),
                        emails=emails))

        print("Downloading new SIVEP database...")
        get_file(newfile20[1], outfile)
        get_file(newfile21[1], outfile + '.21')
        os.system('''cd {folder} &&
                   tail -n +2 {outfile}.21 >> {outfile} &&
                   xz -T4 {outfile} &&
                   rm {outfile}.21'''.format(
            folder=output_folder, outfile=output_fname))
        # add to git and let the other robots work
        if gitUpdate:
            os.system('''cd {folder} &&
                   git add {outfile}.xz &&
                   git commit -m "[auto] base SIVEP-Gripe de {data}" &&
                   rm {outfile} &&
                   git push'''.format(folder=output_folder,
                                    outfile=output_fname,
                                    data=newfile20[0].strftime("%Y_%m_%d")))
            nowcast_folder = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../nowcasting')
            os.system('''cd {nowcast_folder} &&
                    Rscript checa_base.R --updateGit TRUE'''.format(nowcast_folder = nowcast_folder))

