# ReadDataGov_6.py  - 24/3/23
# This script creates a catolog with all the datasets in datagov.
# The result serves as input to prepare catalog updates.
# The result includes 4 text files (delimited with "|"):
# 1. package_list.txt: a list of data set codes 
# 2. resource_list.txt: list of the datasets with info. like name, decription, publisher, etc.
# 3. resources.txt: list of  the files in each data set
# 4. tags.txt: datasets keywords
# The files are saved in a sub-directory defined by variable "ver".

# directory version:
ver = "220324" # "220909" # "220816",  "220523", "211008", "210715"

print ("read data.gov directory")

import os
from datetime import datetime

# os.chdir(dr)       # Change the current working directory
cwd = os.getcwd()  # Get the current working directory
print("Current working directory: {0}".format(cwd))
dr = cwd + "\\" + ver + "\\"
if not os.path.exists(dr):
    os.mkdir(dr)

log = open(dr+'log.txt', 'w', encoding="utf-8")   # moved to utf8 cause an arabic string caused error in write. at the end converto to ansi with np++
now = datetime.now()
log.write("start: " + now.strftime("%Y-%m-%d %H:%M:%S") + "\n")
log.write("n - Package - success  - n1 - n2\n")

# 1. get a list of datasets
import urllib.request, json 
url0 = 'https://data.gov.il/api/3/action/package_list'
with urllib.request.urlopen(url0) as url:
    data = json.loads(url.read())   # .decode()
    #print(data)
 
success = data["success"]
print(success)

flist = data["result"]  #print(flist)
n = len(flist)
print("There are " + str(n) + " datasets")
f = open(dr+'package_list.txt', 'w') 
print("Name of the file: " + f.name)
f.write('\n'.join([''.join(l2) for l2 in flist]))
f.close()

# 2. search for each dataset
sp = "|"

f = open(dr+'resource_list.txt', 'w', encoding="utf-8")   # moved to utf8 cause an arabic string caused error in write. at the end converto to ansi with np++
f.write("n|name|type|date|title|org|geo|freq|resources|tags|author|email|orgh|licID|lictitle|notes\n")

f1 = open(dr+'resources.txt', 'w', encoding="utf-8")
f1.write("n|rsc|name|format|dateCr|dateMo|id|size\n")

f2 = open(dr+'tags.txt', 'w', encoding="utf-8")
f2.write("n|rsc|org|tag|tagId\n")

notfound = 0
i = 0
while i <= n-1:   # n
    aPackage = flist[i]
    url0 = 'https://data.gov.il/api/3/action/package_show?id=' + aPackage

    try:
        with urllib.request.urlopen(url0) as url:  
            data = json.loads(url.read())          # .decode()
    except:
        print(i, aPackage, "FAIL", sep=" - ")
        log.write(str(i) + " - " + aPackage + " - " + "FAIL" + "\n")
        continue

    success = data["success"]

    print(i, aPackage, success )
    log.write(str(i) + " - " + aPackage + " - " + str(success) + "\n")

    if success:
        rsc = data["result"]
        rscname = rsc["name"]
        if aPackage == rscname:
            rsctitle = rsc["title"] 
            rsctype = rsc["type"]
            rscdate = rsc["metadata_modified"]
            rscgeo = rsc["is_geographic"] if "is_geographic" in rsc else "No"
            rscfreq = rsc["Frequency"] if "Frequency" in rsc else "NA"
            num_resources = rsc["num_resources"]
            num_tags = rsc["num_tags"]
            authorname = rsc["author"] if rsc["author"] is not None else "--"
            authormail = rsc["author_email"] if rsc["author_email"] is not None else "--"
            notes = rsc["notes"] if rsc["notes"] is not None else "--"
            notes = notes.strip().replace("\r","").replace("\n","").replace("\t"," ").replace(chr(149),"o")
            org     = rsc["organization"] 
            orgname = org["name"]
            orgtitle = org["title"]
            licID = rsc["license_id"] 
            lictitle = rsc["license_title"]

            # print("    ", j, rscname, sep=" - ")
            f.write(str(i) + sp + rscname + sp + rsctype + sp + rscdate[:10] + sp +  \
                rsctitle + sp + orgname + sp + rscgeo + sp + rscfreq + sp + str(num_resources) + sp + str(num_tags) + sp + \
                authorname + sp + authormail + sp + orgtitle + sp + licID + sp + lictitle + sp + notes + "\n")
            # save resources    
            rscs = rsc["resources"]  # list resources
            k = 0
            while k <= num_resources-1:
                krsc = rscs[k] 
                created = krsc["created"] if krsc["created"] is not None else "--"
                last_modified = krsc["last_modified"] if krsc["last_modified"] is not None else "--"
                rscnam = krsc["name"].replace('\t', '')
                f1.write(str(i) + sp + rscname + sp + rscnam + sp + krsc["format"] + sp + created[:10] + sp + last_modified[:10] + sp + \
                    krsc["id"] + sp + str(krsc["size"]) + "\n")    
                k += 1                    
            # save tags
            tags = rsc["tags"]  # list tags
            k = 0
            while k <= num_tags-1:
                ktag = tags[k] 
                vocid = ktag["vocabulary_id"] if ktag["vocabulary_id"] is not None else "--"
                f2.write(str(i) + sp + rscname + sp + orgname + sp + ktag["name"] + sp + vocid + "\n")     # + sp + krsc["id"]
                k += 1
        else:        
            log.write("error: result name different from package " + aPackage + " <> " + rscname + "\n")
            notfound += 1

    else:
        log.write("error: Package not retrieved " + aPackage + "\n")
        notfound += 1

    i += 1  # end while

f.close()
f1.close()
f2.close()

log.write("not found " + str(notfound) + " datasets" + "\n")
log.write("end: " + now.strftime("%Y-%m-%d %H:%M:%S"))
log.close()
print("not found " + str(notfound) + " datasets" + "\n")
print("done ...")
print ("data.gov directory - saved in sub-directory: " + ver)

# == end ==============================================

'''
            if "is_geographic" in rsc:
                rscgeo = rsc["is_geographic"] 
            else:
                rscgeo = "No"
            if "Frequency" in rsc:
                rscfreq = rsc["Frequency"]
            else:    
                rscfreq = "NA"
'''    


'''

                if ktag["vocabulary_id"] is None:
                    vocid = "--"
                else:    
                    vocid = ktag["vocabulary_id"]


# url0 = 'https://data.gov.il/api/3/action/datastore_search?resource_id=f7b8b843-a95c-4380-9656-b78a7eacb95e&limit=5&q=title:jones'  


import urllib
url = 'https://data.gov.il/api/3/action/datastore_search?resource_id=f7b8b843-a95c-4380-9656-b78a7eacb95e&limit=5&q=title:jones'  
fileobj = urllib.urlopen(url)
print fileobj.read()


import urllib.request
url = 'https://data.gov.il/api/3/action/datastore_search?resource_id=f7b8b843-a95c-4380-9656-b78a7eacb95e&limit=5&q=title:jones'  

req = urllib.request.Request(url)
with urllib.request.urlopen(req) as response:
   the_page = response.read()


import shutil
import tempfile
import urllib.request

with urllib.request.urlopen('http://python.org/') as response:
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file:
        shutil.copyfileobj(response, tmp_file)

with open(tmp_file.name) as html:
    pass


"http://maps.googleapis.com/maps/api/geocode/json?address=google"


with open(dr+'output.txt', 'w') as f:
    f.write('\n'.join([''.join(l2) for l2 in flist]))    
    #json.dump(flist, f)


'''    
