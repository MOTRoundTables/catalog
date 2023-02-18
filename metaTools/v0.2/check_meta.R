# catalog tools - ver. 0.2

#source("metaTools/v0.1/metalib.R") # why ?
source("metalib.R")                 # this is the working dir ? isn't ir
source("files_list.R")              # specify here the file names  

# Please specify:
myfile = 1
dr = x[[myfile]]$dr #  the directory where the zip was opened
fl = x[[myfile]]$fl #  is the name of the meta-file (without file type -assumed .xlsx)


# -------------------------------------------------------
# create json and check meta-data:

startrepfile() 
meta = getjsonmeta(dr, fl) # read meta json (if not there create it from xlsx)
tellmeta(meta)  # show what is it about
chkmetaheaderkeys(meta) # check required keys in header are present
checkmetafilefields(meta) # check file field names
# checkmetafilevalues(meta) # for field with values check that values are valid
printrepfile(dr, fl)

print("done ...")

# -------------------------------------------------------

# open selected file
f1 = openmetafile(meta, 1)  # opens selected data file


# -------------------------------------------------------




