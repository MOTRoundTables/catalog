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
checkmetafilesfields(meta) # check file field names
# checkmetafilevalues(meta) # for field with values check that values are valid
printrepfile(dr, fl)
print("done ...")

# -------------------------------------------------------

# manual checks

startrepfile() 
f1 = openmetafile(meta, 1)  # opens selected data file

f1 = openmetafile(meta, 7)  # opens selected data file

class(f1)
attr(f1, "sf_column") ## [1] "geometry"
st_crs(f1)


print(f1[1:2], n = 3)

s = print(f1, n = 0)
my_str <- capture.output(print(f1, n = 0))



st_geometry_type(f1,1)

type_sumsfc(f1)
obj_sum.sfc() pillar_shaft.sfc()


printrepfile(dr, "manual")
print("done ...")





# -------------------------------------------------------




