# catalog tools

source("metalib.R")

# dr is the directory where the zip was opened.  fl is the name of the meta-file

#dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/tayarut/tourism20162017/"
#fl = "tourist_meta_0.1"

#dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/busvalid/"
#fl = "Metadata-Bus_Tickets_Validation_Survey_2021"

dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/obstru/"
fl = "Metadata-Obstruction in public transportation survey_2021"


# -------------------------------------------------------

# Create a json file from the excel meta
metaxls2json(dr, fl)
# after preparing the file is needed to convert to utf8 to use in R

# -------------------------------------------------------

# read meta json
meta = getjsonmeta(dr, fl)

tellmeta(meta)




meta = tellmeta(dr, fl)

f1 = openmetafile(meta, 1)

metafields = getmetafilefields(meta, 1)
filefields = colnames(f1)

library(base)
differences <- setdiff(filefields, metafields)

A = getmetafilefields(meta, 1)
B = colnames(f1)

A %in% B

intersect(A,B)
setdiff(A,B)
setdiff(B,A)
compare.list(filefields, metafields)

chkmeta(meta, 1, "field names", f1)


print("done ...")
