# catalog tools - ver. 0.1
source("metalib.R")

# Please specify:
# dr is the directory where the zip was opened
# fl is the name of the meta-file (without file type -assumed .xlsx)

# dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/tayarut/tourism_2016_2017_0.2/"
# fl = "tourist_meta_0.2"

# dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/busvalid/"
# fl = "Metadata-Bus_Tickets_Validation_Survey_2021"

# dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/obstru/"
# fl = "Metadata-Obstruction in public transportation survey_2021"

# -------------------------------------------------------

check2file(dr, TRUE) # creates "checkmeta.txt" in dr. FALSE to console

meta = getjsonmeta(dr, fl) # read meta json (if not there create it from xlsx)

tellmeta(meta)  # show what is it about

chkmetaheaderkeys(meta) # check required keys in header are present

checkmetafilefields(meta) # check file field names

#checkmetafilevalues(meta) # for field with values check that values are valid

# -------------------------------------------------------

sink() 
sink(type="message")
print("done ...")

# -------------------------------------------------------




