# catalog tools - ver. 0.1
source("metalib.R")

# Please specify:
# dr is the directory where the zip was opened
# fl is the name of the meta-file (without file type -assumed .xlsx)

# dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/tayarut/tourism_2016_2017_0.2/"
# fl = "tourist_meta_0.2"

# dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/busvalid/"
# fl = "Metadata-Bus_Tickets_Validation_Survey_2021"

#dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/razon/"
#fl = "Metadata-National_Satisfaction_Public_Transport"

#dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/pubopinion/"
#fl = "Metadata-Public_opinion_on_public_transportation_Survey_2021"
      
# dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/obstru/"
# fl = "Metadata-Barriers in public transportation survey_2021"

#dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/info/"
#fl = "Metadata-Information_on_public_transportation_Survey_2021"

dr = "C:/Users/marsz/Desktop/zvl/tmp24/Data/amit/arab/"
fl = "Metadata-Trucks_Parking_Arab_Settlements_2020"


# -------------------------------------------------------

check2file(dr, fl, TRUE) # creates "checkmeta.txt" in dr. FALSE to console

meta = getjsonmeta(dr, fl) # read meta json (if not there create it from xlsx)

tellmeta(meta)  # show what is it about

chkmetaheaderkeys(meta) # check required keys in header are present

checkmetafilefields(meta) # check file field names

#checkmetafilevalues(meta) # for field with values check that values are valid

# -------------------------------------------------------

# open selected file
f1 = openmetafile(meta, 1)  # opens selected data file

# -------------------------------------------------------

sink(type = c("output", "message")) 
print("done ...")

# -------------------------------------------------------




