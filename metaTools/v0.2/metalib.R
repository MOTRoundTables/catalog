# metadata tools function library - ver. 0.2
library(readxl)
library(jsonlite)
library(tidyverse) # library(purrr)
#library(stringr)
library(skimr)  # https://cran.r-project.org/web/packages/skimr/

# ---------------------------------------
# check file functions

checkmetafilefields <- function(meta) {
  #browser()
  
  addrep("check individual files", 2, 1)
  file_required_keys = c(  "File name", "File format", "File description", "File fields")
  
  nfiles = length(meta$"Files list")
  for (i in 1:nfiles) { 
    addrep(paste(i, ": check fields for ", meta$"Files list"[i], sep="" ), 1, 1)
    keys = names(meta$Files[[i]])
    missing1 = setdiff(file_required_keys,keys)
    if (length(missing1)>0) {
      addrep(paste("Error: Missing required keywords in file header:", missing1))
    }    
    addrep(paste("Description:", meta$Files[[i]]$"File description"))
    
    f1 = openmetafile(meta, i)  # opens selected data file
    metafields = getmetafilefields(meta, i)
    filefields = x = colnames(f1) # f1$headline
    differences <- setdiff(filefields, metafields)
    if (length(differences)>0) {
      addrep(paste("Error: discrepacies if meta and file field names:", differences))
    }
    addrep(shortsummary(f1))
    #print(x)
  }
}

shortsummary <- function(f1) {
  #head(f1)
  #summary(f1) 
  x = skim(f1) %>%
    summary() 
  a <- capture.output(print(x, quote=FALSE))
  b = c("")
  for (i in 1:length(a)) {b = c(b, a[i] )}
  return(b)
}

getmetafilefields <- function(meta, filenum) {
  file = meta$Files[filenum]
  file = file[[1]]
  fields = file$"File fields"
  # nfields = length(fields)
  fieldnames <- map_chr(fields, function(x) x[["name"]])
  return(fieldnames)
}

openmetafile <- function(meta, filenum) {
  fl = meta$"Files list"[filenum]
  print( paste("opening:", fl ) )
  fl1 = paste(meta$dir, fl, sep="")
  
  if (file.exists(fl1)) {
    readfile <- read_csv(fl1)
  } else {
    print("File not found")
  }
  return(readfile)
  #return(list(readfile=readfile, headline=headline))
}

getmetacsvheadline <- function(meta, filenum) {  # gets 1st line of a csv
  headline <- read_lines(fl1,n_max = 1) %>% str_split(",") %>% unlist()
}


# ---------------------------------------
# check header  functions

chkmetaheaderkeys <- function(meta) {
  
  hdr_required = c("Publisher", "Contact", "Title", "Description", "Keywords",
                   "Created", "Last updated", "Temporal coverage", 
                   "Spatial coverage", "Dataset file", "Metadata creation date")
  
  files_required = c("Files list", "Files")

  keys = names(meta)
  missing1 = setdiff(hdr_required,keys)
  
  if (length(missing1)>0) {
    addrep(paste("Error: Missing required keywords in header:", missing1), 1)
  }

  missing2 = setdiff(files_required,keys)
  if ((length(meta$"Files list")>0)&(length(missing2)>0)) {
    addrep(paste("Error: There are several files but missing in header:", missing2), 1)
  }

}

# ---------------------------------------
# read & display metadata 

getjsonmeta <- function(dr, fl) { 
  
  fl1 = paste(dr,fl,".json",sep="")
  if (!file.exists(fl1)) {
    print("json meta not found - will attempt to create one")
    metaxls2json(dr, fl) # Create a json file from the excel meta
  }  

  fl1 = paste(dr,fl,".json",sep="")
  meta = read_json(fl1, simplifyVector = FALSE)
  meta$dir = dr
  
  addrep( paste("This is:", meta$Title ), 1 )
  nfiles = length(meta$"Files list")
  addrep( paste("it includes", nfiles, "data files:") )
  for (i in 1:nfiles) { 
    addrep(paste(i, ": ", meta$"Files list"[i], sep="" ))
  }
  return(meta)
}

listmetadatafiles <- function(meta) {
  fls = meta$"Files list"
  for (fl in fls) {
    print( fl )
  }
}  

tellmeta <- function(meta) {
  #browser()
  lines = c("", "Metafile Header:")
  lines = c( lines, paste("    Publisher:", meta$Publisher))
  lines = c( lines, paste("      Contact:", meta$Contact))
  lines = c( lines, paste("Contact Email:", meta$"Contact Email"))
  lines = c( lines, paste("       Author:", meta$Author))
  if (!is.null(meta$"Author Email")) {
    lines = c( lines, paste(" Author Email:", meta$"Author Email"))
  }
  lines = c( lines, "- - - - - - -")
  lines = c( lines, paste("       Title:", meta$Title))
  lines = c( lines, paste("     Created:", meta$Created))
  lines = c( lines, paste("Temporal coverage:", meta$"Temporal coverage"))
  if (is.list(meta$Description)) {
    lines = c( lines, "Description:")
    for (p in meta$Description) {
      lines = c( lines, str_pad(p, 80, side='left'))
    }  
  } else {
    lines = c( lines, paste("       Description:", meta$Description))
  }
  addrep(lines)   #writeLines(lines)
}

# ---------------------------------------
# convert metadata from xlsx to json

metaxls2json <- function(dr, fl) { 
  fl1 = paste(dr,fl,".xlsx",sep="")
  if (!file.exists(fl1)) {
    print("File not found")
    return()
  }  

  # read excel file
  addrep(paste("reading", fl1))
  print(paste("reading", fl1))
  data <- read_excel(fl1,col_names = FALSE)
  num_rows <- nrow(data)
  rng = paste("A1:G", as.integer(num_rows), sep="")
  columns = c("keyword", "name", "type", "description", "comment", "value", "label")
  data <- read_excel(fl1, sheet=1, range=rng, col_names = columns)

  data$lvl <- apply(data, 1, setlvl)
  data$cont <- apply(data, 1, setcont)

  # build header
  tmp <- readkeys(data, 1, "Files")
  i = tmp$last
  hdr = tmp$meta

  # build files 
  nfiles = length(hdr$"Files list")
  #print( paste("This metadata includes", nfiles, "Files") )

  files = list()
  for (f in 1:nfiles) {  
    tmp <- readkeys(data, i, "File fields")
    i = tmp$last
    filehdr = tmp$meta

    tmp <- readflds(data, i, "File name")
    i = tmp$last
    fileflds = tmp$meta
    #browser()

    filehdr[["File fields"]] = fileflds
    files <- append(files, list(filehdr))
    
  }
  hdr[["Files"]] = files   # add files to header

  # print json to file
  json_string <- toJSON(hdr, pretty = TRUE, auto_unbox = TRUE,)
  
  fl1 = paste(dr,fl,".json",sep="")
  print(paste("writing", fl1))
  writeLines(json_string, fl1, useBytes = TRUE)  # usebytes writes in UTF8
  addrep("finished converting meta from excel to json")
  
  #browser()
}

# -------------------------------------

readflds = function (data, strt, stp) {
  # browser()
  stp = toupper(stp)
  
  fields = list()
  field = NA
  vals = NA
  val = list()
  for (i in strt:(nrow(data)+1)) {  
    if ( (!is.na(data$keyword[i]) & (toupper(data$keyword[i])==stp))| i == (nrow(data)+1)) {
      if (is.list(vals)) {
        field$vals = vals
      }
      fields <- append(fields, list(field))                 # add last field to result
      break
    }
    
    if (!is.na(data$name[i])) { #  "name", "type", "description", "comment", "value", "label")
      # save field
      if (is.list(field)) {
        if (is.list(vals)) {
          field$vals = vals
        }
        fields <- append(fields, list(field))
      }
      
      #new field
      field = list()
      vals = NA
      field$name = data$name[i]
      
      if (!is.na(data$type[i]))    field$type = data$type[i]
      if (!is.na(data$type[i]))    field$description = data$description[i]
      if (!is.na(data$comment[i])) field$comment = data$comment[i]
      
      if (!is.na(data$value[i])) {
        vals = list()
        vals[[ data$value[i] ]] <- data$label[i]
      }
    }
    else {
      vals[[ data$value[i] ]] <- data$label[i]
    } 
  }  
  
  return(list(last = i, meta = fields))
}


# -------------------------------------

readkeys = function (data, strt, stp) {
  #browser()
  stp = toupper(stp)
  
  datekeys = c("CREATED", "LAST UPDATED", "METADATA CREATION DATE", "FILE DATE")
  # block_keys = {"Related documents", "Comments", "Files list", "Description"}
  
  jmeta = list()
  x = NA
  for (i in strt:nrow(data)) {  
    #if ( (data$lvl[i] == 1) & (toupper(data$keyword[i])==stp) ) {
    if ( !is.na(data$keyword[i]) & (toupper(data$keyword[i])==stp) ) {
      jmeta = c(jmeta, x)                          # add last name to result
      break
    }
    
    if (data$cont[i] == 1) {                       # continuation
      z = append(x[[ky]], data$name[i])            # add name to keyword list
      x[[ ky ]] <- z
    } else if (data$lvl[i] == 2) {                 # new keyword
      if (is.list(x)) {
        jmeta = c(jmeta, x)                        # add previous key to header
      }
      x = list()
      ky = data$keyword[i]
      
      if (toupper(ky) %in% datekeys) {
        x[[ ky ]] <- getformatteddate(data$name[i])
      } else {
        x[[ ky ]] <- data$name[i]
      }
    }  
  }
  return(list(last = i+1, meta = jmeta))
}

# -------------------------------------

getformatteddate <- function(dt) {
  #browser()
  if (grepl("^[0-9]+$", dt)) {  # dt is only digits, ie. a valid excel date
    date = as.integer(dt)
    if (date>14000) {      # probably a "dd/mm/yyyy" date
      date <- as.Date(as.integer(dt), origin = "1899-12-30")
      date <- format(date, format = "%d/%m/%Y")
    } else {
      date = dt  # leave it as it was
      addrep(paste("warning: suspicious date:", dt))
    }
  } else {
    date = dt  # leave it as it was
    addrep(paste("warning: suspicious date:", dt))
  }  
  return(date)
}


setlvl <- function(row) {
  if (!is.na(row[7])) {
    x = 7 
  } else if (!is.na(row[6])) { 
    x = 6 
  } else if (!is.na(row[5])) { 
    x = 5 
  } else if (!is.na(row[4])) { 
    x = 4 
  } else if (!is.na(row[3])) { 
    x = 3 
  } else if (!is.na(row[2])) { 
    x = 2 
  } else if (!is.na(row[1])) { 
    x = 1 
  } else { 
    x = 0 
  }
  return(x)
}

setcont <- function(row) {
  if ( (is.na(row[1])) & (row[8]==2) ) { 
    x = 1 
  } else { 
    x = 0 
  }
}

# ---------------------------------------
# reports subsystem

rep <- NULL # a list of report lines

startrepfile <- function() {
  rep <<- c('meta tools report', format(Sys.time(), "%a %b %d")) # "%a %b %d %X %Y"
}

addrep <- function(s, lines_before=0, lines_after=0) {  # instead of print
  if (lines_before>0)  {
    for (i in 1:lines_before) { s = c("", s) }
  }
  if (lines_after>0)  {
      for (i in 1:lines_after) { s = c(s, "") }
  }
  rep <<- c(rep, s)
}

printrepfile <- function(dr=NULL, fl=NULL, clear=TRUE) {
  rep <<- c(rep, "", "Finished")
  if ( is.null(dr) & is.null(fl) ) {
    writeLines(rep) # to console
  } else {
    fileConn <- file(paste(dr, "check_", fl, ".txt", sep=""))
    writeLines(rep, fileConn)
    close(fileConn)    
  }
  if (clear) {rep <<- NULL}
}

# file.show("outfile.txt")

# old function - not in use
# check2file(dr, fl, TRUE) # creates "checkmeta.txt" in dr. FALSE to console
# sink(type = c("output", "message")) 
check2file <- function(dr, fl, option) {
  if (option) {
    con <- file(paste(dr,"check_", fl, ".txt",sep=""))
    sink(con, append=FALSE, type = "output")
    #sink(con, append=FALSE, type = c("output", "message"))
  } else {
    sink(type = "output") 
    #sink(type = c("output", "message")) 
  }
}

# = end ===============================================

