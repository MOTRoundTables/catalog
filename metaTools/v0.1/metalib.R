library(readxl)
library(jsonlite)
library(tidyverse) # library(purrr)
#library(stringr)

# ---------------------------------------

chkmeta <- function(meta, filenum, test, f1) {
  # if f1 null then open the file
  # f1 = openmetafile(meta, 1)
  
  file = meta 
    
}

checkmetafilefields <- function(meta, filenum, f1) {
  metafields = getmetafilefields(meta, filenum)
  filefields = colnames(f1)

}
  

getmetafilefields <- function(meta, filenum) {
  file = meta$Files[filenum]
  file = file[[1]]
  fields = file$"File fields"
  # nfields = length(fields)
  fieldnames <- map_chr(fields, function(x) x[["name"]])
  return(fieldnames)
}


# ---------------------------------------

openmetafile <- function(meta, filenum) {
  fl = meta$"Files list"[filenum]
  print( paste("opening:", fl ) )
  fl1 = paste(meta$dir, fl, sep="")
  
  if (file.exists(fl1)) {
    readfile <- read.csv(fl1)
  } else {
    print("File not found")
    readfile = NULL
  }
  return(readfile)
}
  
# ---------------------------------------

getjsonmeta <- function(dr, fl) { 
  fl1 = paste(dr,fl,".json",sep="")
  meta = read_json(fl1, simplifyVector = FALSE)
  meta$dir = dr
  
  print( paste("This is:", meta$Title ) )
  nfiles = length(meta$"Files list")
  print( paste("it includes", nfiles, "Files") )

  return(meta)
}

tellmeta <- function(meta) {
  #browser()
  lines = c(        paste("    Publisher:", meta$Publisher))
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
  writeLines(lines)
}

# ---------------------------------------

metaxls2json <- function(dr, fl) { 
  fl1 = paste(dr,fl,".xlsx",sep="")
  if (!file.exists(fl1)) {
    print("File not found")
    return()
  }  

  # read excel file
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
  print( paste("There are", nfiles, "Files") )

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
  writeLines(json_string, fl1)

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
      message(paste("warning: suspicious date:", dt))
    }
  } else {
    date = dt  # leave it as it was
    message(paste("warning: suspicious date:", dt))
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

# = end ===============================================

