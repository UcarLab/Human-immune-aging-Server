

# Helper functions for the UCAR DATA SHINY APP

library(magrittr)

DATA_DIR <- file.path("/Users/oneils/Desktop/trial_369_another_3/data1")

# Read the data and get it ready for the app
getData <- function() {
	
	# read the data file
  cDat<-readRDS("data1/onedata_frame.RDS")
	cDat
}

getData_pathway <- function() {
  
  cDat_pathway<-readRDS("data1/final_data_front3.RDS")
 
  cDat_pathway
}
getData_cell <- function() {
  
  # read the data file
 cDat_cell<-readRDS("data1/up_cell_dataframe.RDS") # old data frame with ID
          
  
  cDat_cell
}


# Our data has many peak co location, so when plotting I wanted to have a good
# set of 61 unique colours
 getPlotCols <- function() {
 	c22 <- c("dodgerblue2","#E31A1C", # red
 					 "green4",
 					 "#6A3D9A", # purple
 					 "#FF7F00", # orange
 					 "black","gold1",
 					 "skyblue2","#FB9A99", # lt pink
 					 "palegreen2",
 					 "#CAB2D6", # lt purple
 					 "#FDBF6F", # lt orange
 					 "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1",
 					 "darkturquoise", "green1", "yellow4", "brown","#ED1FBC","#FF001E",
 					 "#FF0026","#FF0033","#FF001E","#FF0009","#FF0011","#FF003C","#FF0044","#FF0051","#FF0059",
 					 "#FF0066","#FF006F","#FF0077","#FF0084","#FF0099","#FF008C","#FF00A2","#FF00AA","#FF00B7",
 					 "#FF00BF","#FF00CC","#00CCFF","#00DDFF","#00D5FF","#00B7FF","#00A2FF","#0099FF","#0084FF",
 					 "#0077FF","#0066FF","#0059FF","#0044FF","#003CFF","#0033FF","#0009FF","#0011FF","#001EFF",
 					 "#BB00FF")
 	c22
 }

# Format a range of Age in a nice, easy-to-read way
formatAgeText <- function(Age) {
	if (min(Age) == max(Age)) {
		return(min(Age))
	} else {
		return(paste(Age, collapse = " - "))	
	}
}








formatFDRText <- function(atac_FDR) {
  if (min(atac_FDR) == max(atac_FDR)) {
    return(min(atac_FDR))
  } else {
    return(paste(atac_FDR, collapse = " - "))	
  }
}

