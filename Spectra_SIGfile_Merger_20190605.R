# list all the .sig files in the directory
sig.files <- list.files("C:/Users/Aaron Kamoske/Dropbox/Dissertation/Data/RAW_DATA/SPECTRAL_DATA/TALL/2018", 
                        pattern = c("\\.sig$"),
                        full.names = TRUE)

#loop through all the files and put them into a matrix
for (i in 1:length(sig.files)) {
  
  print(i)
  
  #read in the .sig file
  spec <- read.delim(sig.files[i])
  
  #save just the wavelength data, which occurs at row 27
  wavelength.data <- data.frame(spec[27:nrow(spec),])
  
  #we need to pull the sample name so that we use this later
  file.name <- tools::file_path_sans_ext(strsplit(sig.files[i], 
                                                  "/")[[1]][11])
  id.name <- strsplit(file.name, "_")[[1]][1]
  samp.name <- strsplit(file.name, "_")[[1]][2]
  
  #the first time through we want to add the header data
  if (i == 1){
    
    #create an empty matrix to save all the data in
    spectra.matrix <- matrix(ncol = nrow(wavelength.data) + 2, 
                             nrow = length(sig.files) + 1)
    
    #first we need to populate the first row of the matrix with header data
    spectra.matrix[1,1] <- "ID"
    spectra.matrix[1,2] <- "SAMPLE"
    
    #lets add in the wavelength names too
    for(z in 3:ncol(spectra.matrix)) {
      
      #find the wavelength - we subtract 2 because of starting at the 3rd column
      wl <- as.numeric(strsplit(as.character(wavelength.data[z-2,]), " ")[[1]][1])
      
      #create the header name
      wl.name <- paste0("nm",wl)
      
      #add the name to the spot in the matrix
      spectra.matrix[1,z] <- wl.name
      
    }
    
    #now that the header is done lets add the data to the first row
    spectra.matrix[i + 1, 1] <- id.name
    spectra.matrix[i + 1, 2] <- samp.name
    
    #lets add in all the wavelength information
    for (q in 3:ncol(spectra.matrix)) {
      
      #add the spectral data
      spectra.matrix[i + 1, q] <- as.numeric(strsplit(as.character(wavelength.data[q-2,]), " ")[[1]][7])
      
    }
  } else {
    
    #lets add in all the wavelength information
    for (q in 3:ncol(spectra.matrix)) {
      
      #add the id and sample name to the matrix
      spectra.matrix[i + 1, 1] <- id.name
      spectra.matrix[i + 1, 2] <- samp.name
      
      #add the spectral data
      spectra.matrix[i + 1, q] <- as.numeric(strsplit(as.character(wavelength.data[q-2,]), " ")[[1]][7])
      
    }
  }
}

#lets write this as a csv with no column or row names and using a "," as a seperator
write.table(spectra.matrix, 
            file = "C:/Users/Aaron Kamoske/Dropbox/Dissertation/Data/RAW_DATA/SPECTRAL_DATA/merged_csv_files/TALL2018_merged_spectra.csv",
            sep = ",",
            col.names = FALSE,
            row.names = FALSE)

#read in csv file we just created so that is in the correct format
spectra.csv <- read.csv("C:/Users/Aaron Kamoske/Dropbox/Dissertation/Data/RAW_DATA/SPECTRAL_DATA/merged_csv_files/TALL2018_merged_spectra.csv")

#lets take the average off the spectral bands for each sample
spec.avg <- aggregate(.~ID, FUN = mean, data = spectra.csv)

#lets remove the sample number column, since it is not needed any longer
spec.avg$SAMPLE <- NULL

#lets write this as a csv in the appropriate directory
write.table(spec.avg, 
            file = "C:/Users/Aaron Kamoske/Dropbox/Dissertation/Data/PROCESSED_DATA/AVG_SPECTRA/TALL2018_Average_Spectra_20190605.csv",
            sep = ",",
            row.names = FALSE)








