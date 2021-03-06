pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    count = 0
    msum = 0
    mean2 = 0
    id1 = id[1]
    
    file_list <- list.files()
  
    for (i in id){
        f <- read.csv(file_list[id1])
        bad <- is.na(f[[pollutant]])
        mean1 <- sum(f[[pollutant]][!bad])
        count = count + length((f[[pollutant]][!bad]))
        msum = msum + mean1
        id1 = id1 + 1
    }
    mean2 = msum/count
    print(mean2)
}