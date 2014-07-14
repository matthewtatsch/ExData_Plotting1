##   plot3 loads and reads data from the "electric power consumption" dataset 
##   from the UC Irvine Machine Learning Repository, and creates a plot in line 
##   with instructions at https://github.com/rdpeng/ExData_Plotting1/blob/master/README.md.  

##   Assumes source dataset takes the following format:
##   Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
##   16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000
##   16/12/2006;17:25:00;5.360;0.436;233.630;23.000;0.000;1.000;16.000
##   16/12/2006;17:26:00;5.374;0.498;233.290;23.000;0.000;2.000;17.000
##   [...]

##   getZipData downloads and unzips a dataset in csv format from a source
##   path (a URL) and returns the data in a data frame.
getZipCsv <- function(url, datadir) {
     
     dir.create(datadir)               
     dUrl <- URLdecode(url)            ## Decode HTML encoded chars in path
     tFile <- tempfile()               ## Create temporary file for 
     
     ## download dataset from source URL
     download.file(dUrl, destfile = tFile, method = "curl")
     
     unzip(tFile, exdir = datadir)     ## Unzip *.zip contents to datadir
     files <- list.files(datadir)      ## Get list of files in source *.zip
     
     ## Source *.zip file should contain exactly one *.txt file.
     ## If not exactly one file, throw error, clean up, and return nothing
     if(length(files) != 1) {      
          writeLines("Error: Downloaded *.zip file contains more than 1 file.\n
                     Only 1 file expected.\n
                     Double-check source.")
          unlink("tFile")
          return()
          ## Else create df, clean up, and return df
     } else {                      
          path <- paste(datadir, "/", files, sep = "")
          df <- read.csv2(path, na.strings = c("?"), dec = ".")
          unlink("tFile")
          df
     }
}

##   plot3 takes a data frame and creates a PNG line plot of Energy Sub Metering
plot3 <- function(df) {
     png(file = "plot3.png")
     
     ## create empty plot with space allocated from sm.Max
     with(df, plot(Time, Sub_metering_1, type = "n",
          xlab = "", ylab = "Energy sub metering"))
     
     ## plot sm values and create legend
     with(df, lines(Time, Sub_metering_1))
     with(df, lines(Time, Sub_metering_2, col = "red"))
     with(df, lines(Time, Sub_metering_3, col = "blue"))
     legend("topright", 
            c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
            lty = c(1,1,1), 
            col = c("black", "red", "blue"))
     
     ##dev.copy(png, file = "plot3.png")
     dev.off()
}

##   Main program execution
require(stats)

## set URL for source data
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
datadir <- "data"  ## dir to store data

## set dates to be used
useDates <- c(as.Date("2007-02-01"),as.Date("2007-02-02"))

elecPowCons <- getZipCsv(url, datadir)                     ## create df from source
elecPowCons$Time <- strptime(paste(elecPowCons$Date, elecPowCons$Time), 
                             "%d/%m/%Y %H:%M:%S")          ## convert Time
elecPowCons$Date <- as.Date(elecPowCons$Date, "%d/%m/%Y")  ## convert Date
elecPowCons <- subset(elecPowCons, Date %in% useDates)     ## subset for useDates
plot3(elecPowCons)                                         ## create plot