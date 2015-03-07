## This function has 1 major branch.  Parameter is sent - to use saved transformed data or get fresh data
## from the internet source.  Defaults to "N"
## If the parameter is N - get the file from the internet, unzip, and transform the file.  Temporary file is saved
main3 <- function(useSaveData = "N"){
        
        if(useSaveData != "Y"){
                
                dest <- paste(getwd(),"/data/","HouseHoldPowerConsumption.zip", sep="")        
                getfile(dest)
                tdata <- data_transform(dest)
                
        } else {
                
                tmpfile <- paste(getwd(),"/data/tmphousehold_power_consumption_transformed.csv",sep="")
                
                ## Check if the temporary file actually exists, if it doesn't return error
                if(!file.exists(tmpfile)) return("No temporary file available, re-run with useSaveData = 'N'")
                
                #read the data from file
                tdata <- read.csv(tmpfile)
                
                #Data transformations required from file load
                tdata$Date <- as.Date(tdata$Date)
                tdata$Time <- strptime(x, "%Y-%m-%d %H:%M:%S")
        }
        
        ##This function creates a .png file with a specific resolution and filename "plot1.png" in the working directory
        plot3()
        
        ##returning the transformed data.frame - in case futher manipulation is required
        tdata
        
}

## This function plots the first histogram
plot3 <- function(){
        
        ## if the plot has been run before, delete it and re-run it
        if(file.exists("plot3.png")) {file.remove("plot3.png")}
        
        ## png device setup - filename, resolution, and resolution uom appear to be the only parameters required
        ## to produce the desired output
        png(filename = "plot3.png", width = 480, height = 480, units = "px")
        
        plot(tdata$Time, tdata$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
        lines(tdata$Time, tdata$Sub_metering_2, col = "red")
        lines(tdata$Time, tdata$Sub_metering_1, col = "black")
        lines(tdata$Time, tdata$Sub_metering_3, col = "blue")
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1), lwd=c(2,2), col=c("black", "red", "blue"))
        
        ## Save the file - all hist functions complete
        dev.off()
        
}

## getfile() - download the original data from the source
## if the file already exists - delete it.
getfile <- function(dest) {
        
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if(file.exists(dest)){
                file.remove(dest)
        }
        download.file(fileurl, destfile = dest)
        
}

## extract the zip file saved from source in getfile()
## load the uncompressed file to memory, transform date and time columns, filter it
## save filtered file to disk
## remove from memory the original file
data_transform <- function(zipfile) {
        unzip(zipfile, exdir="./data")
        
        cClass <- (c("character", "character", rep("numeric",7)))
        ## used a shorter file to get transformations set up
        ##filename <- paste(getwd(),"/data/","household_power_consumption_short.txt", sep="")
        filename <- paste(getwd(),"/data/","household_power_consumption.txt", sep="")
        rawdata <- read.table(filename, header=TRUE, sep=";", na.strings="?", colClasses = cClass)
        
        ## NOTE the capital Y in the date format - indicates 4 digit year (2006)
        ## lower case y would only bring in the first two character or call the year 2020
        rawdata[,1] <- as.Date(rawdata[,1],"%d/%m/%Y")
        
        ## tdata = "transformed data"
        tdata <- rawdata[rawdata$Date ==  "2007-02-01" | rawdata$Date == "2007-02-02",]
        tdata$Time <- paste(tdata$Date, tdata$Time)
        tdata$Time <- strptime(x, "%Y-%m-%d %H:%M:%S")
        
        ## create a temporary file with the filtered data.
        tmpFileName <- paste(getwd(),"/data/tmphousehold_power_consumption_transformed.csv",sep="")
        if(file.exists(tmpFileName)) { file.remove(tmpFileName)}
        write.csv(tdata,tmpFileName)
        rm(rawdata) ##removing from memory the original 2 million + record data.frame
        
        return(tdata)
}