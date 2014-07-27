##---------------------------------------------------------------------------------##
## EPA_plot5.R is 5/6 scripts that visually explore data re: PM2.5 Emissions       ##
## using the EPAs National Emissions Inventory Website.                            ##
## This particular plot visually compares the on-road PM2.5 monitoring sources in  ##
## the years 1999, 2002, 2005 and 2008 to see if emissions are decreasing in       ##
## Baltimore City, Maryland                                                        ##                
## 1. Downloads a dataset and unzips it if needed                                  ##          
## 2. loads a data.table from the summarySCC_PM25.rds file in the                  ##
##     exdata_data_NEI_data directory                                              ##
## 3. uses split-apply-combine approach to sum all columns both by year and type   ##
##    and fips (24510 = baltimore city)
## 4. creates a line plot of the on-road source type vs time                       ##                                                                                                                                                            
## 5. prints the plot the screen and copies it to a png file in the root directory ##                                                                                      
##---------------------------------------------------------------------------------##

EPA_plot5 <- function(){
        
        ##install.packages(“plyr”)
        library(plyr)
        
        ##--------------1. CHECK FOR TXT FILE OR ZIP FILE ELSE DOWNLOAD AND UNZIP-----------------------##
        ## check for Dataset dir in root level folder, if found proceed
        ## if not found check for dataset zip, unzip and proceed
        ## and if zip not found then download the dataset from the internet, unzip and proceed
        ##https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
        
        if(file.exists("exdata_data_NEI_data")){
                print("Dataset already downloaded and unzipped")
                print("Loading Data...")
        }else if(file.exists("exdata_data_NEI_data.zip")){
                print("Dataset downloaded. Now Unzipping...")
                unzip("./exdata_data_NEI_data.zip")
                print("Completed.")
                print("Loading Data...")
        }else if(!file.exists("exdata_data_NEI_data.zip")){
                print("Downloading and unzipping dataset...")
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata_data_NEI_data.zip")
                unzip("./exdata_data_NEI_data.zip")
                print("Completed.")
                print("Loading Data...")
        }
        
        ##--------------2. OPENING DATA.TABLES-----------------------##
        ## open 30.5MB file named summarySCC_PM25.rds
        ## it is a file with 6497651 obs and 6 variable
        ## This first line will likely take a few seconds. Be patient!
        print("Opening Data... this may take a minute of more.")
        Data <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds") ## 30.5 MB
        ##Code <- readRDS("Source_Classification_Code.rds") ## 163 KB
        print("Data Loaded.")
        print("Summarizing Data...")
        
        ##------------------3. SUMMARISE DATA.TABLE FOR PLOT-----------------##
        ## use the split.apply.combine approach to split the data frame by year of measurement
        ## sum the emissions for each type and the total per year for only fips=24510 (baltimore city)
        ## and combine that to create a new table
        
        data.summed.line<-ddply(Data, "year", summarise, 
                                total=sum(Emissions[which(fips==24510)]), 
                                point = sum(Emissions[which(type=="POINT" & fips==24510)]), 
                                nonpoint = sum(Emissions[which(type=="NONPOINT" & fips==24510)]),  
                                onroad = sum(Emissions[which(type=="ON-ROAD" & fips==24510)]), 
                                nonroad = sum(Emissions[which(type=="NON-ROAD" & fips==24510)])
                                )
        
        ##Barplot
        ##data.summed.bar<-ddply(Data, "type", summarise, 
        ##                  year.1999 = sum(Emissions[which(year==1999)]), 
        ##                  year.2002 = sum(Emissions[which(year==2002)]), 
        ##                  year.2005 = sum(Emissions[which(year==2005)]), 
        ##                  year.2008 = sum(Emissions[which(year==2008)]))
        
        print("Data Processed.")
        print("Plotting Data...")
        
        ## --------------PRODUCE THE PLOT USING BASE-------------------##
        ## convert the data.table into a matrix for the barplot 
        ##box.matrix <- as.matrix(data.summed[1:4,2:5])
        par(bg="transparent", mar=c(5,5,4,2)) ## set the background to transparent
        ##barplot(box.matrix, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red", 
        with(data.summed.line, plot(y=data.summed.line$onroad, 
                          x=data.summed.line$year, 
                          ylab="PM2.5 Emissions (in tons)", 
                          xlab="Year", xlim=c(1998, 2009), 
                          main="PM2.5 Emission Totals for On-Road Sources in Baltimore City, 1999-2008", 
                          ylim=1.1*c(0, max(data.summed.line$onroad)), 
                          type="o", 
                          col="#DF9EDF",
                          lwd=2.5)
             )
        
        ##legend("topright", title="Emission Sources", legend=c("Sum Total", "POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"), 
        ##       xjust=1, cex=0.8, bg="transparent", lwd=c(2.5,1,1,1,1), lty=c(1,2,2,2,2), bty="n", col=c("#82E153", "#63CFE5", "#5BE2A3", "#DF9EDF", "#DDCC43"))
        
        ## --------------COPY SCREEN DEVICE to PNG-------------------##
        print("Copying Plot to .png")
        dev.copy(png, file = "EPA_plot5.png", width=720, height=480) ## copy from the screen device to a PNG file
        print("PNG created.")
        print("closing device...")
        dev.off() ## Don't forget to close the PNG device!
        print("Script Completed")
}