##---------------------------------------------------------------------------------##
## EPA_plot4.R is 4/6 scripts that visually explore data re: PM2.5 Emissions       ##
## using the EPAs National Emissions Inventory Website.                            ##
## This particular plot visually compares all the PM2.5 monitoring sources in the  ##
## the years 1999, 2002, 2005 and 2008 to see the trend in emissions related to    ##
## coal combustion sources nationally                                              ##                                                ##                
## 1. Downloads a dataset and unzips it if needed                                  ##          
## 2. loads a data.table from the summarySCC_PM25.rds file in the                  ##
##     exdata_data_NEI_data directory                                              ##
## 3. merges the Data table and SCC code tables and then subsets data where 
## coal or coal related combustion is in the fuel source short name and it also    ##
## converts type to a factor
## 4. creates a a jitterplot with linear model smoother with color coded types     ##                                                                                                                                                            
## 5. prints the plot the screen and copies it to a png file in the root directory ##                                                                                      
##---------------------------------------------------------------------------------##

EPA_plot4 <- function(){
        
        ##install.packages(“plyr”)
        #install.packages("ggplot2")
        library(plyr)
        library(ggplot2)
        library(MASS)
        
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
        Code <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds") ## 163 KB
        print("Data Loaded.")
        print("Summarizing Data...")
        
        ##------------------3. MERGE AND SUBSET DATA.TABLE AND MOD FOR PLOT-----------------##
        ## make a temporary dataset called modData
        ## select a subset of the dataset where fips == 24150 (baltimore city)
        ## and convert type to a factor
        
        print("Merging Data... This could take several minutes")        
        data.merged<- merge(Code, Data, by.y="SCC")
        print("Subsetting Data...")
        ##grep for anything coal related, including lignite, coal-related processing and maufacturing practices and charcoal
        data.merged.subset <- data.merged[grep("coal|Coal|Lignite|lignite", data.merged$Short.Name, perl=TRUE),]
        data.merged.subset$type <- as.factor(data.merged.subset$type) ## type of source, POINT or NONPOINT
        data.merged.subset$Coal.Type <- as.factor(data.merged.subset$SCC.Level.Three) ## coal fuel sources
        print("Data Processed.")
        print("Plotting Data...")
        
        ## --------------PRODUCE THE PLOT USING QPLOT from GGPLOT2-------------------##
        ## use ggplot2 qplot to make a scatterplot with robust linear model fitting line 
        ## for each of the four facets of factor types
        ## Emissions is transformed to a log base 10 function
        ## and 0 are subbed for -inf values in order to compress the viewport
        
        ggplot(aes(y = Emissions, x = year, color=type), data = data.merged.subset) 
                + geom_jitter() + 
                ggtitle("EPA PM2.5 Emission related to Coal Combustion for 1999-2005") 
                + coord_cartesian(ylim = c(0, 500)) 
                + geom_smooth(method="lm")
        
        ## --------------COPY SCREEN DEVICE to PNG-------------------##
        print("Copying Plot to .png")
        dev.copy(png, file = "EPA_plot4.png", width=1080, height=568) ## copy from the screen device to a PNG file
        print("PNG created.")
        print("closing device...")
        dev.off() ## Don't forget to close the PNG device!
        print("Script Completed")
}