plot5 <- function() {
    # Read file summarySCC_PM25.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # Read file Source_Classification_Code.rds
    SCC <- readRDS("./data/Source_Classification_Code.rds")
    
    # Subset SCC records that include vehicle
    SCC_V <- SCC[grep('[Vv]ehicle', SCC$EI.Sector),]
    
    # Select vector of codes
    SCC_Codes <- as.vector(SCC_V$SCC)
    
    # Subset measurements for vehicles
    NEI_Vehicle <- subset(NEI, NEI$SCC %in% SCC_Codes)
    
    # Subset measurements for Baltimore
    NEI_Vehicle_B <- subset(NEI_Vehicle, NEI_Vehicle$fips == "24510")
    
    emission <- tapply(NEI_Vehicle_B$Emissions, NEI_Vehicle_B$year, sum)
    
    png(filename = "plot5.png")

    plot(names(emission), emission, col = "red", ylab = "Vehicle Emission in Baltimore (tones)")
    lines(names(emission), emission, col = 'red', type = 'l')

    dev.off()
}