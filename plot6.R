plot6 <- function() {
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
    
    # Subset measurements for Los Angeles
    NEI_Vehicle_LA <- subset(NEI_Vehicle, NEI_Vehicle$fips == "06037")
    
    emission_b <- tapply(NEI_Vehicle_B$Emissions, NEI_Vehicle_B$year, sum)
    
    emission_la <- tapply(NEI_Vehicle_LA$Emissions, NEI_Vehicle_LA$year, sum)

    png(filename = "plot6.png")
    
    plot(names(emission_b), emission_b, log = "y", ylim = c(5, 5000), col = "red", ylab = "Motor Vehicle Emission (tones)")
    
    axis(2, c(0, 50, 500, 5000))
    
    points(names(emission_la), emission_la, col = "blue", pch = 4)
    
    lines(names(emission_b), emission_b, col = 'red', type = 'l')
    lines(names(emission_la), emission_la, col = 'blue', type = 'l')
    
    legend('bottomleft', c('Baltimore', 'Los Angeles'), col = c('red', 'blue'), pch = c(1, 4))
    
    dev.off()
}