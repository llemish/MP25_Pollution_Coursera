plot6 <- function() {
    # Read file summarySCC_PM25.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # Read file Source_Classification_Code.rds
    SCC <- readRDS("./data/Source_Classification_Code.rds")
    
    # Subset SCC records that include vehicle
    SCC_V <- SCC[grep('[Vv]ehicle', SCC$EI.Sector),]
    
    # Select vector of codes
    SCC_Codes <- as.vector(SCC_V$SCC)
    
    # Subset summary set for SCC in SCC_Codes
    NEI_Vehicle <- NEI[NEI$SCC %in% SCC_Codes,]
    
    # Split data frame by year
    NEI_by_year <- split(NEI_Vehicle, NEI_Vehicle$year)
    
    # Get a vector of included years
    years <- names(NEI_by_year)
    
    # Prepare vector that will hold total emission for every year
    t_emission_B = rep(0, length(years))
    t_emission_L = rep(0, length(years))
    
    # Sum total emission for every year for Baltimore and Los Angeles
    for (i in 1:length(years)) {
        # subset data frame to choose only Baltimore City (fips == "24510")
        NEI_Baltimore <- subset(NEI_by_year[[i]], NEI_by_year[[i]]$fips == "24510")
        t_emission_B[i] <- sum(NEI_Baltimore$Emissions, na.rm = T)
        
        NEI_Los_Angeles <- subset(NEI_by_year[[i]], NEI_by_year[[i]]$fips == "06037")
        t_emission_L[i] <- sum(NEI_Los_Angeles$Emissions, na.rm = T)
    }
    
    png(filename = "plot6.png")
    
    plot(years, t_emission_B, log = "y", ylim = c(5, 5000), col = "red", ylab = "Motor Vehicle Emission (tones)")
    
    axis(2, c(0, 50, 500, 5000))
    
    points(years, t_emission_L, col = "blue", pch = 4)
    
    lines(years, t_emission_B, col = 'red', type = 'l')
    lines(years, t_emission_L, col = 'blue', type = 'l')
    
    legend('bottomleft', c('Baltimore', 'Los Angeles'), col = c('red', 'blue'), pch = c(1, 4))
    
    dev.off()
}