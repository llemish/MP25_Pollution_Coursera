plot5 <- function() {
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
    t_emisiion = rep(0, length(years))
    
    # Sum total emission for every year
    for (i in 1:length(years)) {
        # subset data frame to choose only Baltimore City (fips == "24510")
        NEI_Baltimore <- subset(NEI_by_year[[i]], NEI_by_year[[i]]$fips == "24510")
        t_emisiion[i] <- sum(NEI_Baltimore$Emissions, na.rm = T)
    }
    
    png(filename = "plot5.png")
    
    plot(years, t_emisiion, col = "red", ylab = "Total Emission (tones)")
    
    dev.off()
}