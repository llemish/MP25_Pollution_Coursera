plot2 <- function() {
    # Read file summarySCC_PM25.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # Split data frame by year
    NEI_by_year <- split(NEI, NEI$year)
    
    # Get a vector of included years
    years <- names(NEI_by_year)
    
    # Prepare vactor that will hold total emission for every year
    t_emission = rep(0, length(years))
    
    for (i in 1:length(years)) {
        
        # subset data frame to choose only Baltimore City (fips == "24510")
        NEI_Baltimore <- subset(NEI_by_year[[i]], NEI_by_year[[i]]$fips == "24510")
        t_emission[i] <- sum(NEI_Baltimore$Emissions, na.rm = T)
    }
    
    png(filename = "plot2.png")
    
    plot(years, t_emission, col = "red", ylab = "Total Emission (tones)")
    
    dev.off()
    
}