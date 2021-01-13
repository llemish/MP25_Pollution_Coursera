plot1 <- function() {
    # Read file Source_Classification_Code.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # Split data frame by year
    NEI_by_year <- split(NEI, NEI$year)
    
    # Get a vector of included years
    years <- names(NEI_by_year)
    
    # Prepare vector that will hold total emission for every year
    t_emisiion = rep(0, length(years))
    
    # Sum total emission for every year
    for (i in 1:length(years)) {
        t_emisiion[i] <- sum(NEI_by_year[[i]]$Emissions, na.rm = T)
    }
    
    png(filename = "plot1.png")
    
    plot(years, t_emisiion, col = "red", ylab = "Total Emission (tones)")
    
    dev.off()

}