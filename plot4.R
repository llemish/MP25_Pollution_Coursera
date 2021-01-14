plot4 <- function() {
    # Read file summarySCC_PM25.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # Read file Source_Classification_Code.rds
    SCC <- readRDS("./data/Source_Classification_Code.rds")
    
    # Subset SCC records that include fuel combustion
    SCC_FC <- SCC[grep('Fuel Comb', SCC$EI.Sector),]
    # Then subset results that include coal
    SCC_FC_Coal <- SCC_FC[grep('[Cc]oal', SCC_FC$EI.Sector),]
    
    # Select vector of codes
    SCC_Codes <- as.vector(SCC_FC_Coal$SCC)
    
    # Subset summary set for SCC in SCC_Codes
    NEI_Coal <- NEI[NEI$SCC %in% SCC_Codes,]
    
    # Split data frame by year
    NEI_by_year <- split(NEI_Coal, NEI_Coal$year)
    
    # Get a vector of included years
    years <- names(NEI_by_year)
    
    # Prepare vector that will hold total emission for every year
    t_emisiion = rep(0, length(years))
    
    # Sum total emission for every year
    for (i in 1:length(years)) {
        t_emisiion[i] <- sum(NEI_by_year[[i]]$Emissions, na.rm = T)
    }
    
    png(filename = "plot4.png")
    
    plot(years, t_emisiion, col = "red", ylab = "Total Emission (tones)")
    
    dev.off()
}