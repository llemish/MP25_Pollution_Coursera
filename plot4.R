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
    
    emission <- tapply(NEI_Coal$Emissions, NEI_Coal$year, sum)
    
    png(filename = "plot4.png")

    plot(names(emission), emission, col = "red", ylab = "Total Emission from Coal combustion (tones)")
    lines(names(emission), emission, col = 'red', type = 'l')

    dev.off()
}