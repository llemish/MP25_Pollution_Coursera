plot2 <- function() {
    # Read file summarySCC_PM25.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # Subset Baltimore measurements by flps
    NEI_Baltimore <- subset(NEI, NEI$fips == "24510")
    
    # Use tapply for count sum by year
    emission <- tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, sum)

    png(filename = "plot2.png")

    plot(names(emission), emission, col = "red", ylab = "Total Emission in Baltimore (tones)")
    lines(names(emission), emission, col = 'red', type = 'l')

    dev.off()
    
}