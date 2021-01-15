plot1 <- function() {
    # Read file summarySCC_PM25.rds
    NEI <- readRDS('./data/summarySCC_PM25.rds')
    
    # count total emission by year
    emission <- tapply(NEI$Emissions, NEI$year, sum)
    
    png(filename = "plot1.png")
    
    plot(names(emission), emission, col = "red", ylab = "Total Emission (tones)")
    lines(names(emission), emission, col = 'red', type = 'l')
    
    dev.off()

}