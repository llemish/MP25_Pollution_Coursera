plot3 <- function() {
        # Read file summarySCC_PM25.rds
        NEI <- readRDS('./data/summarySCC_PM25.rds')
        
        # Subset Baltimore measurements by flps
        NEI_Baltimore <- subset(NEI, NEI$fips == "24510")

        # Use tapply for count sum by year and type
        emission <- tapply(NEI_Baltimore$Emissions, list(NEI_Baltimore$year, NEI_Baltimore$type), sum)
        
        print(emission)
}