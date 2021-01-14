plot3 <- function() {
        # Read file summarySCC_PM25.rds
        NEI <- readRDS('.//data//summarySCC_PM25.rds')
        
        # Split data frame by year
        NEI_by_year <- split(NEI, NEI$year)
        
        # Get a vector of included years
        years <- names(NEI_by_year)
        
        # Prepare vactor that will hold total emission for every year and every type
        t_emission <- data.frame()
        
        for (i in 1:length(years)) {
                
                # subset data frame to choose only Baltimore City (fips == "24510")
                NEI_Baltimore <- subset(NEI_by_year[[i]], NEI_by_year[[i]]$fips == "24510")
                
                NEI_Baltimore$type <- as.factor(NEI_Baltimore$type)
                
                NEI_by_type <- split(NEI_Baltimore, NEI_Baltimore$type)
                
                types <- names(NEI_by_type)
                
                for (j in 1:length(types)) {
                        t_emission[i, j] <- sum(NEI_by_type[types[j]][[1]]$Emissions, na.rm = T)        
                }
                
        }
        
        qplot(V1, data = t_emission)
        
        # png(filename = "plot3.png")
        
        # plot(years, t_emission, col = "red", ylab = "Total Emission (tones)")
        
        # dev.off()
        
}