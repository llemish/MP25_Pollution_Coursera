plot3 <- function() {
        # Read file summarySCC_PM25.rds
        NEI <- readRDS('.//data//summarySCC_PM25.rds')
        
        # Subset Baltimore measurements by flps
        NEI_Baltimore <- subset(NEI, NEI$fips == "24510")

        # Use tapply for count sum by year and type
        emission <- tapply(NEI_Baltimore$Emissions, list(NEI_Baltimore$year, NEI_Baltimore$type), sum)
        
        emission <- as.data.frame(emission)
        
        emission2 <- data.frame(em = rep(0, 16), typ = rep(0, 16), year = rep(0, 16))
        
        years <- c(1999, 2002, 2005, 2008)
        
        types <- names(emission)
        
        for (i in 1:4){
                j <- (i-1)*4+1
                emission2[j, 1] <- emission[1, i]
                emission2[j+1, 1] <- emission[2, i]
                emission2[j+2, 1] <- emission[3, i]
                emission2[j+3, 1] <- emission[4, i]
                
                emission2[j, 2] <- types[i]
                emission2[j+1, 2] <- types[i]
                emission2[j+2, 2] <- types[i]
                emission2[j+3, 2] <- types[i]
                
                emission2[j, 3] <- years[1]
                emission2[j+1, 3] <- years[2]
                emission2[j+2, 3] <- years[3]
                emission2[j+3, 3] <- years[4]
        }
        
        library(ggplot2)
        
        png(filename = "plot3.png")
        
        print(qplot(year, em, data = emission2, color = typ))
        
        dev.off()
}