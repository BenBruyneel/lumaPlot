# --------------------------------------------------------------------------------------------
# basic parameters

dirbz  <- "V:/R/lumaPlot/"
filebz <- "rawdata.csv"

bla <- 20.76      # "blanco" in calculation

sepbz <- ";"

# plotting

doplot        <- TRUE   # TRUE if you want to plots, FALSE if you don't want plots!!
startplot     <- 0      # first spot to plot
endplot       <- 10     # last spot to plot
maxDatapoints <- 30     # maximum datapoint to use
tofile        <- TRUE  # TRUE if you want files, FALSE if you don't want files

# slope calculations

begintime <- 1  # both in hours, can be 3, 3.14 , 60 etc etc
endtime   <- 4
slopefile <- "rawdata-slopes.csv"

# plot with slopes

linplot      <- FALSE   # TRUE if you want to plots, TRUE if you don't want plots!!
startlin     <- 0       # first spot to plot
endlin       <- 10      # last spot to plot
maxDatalin   <- 30      # maximum datapoint to use
linefile     <- FALSE   # TRUE if you want files, FALSE if you don't want files
begintimelin <- 1       # both in hours, can be 3, 3.14 , 60 etc etc
endtimelin   <- 4

# --------------------------------------------------------------------------------------------
# basics --> always needs to be done

require(dplyr)

setwd(dirbz)

bz <- read.csv(filebz, header = TRUE, sep = sepbz, stringsAsFactors = FALSE)
bz <- as_tibble(bz)

bz1 <- bz %>% 
            group_by(posIdx) %>%                          # group per spot
            mutate(fileTime = fileTime/3600) %>%          # covert time to hours
            mutate(calcArea = IntDen - (Area * bla)) %>%  # calculate data
            filter(imgIdx < maxDatapoints)                # filter out datapoints higher

# --------------------------------------------------------------------------------------------
# plotting

if (doplot) {
            for (i in startplot:endplot){
                 if (tofile){
                    jpeg(paste("spot",i,".jpg",sep = "" ))
                 }
                 with(bz1[bz1$posIdx == i,], plot(x = fileTime, y = calcArea))
                 title(main = paste("spot",i))
                 if (tofile){
                   dev.off()
                 }
                 
            }
}

# --------------------------------------------------------------------------------------------
# calculate slopes

bzsl <- bz1 %>%
              filter((fileTime >= begintime) & (fileTime <= endtime)) %>%
              summarize(slope = summary(lm(calcArea ~ fileTime))$coeff[2,1],
                        r2 = summary(lm(calcArea ~ fileTime))$r.squared)

write.table(bzsl, slopefile, sep = ";", row.names = FALSE, col.names = TRUE)

# --------------------------------------------------------------------------------------------
# plot with slopes

if (linplot) {
    bz2 <- bz %>% 
      group_by(posIdx) %>%                          # group per spot
      mutate(fileTime = fileTime/3600) %>%          # covert time to hours
      mutate(calcArea = IntDen - (Area * bla)) %>%  # calculate data
      filter(imgIdx < maxDatalin)                   # filter out datapoints higher
  
    bzsl2 <- bz2 %>%
    filter((fileTime >= begintimelin) & (fileTime <= endtimelin)) %>%
    summarize(lmmod = list(lm(calcArea ~ fileTime)))
    
    for (i in startlin:endlin){
      if (linefile){
        jpeg(paste("linspot",i,".jpg",sep = "" ))
      }
      with(bz2[bz2$posIdx == i,], plot(x = fileTime, y = calcArea))
      title(main = paste("spot",i))
      abline(bzsl2$lmmod[[i+1]], lw = 2)
      if (linefile){
        dev.off()
      }
    }
}

# --------------------------------------------------------------------------------------------