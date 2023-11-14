####################
# HEXAGON MAPPINGS #
####################

rm(list = ls())

library(R6)
library(ggplot2)
library(readr)

domain <- R6Class(classname = "Domain",
                  public = list(
                    data1 = NA,
                    resolution1 = NA,
                    p = NA,
                    nbins = NA,
                    w = NA,
                    n_bins = NA,
                    initialize = function(data1, resolution1){
                      self$data1 <- data1
                      self$resolution1 <- resolution1
                    },
                    lims_calc = function(){
                      
                      xb <- ceiling(max(self$data1$x)) + 1
                      
                      xa <- floor(min(self$data1$x)) - 1
                      
                      yb <- ceiling(max(self$data1$y)) + 1
                      
                      ya <- floor(min(self$data1$y)) - 1
                      
                      diff_x <- xb - xa
                      
                      diff_y <- yb - ya 
                      
                      center_x <- (xb + xa)/2
                      
                      center_y <- (yb + ya)/2
                      
                      xa_lim <- center_x - 1/2 * max(diff_x, diff_y)
                      xb_lim <- center_x + 1/2 * max(diff_x, diff_y)
                    
                      ya_lim <- center_y - 1/2 * max(diff_x, diff_y)
                      yb_lim <- center_y + 1/2 * max(diff_x, diff_y)
                      
                      self$w <- list()
                      self$w$x <- c(xa_lim, xb_lim)
                      self$w$y <- c(ya_lim, yb_lim)
                      
                      self$n_bins <- 7/240 * (xb_lim - xa_lim) * (yb_lim - ya_lim) + 1.47
                      
                    },
                    plotDomain = function(resolutionIndex, col1, border1, ...){
                      
                      self$lims_calc()
                      
                      if(is.null(dev.list())){
                        self$p <- NA
                      }
                      
                      scale <- (1/self$resolution1[resolutionIndex])
                      
                      if(length(self$p) == 1){
                        
                        self$p <- ggplot(self$data1, aes(x, y)) +
                          scale_x_continuous(limits = self$w$x) +
                          scale_y_continuous(limits = self$w$y) +
                          coord_equal()
                        
                        self$p <- self$p + 
                          geom_hex(bins = self$n_bins * scale, col = border1, fill = col1) +
                          theme(panel.background = element_blank(), legend.position = "none") +
                          theme(panel.border = element_rect(color = "black",
                                                            fill = NA,
                                                            size = 0.5))  
                      }else{
                        
                        self$p <- self$p + geom_hex(bins = self$n_bins * scale, col = border1, fill = col1) 
                        
                      }
                      
                      print(self$p)
                      return(invisible(self))
                    },
                    plotPoints = function(cex, ...){
                      
                      self$lims_calc()
                      
                      if(is.null(dev.list())){
                        self$p <- NA
                      }
                      
                      if(length(self$p) == 1){
                        
                        self$p <- ggplot(self$data1, aes(x, y)) +
                          scale_x_continuous(limits = self$w$x) +
                          scale_y_continuous(limits = self$w$y) + 
                          coord_equal()
                        
                        self$p <- self$p + 
                          geom_point(alpha = cex, ...)  +
                          theme(panel.background = element_blank(), legend.position = "none") +
                          theme(panel.border = element_rect(color = "black",
                                                            fill = NA,
                                                            size = 0.5))  
                      }else{
                        
                        self$p <- self$p + geom_point(...)
                        
                      }
                      
                      print(self$p)
                      return(invisible(self))
                      
                    }
                  )
)


lighter <- function(color, percentLighter = 50) {
  r <- strtoi(paste0("0x", substr(color, 2, 3))) / 255
  g <- strtoi(paste0("0x", substr(color, 4, 5))) / 255
  b <- strtoi(paste0("0x", substr(color, 6, 7))) / 255
  a <- strtoi(paste0("0x", substr(color, 8, 9))) / 255
  a <- a * percentLighter / 100
  rgb(r, g, b, a)
}



#################
# FIRST EXAMPLE #
#################

test_data_00 <- read_csv("C:/Users/kacpe/Desktop/Adv R/1st project/test_data_00.csv")

data1 <- test_data_00

a <- domain$new(data1 = data1, resolution1 = c(0.5, 0.7, 1))

a$plotDomain(resolutionIndex = 3, border = rgb(107/255, 9/255, 91/255, .5), col = rgb(107/255, 9/255, 91/255, .4))

a$plotDomain(resolutionIndex = 2, border = NA, col = rgb(166/255, 123/255, 91/255, 0.9))

a$plotDomain(resolutionIndex = 1, border = rgb(242/255, 189/255, 205/255, .5), col = rgb(242/255, 189/255, 205/255, .9))

a$plotPoints(pch = 20, col = rgb(1, 0, 0, 1), cex = 0.5)

###################################
# FIRST EXAMPLE (SECOND APPROACH) #
###################################

graphics.off()

a$plotPoints(pch = 20, cex = .5, col = "red")$
  plotDomain(resolutionIndex = 3, border = rgb(0, 1, 1, .3), col = rgb(0, 1, 1, .1))$
  plotDomain(resolutionIndex = 2, border = rgb(1, 0, 1, .3), col = rgb(1, 0, 1, .1))$
  plotDomain(resolutionIndex = 1, border = rgb(0, 1, 1, .5), col = rgb(0, 1, 1, .3))
  
##################
# SECOND EXAMPLE #
##################

d <- read.csv(file = "C:/Users/kacpe/Desktop/Adv R/1st project/test_data_01.csv")

data1 <- d

a <- domain$new(data1 = data1, resolution = c(0.3, 0.5, 1))

### Colors
col1 <- rgb(1, 0, 0, .4)
col2 <- rgb(1, 0, 1, .4)
col3 <- rgb(0, 0, 1, .4)
col4 <- "#FFF300"
  
### Closing all graphical devices
graphics.off()

### Creating a graph
a$plotDomain(resolutionIndex = 3, col = lighter(col1, 10), border = col1)
a$plotDomain(resolutionIndex = 2, col = lighter(col2, 50), border = col2)
a$plotDomain(resolutionIndex = 1, col = col1, border = NA)
a$plotPoints(pch = 20, cex = 0.5, col = col4)

graphics.off()

a$plotDomain(resolutionIndex = 3, border = col1, col = lighter(col1, 10))$
  plotDomain(resolutionIndex = 2, border = col2, col = lighter(col2, 50))$
  plotDomain(resolutionIndex = 1, border = NA, col = col1)$
  plotPoints(pch = 20, cex = .5, col = col4)


#################
# THIRD EXAMPLE #
#################

d <- read.csv(file = "C:/Users/kacpe/Desktop/Adv R/1st project/test_data_02.csv")

### Creating an object
a <- domain$new(data = d, resolution = c(0.1, 0.3, 1))

### Colors
col1 <- rgb(0, 1, 1, .4)
col2 <- rgb(1, 0, 1, .4)
col3 <- rgb(0, 0, 1, .4)
col4 <- rgb(0, 0, 1, 1)

### Closing all graphical devices
graphics.off()

### Creating a graph
a$plotDomain(resolutionIndex = 3, col = lighter(col1, 10), border = col1)
a$plotDomain(resolutionIndex = 2, col = lighter(col2, 50), border = col2)
a$plotDomain(resolutionIndex = 1, col = lighter(col3, 50), border = lighter(col3, 20))
a$plotPoints(pch = 20, size = 0.5, col = col4)

