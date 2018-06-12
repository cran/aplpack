##define [[puticon()]]:##
#275:
puticon <- function(x = 0, y = 0, icon = "", grey.levels = .5,
    icon.cex = 10, color = "red", ..., adj = c(0.5,0.5), xpd = NA){
  
#276:
##check inputs and manage case of showing design in [[puticon()]]:##
pch <- file <- ""
if( is.numeric(icon) && length(icon) == 1 ) pch <- icon
if( is.character(icon) && !is.raster(icon)){
  icon <- icon [1]; h <- nchar(icon) 
  if( substring( icon, h - 3, h ) %in% 
        c(".jpg", ".JPG", "jpeg", "JPEG", ".pnm", ".PNM", ".png", ".PNG") 
  ){ 
    if( substring( icon, 1, 4 ) == "http" ){
      fname <- sub(".*[.]", ".", icon)
      fname <- tempfile("icon-pic", fileext = fname)
      h <- try( utils::download.file(url = icon, destfile = fname) )
      if( "error" %in% class(h) ){ 
        cat("Error in puticon: Download of file", icon, "failed.\n")
        return()
      } 
      on.exit(file.remove(fname))
      icon <- fname
    }
    if( file.exists(icon) ) file <- icon else {
      cat("Error in puticon: reading of file", icon, "failed.\n")
    }
  } 
}
if(missing(x)){
  
#277:
##show design layout of special icon in [[puticon()]]:##
   
#314:
##define [[show.icon.design()]]:##
show.icon.design <- function(icon, reset = TRUE, color = NA, ...){
  old <- par(pin = c(100, 100) / 25.4)
  plot(1:100, xlim = c(0,100), ylim = c(0,100), type = "n", 
       axes = FALSE, xlab = "", ylab = "")
  par(usr = c(0,100,0,100)); axis(1); axis(2)
  color <- ifelse(is.na(color), sample(rainbow(15)), color)
  abline(h = c(0,100), v = c(0,100), lwd = 2, col = "blue", xpd = NA)
  puticon(50, 50, icon, icon.cex = 100, color = color, ...)
  if(reset) par(pin = old)
}
##:define [[show.icon.design()]]##
#:314
 
   if(identical(icon, "")){  # no specific input
     # show internal icon generators:  # find headers of generator
     h <- deparse(puticon); h <- h[grep(paste("\"internal generator", "function"), h) + 1]
     h <- sub("[<][-].*", "", h); h <- gsub(" ", "", h) # remove assignment
     h <- unique(h)
     cat("list of internal generator functions :\n"); cat(h, "\n")
     return(invisible(h))
   }
   if(is.function(icon)){    # icon is a generator function
     show.icon.design(icon, color = "lightblue") 
     cat("arguments of", substitute(icon),":\n")
     print(noquote(h <- args(icon)))
     return()
   }
   if(is.raster(icon)){      # icon is a raster graphics
     show.icon.design("car", color = "green") 
     # icon[] <- (icon - (h <- min(icon))) / (max(icon) - h) # standardization
     rasterImage(icon, 0, 0, 100, 100 * ((h <- dim(icon))[1] / h[2]), 
                 interpolate = FALSE, xpd = xpd)
     return()
   }
   if( is.numeric(icon) ){   # central symbol
     show.icon.design("circle", color = "white") 
     cex <- 25.4 * par()$pin[1] * 0.21 
     points(50, 50, pch = pch, cex = cex, lwd = cex )
     cat("central symbol", icon[1], "will be used as icon; see points()\n")
     return()
   } # now icon is the name of an internal generator or a file name
   if( is.character(icon) ){ # but no raster object 
     if( "" != file ){       # variable file is assigned yet
       show.icon.design("circle", color = "white") 
       
#287:
##read jpeg, png or pnm file in [[puticon()]]:##
  icon <- 0; class(icon) <- "error"
  # JPEG
  if( 0 < length(grep("jp[e]{0,1}g$", file))  || 
      0 < length(grep("JP[E]{0,1}G$", file))){
    if(!"package:jpeg" %in% search()){
      print("puticon() requires package jpeg")
      # library(jpeg, lib.loc = .libPaths()) 
    }
    icon <- try(jpeg::readJPEG(file, native = !TRUE)) #############
  }
  # PNG
  if( 0 < length(grep("png$", file))  || 0 < length(grep("PNG$", file))){
    if(!"package:png" %in% search()){
      print("puticon() requires package png")
      # library(png, lib.loc = .libPaths()) 
    }
    icon <- png::readPNG(file, native = !TRUE)  ##################
  }
  # PNM
  if( 0 < length(grep("pnm$", file))  || 0 < length(grep("PNM$", file))){
    if(!"package:tcltk" %in% search()){
      print("puticon() requires package tcltk")
      # library(tcltk, lib.loc = .libPaths()) 
    }
    
#221:
##define [[get.pnm()]]:##
get.pnm <- function(filename, verbose = FALSE){
  
#222:
##find P* type of pnm file:##
nextline <- scan(filename, what="", n=1)
if( substring(nextline, 1, 1) != "P" || 
    ! ( PType <- substring(nextline, 2, 2) ) %in% as.character(1:6)) 
  return("ERROR: reading pnm file failed: no pnm file")
PType <- as.numeric(PType); if(verbose) cat("PType:", PType); skip <- 0
nextline <- scan(filename, what="", sep="\n", n=1, skip = skip, 
                 blank.lines.skip = FALSE)
nextline <- unlist( strsplit( nextline, "[ \t]"))[-1]
##:find P* type of pnm file##
#:222
  
#223:
##get width of pnm picture:##
#226:
##get next line that is not empty:##
idx <- 20
while(idx > 0 & 0 == length(nextline)){
  idx <- idx - 1; skip <- skip + 1
  nextline <- scan(filename, what="", sep="\n", n=1, skip = skip,
                   blank.lines.skip = FALSE)
  nextline <- sub("[#].*", "", nextline)
  nextline <- unlist( strsplit( nextline, "[ \t]"))
}
if( idx == 0 ) return("ERROR: reading pnm file failed: too many #-lines")
nextline <- as.numeric(nextline)
##:get next line that is not empty##
#:226
width <- nextline[1]; if(verbose) cat("width:",width)
nextline <- nextline[-1]
##:get width of pnm picture##
#:223
  
#224:
##get height of pnm picture:##
if( length(nextline) == 0 ){
  
#226:
##get next line that is not empty:##
idx <- 20
while(idx > 0 & 0 == length(nextline)){
  idx <- idx - 1; skip <- skip + 1
  nextline <- scan(filename, what="", sep="\n", n=1, skip = skip,
                   blank.lines.skip = FALSE)
  nextline <- sub("[#].*", "", nextline)
  nextline <- unlist( strsplit( nextline, "[ \t]"))
}
if( idx == 0 ) return("ERROR: reading pnm file failed: too many #-lines")
nextline <- as.numeric(nextline)
##:get next line that is not empty##
#:226
}
height <- nextline[1]; if(verbose) cat("height:", height)
nextline <- nextline[-1]
##:get height of pnm picture##
#:224
  
#225:
##get colors of pnm picture:##
if(PType == 1 || PType == 4) colors <- 1 else {
  if( length(nextline) == 0 ){
    
#226:
##get next line that is not empty:##
idx <- 20
while(idx > 0 & 0 == length(nextline)){
  idx <- idx - 1; skip <- skip + 1
  nextline <- scan(filename, what="", sep="\n", n=1, skip = skip,
                   blank.lines.skip = FALSE)
  nextline <- sub("[#].*", "", nextline)
  nextline <- unlist( strsplit( nextline, "[ \t]"))
}
if( idx == 0 ) return("ERROR: reading pnm file failed: too many #-lines")
nextline <- as.numeric(nextline)
##:get next line that is not empty##
#:226
  }
  colors <- nextline[1]
}; if(verbose){ cat("colors:", colors); cat("head processed") }
##:get colors of pnm picture##
#:225
  
#228:
##get decpixel of pnm picture:##
if(PType < 4){
  decpixel <- scan(filename, what="", sep="\n", skip = skip+1)
  decpixel <- paste(collapse=" ", decpixel)
  decpixel <- unlist( strsplit( decpixel, " +"))
  decpixel <- as.numeric(decpixel)
  decpixel <- matrix( decpixel, ncol = width, byrow = TRUE)
} else { # P4, P5, P6
  
#227:
##read picture data of P4, P5 or P6 pictures:##
tclcmds <- c('
  # https://de.wikipedia.org/wiki/Portable_Pixmap
  # set fname mm.pnm
  set fname FILENAME       
  set size [ file size $fname ]  
  # puts $size
  set fp [open $fname]
  # http://www.tek-tips.com/viewthread.cfm?qid=1477934  
  fconfigure $fp -translation binary 
  # scan [string range $contents i i] %c strA  
  set binpixel [read $fp $size]
  close $fp
  binary scan $binpixel cu* decpixel
')
tclcmds <- sub("FILENAME", filename, tclcmds)
#require(tcltk)
if( requireNamespace("tcltk") ){ 
  tcltk::.Tcl(tclcmds)
} else { 
  print("Error from get.pnm: tcltk not found!!"); return() 
}
decpixel <- as.character(tclvalue("decpixel"))
##:read picture data of P4, P5 or P6 pictures##
#:227
  decpixel <- unlist( strsplit( decpixel, " "))
  if( PType == 4){
    n.infos <- ceiling(width/8) * height 
    # if(verbose) cat("length(decpixel)", length(decpixel), "n.infos", n.infos)
    decpixel <- decpixel[ -(1 : ( length(decpixel) - n.infos )) ]
    encode <- function(number, base) {
      # simple version of APL-encode / APL-representation "T", pw 10/02
      # "encode" converts the numbers "number" using the radix vector "base"
      n.base <- length(base); result <- matrix(0, length(base), length(number))
      for(i in n.base:1){
        result[i,] <- if(base[i]>0) number %% base[i] else number
        number     <- ifelse(rep(base[i]>0,length(number)),
                             floor(number/base[i]), 0)
      }
      return( if(length(number)==1) result[,1] else result )
    }
    decpixel <- encode(as.numeric(decpixel), rep(2,8))
    decpixel <- matrix(decpixel, nrow = height, byrow = TRUE)[, 1:width]
    # decpixel <- matrix(decpixel, ncol = height, byrow = !TRUE)
    # decpixel <- t(decpixel[(1:width),])
  } else { # P5 or P6
    BigEndian <- colors > 255
    n.infos <- width * height * c(1,3)[1+(PType == 6)] * c(1,2)[1+BigEndian]   
    decpixel <- decpixel[ -(1 : ( length(decpixel) - n.infos )) ]
    if( BigEndian ){ # use the first byte of a pixel only
      decpixel <- matrix(decpixel, ncol = 2, byrow = TRUE)[,1] ### 1 or 2?
    }
    decpixel <- as.numeric(decpixel)
    decpixel <- matrix( decpixel, ncol = width * (1 + 2*(PType == 6)), byrow = TRUE)
  }
  PType <- PType - 3
}
##:get decpixel of pnm picture##
#:228
  
#229:
##define [[decpixel.to.raster()]]:##
decpixel.to.raster <- function(decpixel, PType, width, height, colors){
  HEX <- unlist(strsplit("0123456789ABCDEF",""))
  if(PType < 3){ # black and white or grey -- P1 or P2
    if(PType==1) decpixel <- colors - decpixel
    pixel <- decpixel / colors * 255
    first <- floor( pixel / 16 ); second <- pixel %% 16
    hexpixel <- paste(sep="", HEX[1 + first], HEX[1 + second])
    hexpixel <- paste(sep="", "#", hexpixel, hexpixel, hexpixel)
    hexpixel <- matrix(hexpixel, ncol = width)
  } else {       # colors -- P3
    decpixel <- array(t(decpixel), c(3, width, height))
    if( 255 < colors ) colors <- 255                                 # 160928
    pixel <- decpixel / colors * 255
    first <- floor(pixel / 16); second <- pixel %% 16
    hexpixel <- paste(sep="", HEX[1 + first], HEX[1 + second])
    hexpixel <- array(hexpixel, c(3, width, height))
    hexpixel <- paste(sep="", "#", hexpixel[1,,], hexpixel[2,,], hexpixel[3,,])
    hexpixel <- matrix(hexpixel, ncol = width, byrow = TRUE)
  }
  raster <- hexpixel
}
#table(decpixel.to.raster(a, 6, 724, 561, 65535))
#table(a)
##:define [[decpixel.to.raster()]]##
#:229
  as.raster(decpixel.to.raster(decpixel, PType, width, height, colors))
} #; dump("get.pnm", file = "get.pnm.R")
##:define [[get.pnm()]]##
#:221
    icon <- get.pnm(file)
  }
  if( "try-error" %in% class(icon) ){
    cat("Error in puticon(): file", file, "not readable by puticon()")
    return()
  }
##:read jpeg, png or pnm file in [[puticon()]]##
#:287
       rasterImage(icon, 0, 0, 100, 100 * ((h <- dim(icon))[1] / h[2]), 
                   interpolate = FALSE, xpd = xpd)
       return()
     } else {                # internal generator
       icon <- icon [1]
       h <- deparse(puticon); hh <- grep(paste(icon, sep = "", " [<][-]"), h)
       if( 0 < length(hh) ){
         h <- h[hh + (0:5)]; h <- paste(h, collapse = "")
         h <- sub("^[^(]*[(]", "", h); h <- sub("[{].*", "", h) # )))(
         h <- gsub(" +", " ", h); h <- sub("[)] *$", "", h)     #
         if(nchar(h) > 0){ 
           
#351:
##define generator functions:##
h <- "internal generator function"
  
#315:
##define [[BI()]]:##
BI <- function(){
  result <- list()
  res <- cbind( x = c(0, 25, 25, 0), y = c(0, 0, 61, 61), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- cbind( x = c(72, 57, 57, 72), y = c(0, 0, 61, 61), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- cbind( x = c( 0, 72,  72,  56, 56, 44,  44,  29, 29, 16,  16,   0), 
                y = c(72, 72, 100, 100, 87, 87, 100, 100, 87, 87, 100, 100), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- rbind( c(19.0, 43, 24,   43, lwd.mm = 36, color = NA),
                c(19.0, 18, 30.5, 18, lwd.mm = 36, color = NA),
                c(12, 43, 21, 43, lwd.mm = 11, 0),
                c(12, 18, 27, 18, lwd.mm = 11, 0) )
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"
  result <- c(result, list(res))
  res <- cbind( x = c(0, 15, 15, 0), y = c(0, 0, 61, 61), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  result
}
##:define [[BI()]]##
#:315
h <- "internal generator function"
  
#318:
##define [[TL()]]:##
TL <- function(L = c("AB", "DT", "PW", "NV", "Hello")[1], t.cex.mm = 10, 
               startx, starty, delx, dely, Lcolors,
               pointx = 90, pointy = 90, pointsize = 8, pointcolor = "red" ){
  # L               letters to be used
  # t.cex.mm        letter size: a 'W' will have a width of 'text.cex.mm' mm 
  # startx, starty  x coordinate of first letter in mm
  # delx, dely      shift in x and in y between letters in mm
  # Lcolors         colors of the letters to be used
  # pointx, pointy  x and y coordinate of point
  # pointsize       size of the point
  # pointcolor      color of the point
  if(is.factor(L) || is.numeric(L) ) L <- as.character(L)
  L <- unlist(strsplit(L,""))
  n <- length(L)
  check.num <- function(x, n = 2){
    if(is.factor(x)) x <- as.character(x); x <- as.numeric(x); x <- rep(x, n)[1:n] 
  }
  if(missing(startx)) startx <- 50 / n; if(missing(delx)) delx <- 100 / n  
  if(missing(starty)) starty <- 50 / n; if(missing(dely)) dely <- 100 / n 
  if(missing(t.cex.mm))  t.cex.mm  <- 100 / n 
  if(missing(pointx))   pointx <- 100 - min(60, 5 + n * 2.5)
  if(missing(pointy)){  pointy <-       min(40, 5 + n * 2.5)
                        if(starty[1] > 50) pointy <- 100 - pointy }
  if(missing(pointsize)) pointsize <- min(40, 10 + n * 2.5)
  if(missing(Lcolors)) Lcolors <- 1; Lcolors <- rep(Lcolors, n)[1:n]
  startx <- check.num(startx, n); starty <- check.num(starty, n) 
  delx <- cumsum(c(0, check.num(delx, n - 1))); dely <- cumsum(c(0, check.num(dely, n - 1)))
  startx <- (startx + delx) %% 100; starty <- (starty + dely) %% 100
  result <- list()
  res <- data.frame( x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), color = NA) 
  class(res) <- c( class(res), "polygon"); result <- c(result, list(res))  # box of the icon
  res <- data.frame(x0 = pointx, y0 = pointy, x1 = pointx, y1 = pointy, 
                    lwd.mm = pointsize, color = pointcolor)
  class(res) <- c( class(res), "segments"); result <- c(result, list(res)) # special point
  res <- data.frame(x = startx, y = starty, L = L, text.cex.mm = t.cex.mm, color = Lcolors)
  class(res) <- c( class(res), "text"); result <- c(result, list(res))     # letters
  result
} # ; show.icon.design(TL) #; TL()
##:define [[TL()]]##
#:318
h <- "internal generator function"
  
#325:
##define [[cross.simple()]]:##
cross.simple <- function(){  # print("in cross")
  res <- rbind( c( 05, 05, 95, 95, lwd.mm = 10, NA), 
                c( 05, 95, 95, 05, lwd.mm = 10, NA),
                c( 50, 50, 50, 50, lwd.mm = 30, 2) ) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[cross.simple()]]##
#:325
h <- "internal generator function"
  
#326:
##define [[cross()]]:##
cross <- function(z = 0.30){ # print("in cross")
  if(is.factor(z)){ z <- as.numeric(z); z <- 0.5 * z / length(levels(z)) } 
  z <- z * 100; eps <- 1 # *0.7
  z <- min(100, max(0, z))
  result <- list()
  res <- cbind( x = c(z, 100 - z, 100 - z, z), 
                y = c(0, 0, 100 - z, 100 - z), 
                color = 5) 
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- rbind( c(eps*c( 5,   5, 95, 95, lwd.mm = 10), NA), 
                c(eps*c( 5,  95, 95,  5, lwd.mm = 10), NA),
                c(eps*c( 50, 50, 50, 50, lwd.mm = 30), 3) ) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"
  result <- c(result, list(res))
  result
}
##:define [[cross()]]##
#:326
h <- "internal generator function"
  
#327:
##define [[circle.simple()]]:##
circle.simple <- function(){ # print("in circle.simple")
  res <- rbind( c( 50, 50, 50, 50, lwd.mm = 100, NA)) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[circle.simple()]]##
#:327
h <- "internal generator function"
  
#328:
##define [[circle()]]:##
circle <- function(whole = 0.50){     # print("in circle")
  if(is.factor(whole)){ 
    whole <- as.numeric(whole); whole <- 0.50 * whole / length(levels(whole))
  }
  whole <- min(1, whole)
  res <- rbind( c( 50, 50, 50, 50, lwd.mm = 100,          NA), 
                c( 50, 50, 50, 50, lwd.mm = whole * 100,  0)) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[circle()]]##
#:328
h <- "internal generator function"
  
#331:
##define [[car.simple()]]:##
car.simple <- function(){ # print("in cross")
  res0 <- cbind(t(cbind( 0.6* c( 05, 05, 95, 95), 0.6* c( 05, 95, 95, 05),
                         0.6* c( 50, 50, 50, 50)) + c(2.7,2.2)) ,
                lwd.mm = c(10,10,30), color =  c(2,5,7) ) 
  colnames(res0) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res0) <- "segments"
  res1 <- cbind( x = c(10, 90, 85, 75, 70, 45, 40, 20, 10), # car polygon
                 y = c(10, 10, 30, 30, 45, 45, 30, 30, 10))
  class(res1) <- "polygon"
  res2 <- cbind(t(cbind( 0.3* c( 05, 05, 95, 95), 0.3* c( 05, 95, 95, 05),
                         0.3* c( 50, 50, 50, 50)) + c(43, 10)) ,
                lwd.mm = 0.3 * c(10,10,30), # cross on door
                color =  c(4,6,2) ) 
  colnames(res2) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res2) <- "segments"
  res3 <- rbind( c(25, 10, 25, 10, 15, 1), c(75, 10, 75, 10, 15, 1)) # wheel
  colnames(res3) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res3) <- "segments"
  list(res1, res1, res2, res3)
} # ; car.simple()
##:define [[car.simple()]]##
#:331
h <- "internal generator function"
  
#332:
##define [[car()]]:##
car <- function(width = .5, height = .0){ # print("in cross")
  width  <- (width  * 2   + 2) / 3.2; height <- (height * 5.0 + 1) / 3.2
  x <- c(-40,  40, 35, 25, 20,-05,-10,-30, -40) * width  + 50
  y <- c(-20, -20,  0,  5, 20, 20,  5,  0, -20) * height + 50
  wheel.size <- height * 10 + 5
  ymin <- min(y); xmin <- min(x); xmax <- max(x)
  res1 <- cbind( x, y) # car polygon
  class(res1) <- "polygon"
  res2 <- rbind( c(h <- 0.75*xmin + 0.25*xmax, ymin, h, ymin, wheel.size, 1), 
                 c(100 - h, ymin, 100 - h, ymin, wheel.size, 1)) # wheel
  colnames(res2) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res2) <- "segments"; list(res1, res2)
} # ; car()
##:define [[car()]]##
#:332
h <- "internal generator function"
  
#334:
##define [[nabla()]]:##
nabla <- function(){ 
  res <- rbind( c( 05, 95, 50, 05, 10), c( 50, 05, 95, 95, 10),
                c( 95, 95, 05, 95, 10) );  class(res) <- "segments"
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm"); res
} # ; nabla()
##:define [[nabla()]]##
#:334
h <- "internal generator function"
  
#363:
##define [[walkman()]]:##
walkman <- function( balpha = 70, col = NA, 
           ll1alpha =  -80, ll2alpha = -120, lr1alpha = -45, lr2alpha = -100,
           al1alpha = -170, al2alpha = -100, ar1alpha = -60, ar2alpha =  +20 ){
  # generates a walking man in a device of pin-sizes: 10cm x 10 cm and lwd = 10 mm
  # col <- sample(1:10, size = 1)
  xy <- c(0,0); dxq <- 10; dyq <- 10; size <- 10; lwd <- 10.5; lw.unit <- 1
  segs.set <- NULL; col.set <- NULL
  scale.xy <- 2.54 
  balpha   <- balpha   / 180 * pi
  ll1alpha <- ll1alpha / 180 * pi;  ll2alpha <- ll2alpha / 180 * pi
  lr1alpha <- lr1alpha / 180 * pi;  lr2alpha <- lr2alpha / 180 * pi
  al1alpha <- al1alpha / 180 * pi;  al2alpha <- al2alpha / 180 * pi
  ar1alpha <- ar1alpha / 180 * pi;  ar2alpha <- ar2alpha / 180 * pi
  
#364:
##define body of [[walkman]]:##
  x <- c(cos(balpha), sin(balpha)) * scale.xy
  ba <- c(0,0); be <- ba + x
  bal <- lwd * lw.unit * 1.7; bac <- col 
  seg.mat <- cbind(a=ba[1], b=ba[2], c=be[1], d=be[2], e=bal)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, bac)
##:define body of [[walkman]]##
#:364
  
#366:
##define head of [[walkman]]:##
  h <- be + ( be - ba) * .75; hl <- lwd * lw.unit * 1.6; hc <- col
  seg.mat <- cbind(a=h[1], b=h[2], c=h[1], d=h[2], e=hl)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define head of [[walkman]]##
#:366
  
#365:
##define legs of [[walkman]]:##
  lbecken <- 0.19; llength <- 1; ll <- lwd * lw.unit * 0.85
  ll1a <- ba +   c(cos(balpha+pi/2), sin(balpha+pi/2)) * scale.xy * lbecken
  ll1e <- ll1a + c(cos(ll1alpha),    sin(ll1alpha))    * scale.xy * llength
  lr1a <- ba +   c(cos(balpha-pi/2), sin(balpha-pi/2)) * scale.xy * lbecken
  lr1e <- lr1a + c(cos(lr1alpha), sin(lr1alpha))  * scale.xy * llength
  ll2a <- ll1e 
  ll2e <- ll2a + c(cos(ll2alpha), sin(ll2alpha)) * scale.xy * llength
  lr2a <- lr1e 
  lr2e <- lr2a + c(cos(lr2alpha), sin(lr2alpha)) * scale.xy * llength
  l <- rbind(cbind(ll1a[1], ll1a[2], ll1e[1], ll1e[2])
            ,cbind(lr1a[1], lr1a[2], lr1e[1], lr1e[2])
            ,cbind(ll2a[1], ll2a[2], ll2e[1], ll2e[2])
            ,cbind(lr2a[1], lr2a[2], lr2e[1], lr2e[2]) )
  seg.mat <- cbind(l, e=ll)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define legs of [[walkman]]##
#:365
  
#367:
##define arms of [[walkman]]:##
  aschulter <- 0.19; alength <- 0.7; al <- lwd * lw.unit * 0.85
  al1a <- be +   c(cos(balpha+pi/2), sin(balpha+pi/2)) * scale.xy * aschulter
  al1e <- al1a + c(cos(al1alpha), sin(al1alpha))       * scale.xy * alength
  ar1a <- be +   c(cos(balpha-pi/2), sin(balpha-pi/2)) * scale.xy * aschulter
  ar1e <- ar1a + c(cos(ar1alpha), sin(ar1alpha))       * scale.xy * alength
  al2a <- al1e 
  al2e <- al2a + c(cos(al2alpha), sin(al2alpha)) * scale.xy * alength
  ar2a <- ar1e 
  ar2e <- ar2a + c(cos(ar2alpha), sin(ar2alpha)) * scale.xy * alength
  a <- rbind( cbind(al1a[1], al1a[2], al1e[1], al1e[2]), 
              cbind(ar1a[1], ar1a[2], ar1e[1], ar1e[2]),
              cbind(al2a[1], al2a[2], al2e[1], al2e[2]),
              cbind(ar2a[1], ar2a[2], ar2e[1], ar2e[2]) )
  seg.mat <- cbind(a, e=al)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define arms of [[walkman]]##
#:367
  segs.set[, 1:4] <- segs.set[, 1:4] + 5 # shift to the center
  segs.set <- cbind(as.data.frame(segs.set), f = col) # set color
  class(segs.set) <- c(class(segs.set), "segments")
  segs.set[, 1:4] <- segs.set[, 1:4] * 10
  colnames(segs.set) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  return(segs.set)
} #; show.icon.design(walkman, balpha = 90) # ; 
# plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
# puticon(5, 5.5, icon = walkman, icon.cex = 160, balpha = 80)
# walkman()
##:define [[walkman()]]##
#:363
h <- "internal generator function"
  
#371:
##define [[smiley.blueeye()]]:##
smiley.blueeye <- function(){
  # output: x0, y0, x1, y1, lwd, col 
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd.mm = 5, # a == radius
                     time.0 = 0, time.1 = 12, n = 30){
    # function to draw a part of a circle line
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0; y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd.mm)
  }
  res <- NULL
  #                               x0   y0   radius lwd.mm
  res <- rbind( res, cbind(circle(50, 49.5, 23,    50), col.code = NA) ) # face
  res <- rbind( res, cbind(       50, 45,   50,    50,  15,         1) ) # nose
  res <- rbind( res, cbind(circle(50, 49.5, 47.5,   5), 1) ) # margin
  res <- rbind( res, cbind(circle(35, 65,  2.5,    10), 4) ) # eye left
  res <- rbind( res, cbind(circle(65, 65,  2.5,    10), 1) ) # eye right
  res <- rbind( res, cbind(circle(50, 50,   27,     8,  7.50, 4.50), 3) ) # mouth
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
} # ; show.icon.design(smiley.blueeye) # ; smiley.blueeye()
##:define [[smiley.blueeye()]]##
#:371
h <- "internal generator function"
  
#372:
##define [[smiley.normal()]]:##
smiley.normal <- function(){
  # output: x0, y0, x1, y1, lwd, col 
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd.mm = 5, # a = radius
                     time.0 = 0, time.1 = 12, n = 30){
    # function to draw a part of a circle line
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0; y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd.mm)
  }
  res <- NULL # res <- rbind( res, cbind( 50, 45, 50, 50, 15,      1) ) # nose
  res <- rbind( res, cbind(circle(50, 49.5, 23,   50), col.code = NA) ) # face
  res <- rbind( res, cbind(circle(50, 49.5, 47.5,  5), 1) ) # rand
  res <- rbind( res, cbind(circle(35, 60.5,  3.0, 10), 1) ) # eye
  res <- rbind( res, cbind(circle(65, 60.5,  3.0, 10), 1) ) # eye 
  res <- rbind( res, cbind(circle(50, 50,   27,    8, 7.50, 4.50),1) ) # mouth
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
} #; show.icon.design(smiley.normal)
##:define [[smiley.normal()]]##
#:372
h <- "internal generator function"
  
#377:
##define [[smiley()]]:##
smiley <- function(smile = 0.8){
  if(is.factor(smile)) smile <- as.numeric(smile) / length(levels(smile))
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd = 5, 
                     time.0 = 0, time.1 = 12, n = 60){
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0;               y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd)
  }
  res <- NULL
  # res <- rbind( res, cbind(circle(50, 49.5,23,   50), col.code = NA) ) # face
  # res <- rbind( res, cbind(circle(50, 49.5,47,   60),  1) )            # rand
  res <- rbind( res, cbind(         50, 50,  50,   50, 100, 1 ))         # face+rand
  res <- rbind( res, cbind         (50, 50,  50,   50,  88, NA))         # face 
  res <- rbind( res, cbind(circle(  35, 60.5, 3.0, 10),  1) )            # eye
  res <- rbind( res, cbind(circle(  65, 60.5, 3.0, 10),  1) )            # eye 
  if(is.na(smile)){
    res <- rbind( res, cbind(circle(50, 50, 27,  7.5, 7.50, 4.50),1) ) # mouth
  } else {
    #             x0  y0            a lwd time.0  time.1
    # hs <- circle(50,50,         27,  10,   7.5,    4.5) # mouth laughing
    # hn <- circle(50,10,         27,  10,  10.5,   13.5) # mouth not laughing    
    hs <- circle( 50, 40,         17,  10,   8.5,    3.5) # mouth laughing
    hn <- circle( 50, 20,         17,  10,   9.5,   14.5) # mouth not laughing    
    s <- smile; n <- 1 - s
    h <- cbind( hs[,1], s*hs[,2]+n*hn[,2], hs[,3], s*hs[,4]+n*hn[,4], hs[,5])
    res <- rbind( res, cbind(h, 1) )                      # mouth
  }
  class(res) <- "segments"; res
  return(res)
}
##:define [[smiley()]]##
#:377
h <- "internal generator function"
  
#373:
##define [[smiley.sad()]]:##
smiley.sad <- function(){
  # output: x0, y0, x1, y1, lwd, col 
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd.mm = 5, 
                     time.0 = 0, time.1 = 12, n = 30){
    # function to draw a part of a circle line
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0; y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd.mm)
  }
  res <- NULL #; res <- rbind( res, cbind(50, 45, 50, 50, 15,      1) ) # nose
  res <- rbind( res, cbind(circle(50, 49.5, 23,   50), col.code = NA) ) # face
  res <- rbind( res, cbind(circle(50, 49.5, 47.5,  5), 1) ) # rand
  res <- rbind( res, cbind(circle(35, 60.5,  3.0, 10), 1) ) # eye
  res <- rbind( res, cbind(circle(65, 60.5,  3.0, 10), 1) ) # eye 
  res <- rbind( res, cbind(circle(50, 10,   27,    8, 10.50, 13.50),1) ) # mouth
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
} # ; show.icon.design(smiley.sad)
##:define [[smiley.sad()]]##
#:373
h <- "internal generator function"
  
#381:
##define [[mazz.man()]]:##
mazz.man <- function(Mean = 100, Penalty = 1, Region = "region:", 
                     expo = 1/(1:3)[3], Mean.max = 107, Mean.half = 90, 
                     Penalty.max = 5, Penalty.min = 0, 
                     x.text = 70, y.text = 10, text.cex.mm = 10){ 
  # bag.size %in% [0,1] # idea of the icon: Adriano Pareto, Matteo Mazziotta
  Mean.min <- Mean.half - (Mean.max - Mean.half) / ((h <- 2^(1/expo)) - 1)
  Mean.min <- min(Mean.min, Mean)
  fac      <- 0.95 * ((h * (Mean - Mean.min)) / Mean.max) ^ expo
  bag.size <- 0.80 * ((Penalty - Penalty.min) / Penalty.max )^expo /2
  res <- rbind(
    c(50,            77.5*fac + 5, 50,          77.5 *fac + 5), #head
    c(50,            35  *fac + 5, 50,          60   *fac + 5), #body
    c(50,            32  *fac + 5, 50,           0   *fac + 5), #leg in white
    c(50,            32  *fac + 5, 50,           0   *fac + 5), #leg
    c(50 + 30*fac,   55  *fac + 5, 50 + 25*fac, 75   *fac + 5), #tape2
    c(50 - 20*fac,   65  *fac + 5, 50 + 30*fac, 70   *fac + 5), #stick
    c(50,            64  *fac + 5, 50 - 15*fac, 45   *fac + 5), #arm one
    c(50 - 20*fac,   65  *fac + 5, 50 - 15*fac, 45   *fac + 5), #arm
    c(50 + 27.5*fac, 50  *fac + 5 - 20*bag.size ,
      50 + 27.5*fac, 50  *fac + 5 - 20*bag.size),               #bag
    c(50 + 25*fac,   55  *fac + 5, 50 + 30*fac, 75   *fac + 5)) #tape1
  colnames(res) <- c("x0", "y0", "x1", "y1")
  lwd.mm <-  c( c(17, 14, 12, 10, 2.5, 2, 6, 6) * fac / 0.927042
                ,   31 * bag.size / 0.2924, 2.5 * fac / 0.927042 ) 
  colors <- c("#3377BB", "white", "brown", "orange")[c(1,1,2,1,4,3,1,1,4,4)]
  res <- data.frame(res, lwd.mm = lwd.mm, color = colors)
  class(res) <- c(class(res), "segments"); result <- list(res)
  res <- data.frame(x = x.text, y = y.text, L = Region, text.cex.mm = text.cex.mm, color = 1) 
  class(res) <- c(class(res), "text"); res <- list(res) 
  result <- c(result, res)                   
  return(result)
} # ; show.icon.design(mazz.man) # Mazzi.Pareto
  # res1 <- rbind(c(0,0,100,100)); class(res1)<-c("segments"); res1 <- list(res1) 
  # res2 <- rbind(c(100,0,0,100)); class(res2)<-c("segments"); res2 <- list(res2) 
##:define [[mazz.man()]]##
#:381
h <- "internal generator function"
  
#389:
##define [[bike()]]:##
bike <- function(){
   res.liste <- NULL; a <- 1.5
   res <- rbind( c(20, 20, 20,    20, 40, 1),  # wheel front
                 c(20, 20, 20,    20, 30, NA), # wheel front
                 c(80, 20, 80,    20, 40, 1),  # wheel back
                 c(80, 20, 80,    20, 30, NA), # wheel back
                 c(50, 20, 80,    20,  3*a, 1),  # ---
                 c(50, 20, 65,    50,  3*a, 1),  # /
                 c(80, 20, 32.5,  45,  3*a, 1),    # \
                 c(50, 20, 32.5,  45,  3*a, 1),    # \
                 c(60, 50, 70,    50,  5*a, 1),  # seat
                 c(20, 20, 40,    60,  3*a, 1),  # /
                 c(40, 60, 45,    60,  5*a, 1) # control
   )
   res[, c(2,4)] <- res[, c(2,4)] + 20; class(res) <- "segments"
   colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
   res.liste <- c(res.liste, list(res))
}
##:define [[bike()]]##
#:389
h <- "internal generator function"
  
#390:
##define [[bike2()]]:##
bike2 <- function() {
   res.liste <- NULL; a <- 1.5
   res <- rbind( c(20, 20, 20,    20, 40, 1),  # wheel front
                 c(20, 20, 20,    20, 30, NA), # wheel front
                 c(80, 20, 80,    20, 40, 1),  # wheel back
                 c(80, 20, 80,    20, 30, NA), # wheel back
                 c(50, 20, 80,    20,3*a, 1),  # ---
                 c(50, 20, 65,    50,3*a, 1),  # /
                 c(80, 20, 32.5,  45,3*a, 1),  # \
                 c(50, 20, 32.5,  45,3*a, 1),  # \
                 c(60, 50, 70,    50,5*a, 1),  # seat
                 c(20, 20, 40,    60,3*a, 1),  # /
                 c(40, 60, 45,    60,5*a, 1))  # control  
   res[, c(2,4)] <- (res[, c(2,4)] - 9.3) * 10/5.3; class(res) <- "segments"
   colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
   res.liste <- c(res.liste, list(res))
}
##:define [[bike2()]]##
#:390
h <- "internal generator function"
  
#391:
##define [[heart()]]:##
heart <- function(txt = "xy"){
   txt <- substring(paste(txt, " "), 1:2, 1:2)
   res1 <- cbind( x = c(50, 80, 90, 70, 50, 50, 30, 10, 20, 50),
                  y = c(05, 30, 60, 85, 50, 50, 85, 60, 30, 05) + 05, color = NA)
   class(res1) <- c(class(res1), "polygon"); res1 <- list(res1)
   res2 <- cbind( x = c(50, 90, 70, 50, 50, 30, 10, 50),
                  y = c(05, 60, 85, 50, 50, 85, 60, 05) + 05)
   res2 <- data.frame( res2, lwd.mm = 19.5, color = NA)
   class(res2) <- c(class(res2), "spline"); res2 <- list(res2)
   res3 <- data.frame( x = c(27, 73), y = c(65, 65), txt = txt, 40, 1)
   class(res3) <- c(class(res3), "text");   res3 <- list(res3)
   result <- c(res1, res2, res3) 
} # ; show.icon.design(heart)()
##:define [[heart()]]##
#:391
h <- "internal generator function"
  
#392:
##define [[bend.sign()]]:##
bend.sign <- function(txt = "xy"){
   txt <- substring(paste(txt, " "), 1:2, 1:2)
   ground <- 6; top <- 90; center <- 55.5; size <- 25
   res0o <- c(50, top, 50, ground + 3, 7, 1) # Pfahl outer
   res0i <- c(50, top, 50, ground + 2, 3, 3) # Pfahl inner
   res1  <- c(30, ground, 70, ground, 2, 1)  # Fundament
   res2  <- rbind( c(50, center+size, 50-size, center), c(50-size, center, 50, center-size),
                   c(50, center-size, 50+size, center), c(50+size, center, 50, center+size))
   res2 <- cbind(res2, lwd.mm = 15, color = 1)    # Schildrand
   size <- size - 0                               # Innenrand:
   res3 <- rbind( c(50, center+size, 50-size, center), c(50-size, center, 50, center-size),
                  c(50, center-size, 50+size, center), c(50+size, center, 50, center+size))
   res3 <- cbind(res3, lwd.mm = 10, color = 2)
   res <- rbind(res0o, res0i, res1, res2, res3); rownames(res) <- NULL
   res <- as.data.frame(res)
   colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
   res$color <- c("black", NA, "gray")[res$color]
   class(res) <- c(class(res), "segments"); res <- list(res)
   size <- size - 2
   res1b <- rbind( c(50, center+size), c(50-size, center),
                   c(50, center-size), c(50+size, center))
   res1b <- cbind(res1b, color = NA)
   class(res1b) <- c(class(res1b), "polygon"); res1b <- list(res1b) # Innenflaeche
   f <- size / 25
   res2b <- cbind( x = 50     + f*c( h <- c(- 9,  5, 12), rev(-h)),
                   y = center + f*c( h <- c( 16, 13,  6), rev(-h)), 
                   lwd.mm = f^0.6*6.5, color = 1)
   class(res2b) <- c(class(res2b), "spline"); res2b <- list(res2b)
   #res3 <- data.frame( x = c(27, 73), y = c(65, 65), txt = txt, 40, 1)
   #class(res3) <- c(class(res3), "text");   res3 <- list(res3)
   result <- c(res, res1b, res2b) #, res1) 
}  # ; show.icon.design(bike2)# bend.sign) #; bend.sign()
##:define [[bend.sign()]]##
#:392
h <- "internal generator function"
  
#398:
##define [[fir.tree()]]:##
fir.tree <- function(height = 1, width = 1, txt = ".....", t.cex.mm = 10){ 
  fac.x <- width * 100/60; fac.y <- height * 100/70
  # build standardized elements of pictogram
  res <- data.frame( 
    x = c(20, 40, 25, 45, 35, 50, 65, 55, 75, 60, 80),
    y = c(20, 40, 40, 60, 60, 80, 60, 60, 40, 40, 20) + 5,
    color = NA)
  class(res) <- c(class(res), "polygon")
  res.liste <- c(list(res))
  res <- data.frame( 
    x = c(55, 55, 45, 45),
    y = c(20, 10, 10, 20) + 5 ,
    color = "brown")
  class(res) <- c(class(res), "polygon")
  res.liste <- c(res.liste, list(res))
  # integrate effects of arg1 and arg2
  res.liste <- lapply( res.liste, function(xyc){
                         xyc$x <- fac.x * (xyc$x - 50) + 50; xyc} )
  res.liste <- lapply( res.liste, function(xyc){
                         xyc$y <- fac.y * (xyc$y - 50) + 50; xyc} )
  # append text element # res <- data.frame( x = 20, y = 2, txt = txt, t.cex.mm, color = "1") 
  res <- data.frame( x =  fac.x * (30 - 50) + 50, y = fac.y * (10 - 50) + 50, 
                     txt = txt, t.cex.mm, color = "1") #180327
   class(res) <- c(class(res), "text")
  res.liste <- c(res.liste, list(res))
  res.liste
}  # ; show.icon.design(fir.tree)
##:define [[fir.tree()]]##
#:398
#343:
##define [[find.smooth.curve()]] and [[find.brush.polygon()]]:##
#342:
##define [[bs.approx()]] and [[loess.approx()]]:##
bs.approx <- function(x, y, x.new, degree = 3, knots = 10, df = NULL){
  # library(splines)                                          # check package splines
  if(is.matrix(x) || is.data.frame(x)){y <- x[,2];x <- x[,1]} # check x,y input
  n <- length(x); idx <- order(x); x <- x[idx]; y <- y[idx]   # order by x
  y.new <- rep(NA, length(x.new))                             # init y result
  x.all <- c(x, x.new);   y.all <- c(y, y.new)                # combine old and new points
  basis <- splines::bs(x.all, degree = degree, df = df, knots = knots) # find design matrix 
  res <- lm(y.all ~ basis); coef.ok <- !is.na(res$coeff)      # estimate spline coefficients
  X     <- cbind(1, basis[  1:n ,])[,coef.ok]                 # extract design matrix for old 
  X.new <- cbind(1, basis[-(1:n),])[,coef.ok]                 # extract design matrix for new
  y.dach     <- X     %*% res$coefficients[coef.ok]           # compute spline of old points 
  y.new.dach <- X.new %*% res$coefficients[coef.ok]           # compute spline of new points
  list(cbind(x, y.dach), cbind(x.new, y.new.dach))            # compose result
}
loess.approx <- function(x, y, x.new, span = 0.6, degree = 2){
  smooth.curve <- loess(y ~ x, span = span, degree = degree)
  res.new <- predict(smooth.curve, x.new)
  res.old <- predict(smooth.curve, x)
  return(list(cbind(x, res.old), cbind(x.new, res.new)))
}
##:define [[bs.approx()]] and [[loess.approx()]]##
#:342
find.smooth.curve <- function(x.in, y.in, n.new = 100, method = c("bs", "loess")[1],
                              degree = 3, knots = 50, span = 0.75){
  if(is.matrix(x.in) || is.data.frame(x.in)){y.in <- x.in[,2]; x.in <- x.in[,1]} # check input
  n <- length(x.in)
  dx.min <- 0.1 * diff(range(x.in)) / length(x.in)            # set minimal dx of spline
  x.h <- cumsum(c(1, pmax(dx.min, (diff(x.in)^2 + diff(y.in)^2)^0.5))) # find x of spline
  x.new <- seq(x.h[1], x.h[n], length = n.new)                # find new x for spline eval 
  if( method == "bs" ){
    res.x <- bs.approx(x = x.h, y = x.in, x.new = x.new, degree = degree, knots = knots)
    res.y <- bs.approx(x = x.h, y = y.in, x.new = x.new, degree = degree, knots = knots)
  } else {
    res.x <- loess.approx(x = x.h, y = x.in, x.new = x.new, span = span, degree = min(2, degree))
    res.y <- loess.approx(x = x.h, y = y.in, x.new = x.new, span = span, degree = min(2, degree))
  }
  return(cbind(x = res.x[[2]][,2], y = res.y[[2]][,2]))       # compose result
}
find.brush.polygon <- function(x, y, hwd = 10){
  # find area along the polygon of points (x, y) with width 2*hwd 
  if(is.matrix(x) || is.data.frame(x)){ y <- x[,2]; x <- x[,1] } # check input
  dy <- diff(x); dx <- -diff(y); h <- length(dx)            # find orthogonal vectors to segments
  dx <- c(dx[1], 0.5 * (dx[-1] + dx[-h]), dx[h])            # find means of neighbours 
  dy <- c(dy[1], 0.5 * (dy[-1] + dy[-h]), dy[h])            #    of orthogonal vectors
  d <- hwd / sqrt(dx^2 + dy^2); dy <- d * dy; dx <- d * dx  # scale orthognal vectors
  xy <- rbind(cbind(x = x + dx, y = y + dy), cbind(x = rev(x - dx), y = rev(y - dy))) 
  rbind(xy, xy[1,])                                         # copy first point to the end
}
##:define [[find.smooth.curve()]] and [[find.brush.polygon()]]##
#:343
h <- "internal generator function"
  
#406:
##define [[comet()]]:##
comet <- function(comet.color = NA){ 
  t2xy <- function(t,radius,init.angle=0) {
        t <- t / 360
        t2p <- 2*pi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  center <- c(17, 30); fac <- 1.2 #; fac <- .2
  # c.color <- 4; s.color <- 3; comet.bg.color <- 7;  bg.color <- 0; t.color <- 5
  # c.color <- "gold"; s.color <- "red"; comet.bg.color <- "green"; 
  # t.color <- "gold";  bg.color <- "lightgrey"
  comet.bg.color <- "white"; t.color <- NA;  bg.color <- "white"; s.color <- "white"
  c.color <- comet.color
  res.liste <- NULL
  # aera of icon -----------------------------------------------------------------------------
  res <- data.frame(c(1, 99, 99, 1, 1), c(1, 1, 99, 99, 1), color = bg.color)
  class(res) <- c(class(res), "polygon"); res.liste <- c(res.liste, list(res)) 
  # aera of comet ----------------------------------------------------------------------------
  width <- 20 * fac
  res <- data.frame(center[1], center[2], center[1], center[2], width, color = comet.bg.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
  # letter C of Comet -----------------------------------------------------------------------
  width <- 3 * fac; radius <- 10 * fac
  P <- t2xy( 90:-45 , radius, 0)
  res <- data.frame(P$x + center[1], P$y + center[2]); res <- cbind(res, res, 
                    width, color = bg.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # C missing part
  P <- t2xy( 67.5:180 , radius, 0)
  res <- data.frame(P$x + center[1], P$y + center[2]); res <- cbind(res, res, 
                    width, color = c.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # C
  P <- t2xy( -180:-22.5 , radius, 0)
  res <- data.frame(P$x + center[1], P$y + center[2]); res <- cbind(res, res, 
                    width, color = c.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # C
  # letter M of coMet ----------------------------------------------------------------------
  if( TRUE){
    width <- 2.5 * fac; shift <- c(1, 0.5) * 2 * fac; h <- 22.5 / 360 * 2 * pi
    xy <- cbind(c(-1, -1, 0, 1, 1), c(-1, 1, -1, 1, -1))
    xy <- xy %*% matrix( c( cos( h ), -sin(h), sin(h), cos(h)), 2, 2)
    x <- shift[1] + xy[,1] * 4 * fac; y <- shift[2] + xy[,2] * 4 * fac
    res <- data.frame(x[-5] + center[1], y[-5] + center[2], x[-1] + center[1], y[-1] + center[2],  
                      width, color = c.color)
    class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # M
  }
  # tail of comet with letter T of comeT --------------------------------------------------
  radius <- c(1, 5) * fac; width <- 3 * fac
  for(i in 1:6){
    radius <- radius + 10 * fac
    P1 <- t2xy(c(0, 22.5, 45), radius[1], 0)
    P2 <- t2xy(c(0, 22.5, 45), radius[2], 0)
    res <- data.frame( P1$x + center[1], P1$y + center[2], P2$x + center[1], P2$y + center[2], 
                       width, color = t.color)
    class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
    if(i == 1){
      res <- data.frame(P1$x[1] + center[1], P1$y[1] + center[2], 
                        P1$x[3] + center[1], P1$y[3] + center[2], 
                        width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
      res <- data.frame( P1$x + center[1], P1$y + center[2], P2$x + center[1], P2$y + center[2], 
                        width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
    } else {
      res <- data.frame( P1$x + center[1], P1$y + center[2], P2$x + center[1], P2$y + center[2], 
                        width, color = t.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
    }
    # T of comeT
    if(i == 5){
      h <- c(P2$x[2] - P1$x[2], P2$y[2] - P1$y[2]) * 2.5
      res <- data.frame( P1$x[2] + center[1], P1$y[2] + center[2], 
                         P2$x[2] + h[1] + center[1], P2$y[2] + h[2] +  center[2], 
                         width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
      h <- h * 0.6
      res <- data.frame( P1$x[2] + center[1], P1$y[2] + center[2], 
                         P1$x[2] - h[2] + center[1], P1$y[2] + h[1] +  center[2], 
                         width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
      res <- data.frame( P1$x[2] + center[1], P1$y[2] + center[2], 
                         P1$x[2] + h[2] + center[1], P1$y[2] - h[1] +  center[2], 
                         width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb     
    }
  }
  # letter O of cOmet
  width <- 2.5 * fac; radius <- 7.5 * fac; shift <- c(1, 0.5) * 1.2 * fac
  P <- t2xy( 0:359 , radius, 0)
  res <- data.frame(P$x + shift[1] + center[1], P$y + shift[2] + center[2])
  res <- cbind(res, res, width, color = "white")
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # O by white
  res.liste
} # ; print(comet());  show.icon.design(comet)
##:define [[comet()]]##
#:406
h <- "internal generator function"
  
#337:
##define [[coor.system()]]:##
coor.system <- function(xxx, yyy, pcex = 5, xrange, yrange, axes = FALSE){ 
  shift <- 0.5; lwd <- .25
  x <- c(.1, 3, 6, 9.9); y <- c(0, 0.1, -0.1, 0) + shift
  xy <- spline(x,y); x <- xy$x; y <- xy$y; n.1 <- length(x) - 1
  res <-            cbind(x[1:n.1], y[1:n.1], x[-1], y[-1], lwd)  # x line
  res <- rbind(res, cbind(y[1:n.1], x[1:n.1], y[-1], x[-1], lwd)) # y line
  res <- rbind(res, c(9.5, shift - 0.4, 9.9, shift, lwd),         # x arrow
                    c(9.4, shift + 0.3, 9.9, shift, lwd),         # x arrow
                    c(shift - 0.4, 9.5, shift, 9.9, lwd),         # y arrow
                    c(shift + 0.3, 9.4, shift, 9.9, lwd)) * 10    # y arrow
  res <- cbind(res, 1)
  class(res) <- "segments";   res.list <- list(res)
  if( !missing(xxx) ){
    if(missing(xrange)){ xrange <- range(xxx); xrange <- xrange + c(-0.1,0.1)*diff(xrange) }
    if(missing(yrange)){ yrange <- range(yyy); yrange <- yrange + c(-0.1,0.1)*diff(yrange) }
    xticks <- pretty(xrange); xticks <- xticks[ xrange[1] < xticks & xticks < xrange[2]]
    yticks <- pretty(yrange); yticks <- yticks[ yrange[1] < yticks & yticks < yrange[2]]
    xtickspos <- (xticks - xrange[1]) / (xrange[2] - xrange[1]) * 95 + 5
    ytickspos <- (yticks - yrange[1]) / (yrange[2] - yrange[1]) * 95 + 5
    res <- rbind(
             cbind(xtickspos, 5, xtickspos, 2, 2, 1)   # x ticks
            ,cbind(2, ytickspos, 5, ytickspos, 2, 1)   # y ticks
           )
    class(res) <- "segments"; res.list <- c(res.list, list(res))
    x <- c(-3, -1.5, 0, 1.5, 3); y <- c(-2.5, -0.5, 0.4, 1.5, 2.5) 
    xy <- spline(x,y); x <- xy$x; y <- xy$y
    xxx <- (xxx - xrange[1]) / (xrange[2] - xrange[1]) * 95 + 5
    yyy <- (yyy - yrange[1]) / (yrange[2] - yrange[1]) * 95 + 5
    res <- NULL; h <- length(x)
    for( i in seq(along = xxx) ){
      res <- rbind(res, 
             cbind(xxx[i] + x[-1], yyy[i] + y[-1], 
                   xxx[i] + x[-h], yyy[i] + y[-h], pcex * 0.2, NA), # points xxx, yyy
             cbind(xxx[i] + x[-1], yyy[i] + rev(y)[-1], 
                   xxx[i] + x[-h], yyy[i] + rev(y)[-h], pcex * 0.2, NA) # points xxx, yyy
      )
    }
    class(res) <- "segments"; res.list <- c(res.list, list(res))
    h <- length(xticks)  # x axis
    res <- data.frame( xtickspos, rep(-4, h), paste(xticks), rep(6, h),rep(1, h))
    class(res) <- c(class(res), "text"); if(axes) res.list <- c(res.list, list(res))
    h <- length(yticks); hh <- max(nchar(yticks)) # y axis
    res <- data.frame( rep(-2*hh, h), ytickspos, paste(yticks), rep(6, h),rep(1, h))
    class(res) <- c(class(res), "text"); if(axes) res.list <- c(res.list, list(res))
  } 
  res.list
}  ; coor.system()
##:define [[coor.system()]]##
#:337
##:define generator functions##
#:351
           show.icon.design(icon, color = "lightblue") 
           cat("arguments of", icon,":\n"); print(noquote(h))
           return(get(icon))
         }
       } else {
         cat("Error in puticon(): something wrong with input icon =", 
             paste('"',icon,'"',sep = ""), "\n")
         return()
       }
     } 
   } 
##:show design layout of special icon in [[puticon()]]##
#:277
}
if(identical(icon, "")) icon <- "circle" ## default setting of icon !!!!
if( is.matrix(x) && ncol(x) == 2){ 
  xy <- x 
} else {
  if(missing(y)){ 
    cat("Error in puticon: no x or no y coordinates are given!\n"); return()
  }
  xy <- cbind(x, y)
}
n.items <- dim(xy)[1]
##:check inputs and manage case of showing design in [[puticon()]]##
#:276
  
#282:
##get device infos and compute aspect ratios in [[puticon()]]:##
  h <- par(); usr <- h$usr; pin <- h$pin; cin <- h$cin 
  xwcoor.pmm <- diff(usr[1:2]) / pin[1] / 25.4 # xrange per mm of plot-region
  ywcoor.pmm <- diff(usr[3:4]) / pin[2] / 25.4 # yrange per mm of plot-region
  # correction because of uncorrect display sizes may by processed by:
  # dev.fac.vp <- c(1, 1, 1.1)[ c(dev.cur() == c("postscript", "pdf"), TRUE) ][1]
  # xwcoor.pmm <- xwcoor.pmm * dev.fac.vp; ywcoor.pmm <- ywcoor.pmm * dev.fac.vp
  aspect     <- xwcoor.pmm / ywcoor.pmm  # width / height concerning plot-region
  # if icon.cex < 1: size of icon is defined relative to width of plot #180411
  icon.cex <- ifelse( icon.cex < 1, icon.cex * pin[1] * 25.4, icon.cex)
  if(length(icon.cex) < n.items) icon.cex <- rep(icon.cex, n.items)[1:n.items]
  xsize <- icon.cex * xwcoor.pmm         # width(s)  of icon in user coordinates
  ysize <- icon.cex * ywcoor.pmm         # height(s) of icon in user coordinates
  if(length(xsize) < n.items) xsize <- rep(xsize, n.items)[1:n.items]
  if(length(ysize) < n.items) ysize <- rep(ysize, n.items)[1:n.items]
##:get device infos and compute aspect ratios in [[puticon()]]##
#:282
  
#283:
##define [[mm.to.lwd()]] and [[mm.to.cex]] in [[puticon()]]:##
  dev.fac <- c(0.8, 1, 1)[ c(dev.cur() == c("postscript", "pdf"), TRUE) ][1]
  mm.to.lwd <- function(lwd.mm) lwd.mm * 3.787878 * dev.fac 
  ## mm.to.lwd <- function(lwd.mm) lwd.mm * 100 / 30 * dev.fac 
  mm.to.cex <- function(text.cex.mm) text.cex.mm / (cin[1] * 25.4)
##:define [[mm.to.lwd()]] and [[mm.to.cex]] in [[puticon()]]##
#:283
  
#280:
##define [[transform.color.to.rgb.integer()]]:##
transform.color.to.rgb.integer <- function(x, to = c("#rrggbb", "rgb"), 
                                           debug = FALSE){
  dim.x <- n <- length(x)
  # if(is.matrix(x)) rasterImage(t(x), 5, 5, 15, 15)
  if( 0 < length( h <- dim(x) ) ) dim.x <- h
  if( is.numeric(x[1]) ){
    if( all( x %in% 0:8 ) ){
          from <- "color.code"      # col = 3
          xx <- t(col2rgb(palette("default")[x]))
    } else {
      if( all(x <= 1) ){            # 
          from <- "rgb.decimal"     # col = c(0.3, 0.5, 0.6 )
          xx <- floor( x * 255 ) 
      } else { 
        if( any( 1 < x ) & all(x == floor(x)) ){
          from <- "rgb.integer"     # col = c( 13, 25, 254 )
          xx <- x
        }
      }
    }
  } else {
    if( is.character(x[1]) ){
      from <- "color.name.or.char"  # col = "red" or col = "#1304FF"
      xx <- t(col2rgb(x))           # t?
    } 
  }
  if( (3 * n) == length(xx) ){ 
    xx <- array( xx, c(rev(dim.x), 3)) 
    if( length(dim(xx)) == 3 ) xx <- aperm(xx, c(2,1,3))
  }
  if(debug) cat("dim.x", dim.x, "from = ", from, "::", x, "->", (xx))
  xx
}
##:define [[transform.color.to.rgb.integer()]]##
#:280
  
#291:
##define [[greys.to.colors.puticon()]]:##
greys.to.colors.puticon <- function(grey.idx, color,      # invert = FALSE, 
                         set.black.and.white = FALSE, simple = FALSE){
  # cat("greys.to.colors.puticon")
  # if(invert) grey.idx[] <- max(grey.idx) + 1 - grey.idx # inversion of levels
  if(simple){ # simple case of recoloring
    # set of colors based on color
    color <- c("#000000", color, "#FFFFFF") # ;print(color)
    grey.idx[] <- color[grey.idx]; icon <- grey.idx; return(icon)
  } 
  n <- max(grey.idx)                        # find colors based on color:
  rgb.col1 <- col2rgb(color[1]) / 255; rgb.col2 <- 1 - rgb.col1
  n1 <- round(n / 2) ; n2 <- n - n1 
  f1 <- ((1:n1) - 1/2) / n1; f2 <- ((n2:1) - 1/2) / n2
  rgb.col1 <-     cbind(f1 * rgb.col1[1], f1 * rgb.col1[2], f1 * rgb.col1[3])
  rgb.col2 <- 1 - cbind(f2 * rgb.col2[1], f2 * rgb.col2[2], f2 * rgb.col2[3])
  rgb.col <- rbind(rgb.col1, rgb.col2); colors <- rgb(rgb.col)
  if(0 < set.black.and.white){
    if(set.black.and.white <= 1) colors[1]              <- "#000000"
    if(1 <= set.black.and.white) colors[length(colors)] <- "#FFFFFF"
  }  
  grey.idx[] <- colors[grey.idx]; icon <- grey.idx; return(icon)
} 
##:define [[greys.to.colors.puticon()]]##
#:291
     #
  
#233:
##define [[raster.to.greys()]]:##
raster.to.greys <- function(pic, grey.levels = c(0.05, 0.95),  # 2 :: black + white  
                            reduce = TRUE, n.icons = 1){
  
#234:
##prepare [[mat]] for coloring:##
d.mat <- dim(pic)
if( 3 == length(d.mat) ){ 
  mat <- apply(pic, 1:2, sum)  ; dim(mat) <- d.mat <- d.mat[1:2] 
} else {
  mat <- as.matrix(pic); d.mat <- dim(mat)
  mat <- col2rgb(mat, 7 < nchar(mat[1]))
  mat <- colMeans(mat)
}
mat   <- mat/max(1,mat) # /255 # shifted from else statements #180417 
mat <- matrix(mat, ncol = d.mat[2])
##:prepare [[mat]] for coloring##
#:234
  if( 2 == length(grey.levels) ){
    limits <- grey.levels
    
#241:
##find levels if the two [[limits]] are in (0,1):##
#235:
##reduce size of [[mat]]:##
if(reduce > 0 ){
  if(is.logical(reduce)) {
    # pixel.per.pic.plan <- width.of.picture * pixel.per.mm / n.icons^0.5
    pixel.per.pic <- 120 * 5 / n.icons^0.5 
    # reduction.factor <- pixel.per.pic.real / pixel.per.pic.plan
    reduce <- ceiling( max(d.mat) / pixel.per.pic )
  }
  if(reduce > 1){
    dim.mat.new <- reduce * (floor(d.mat/reduce))
    mat.new <- mat[1:dim.mat.new[1], 1:dim.mat.new[2]]
    mat.new <- array(mat.new, rbind(reduce, dim.mat.new/reduce))
    mat.new <- apply(mat.new, c(2,4), mean)
    d.mat <- dim(mat.new); mat <- pmin(1,mat.new); dim(mat) <- d.mat 
  }
}
##:reduce size of [[mat]]##
#:235
# generate matrix of levels
levs <- 1 + (limits[1] < mat) + (limits[2] < mat)   
dim(levs) <- d.mat  
##:find levels if the two [[limits]] are in (0,1)##
#:241
    return(levs)
  }
  
#235:
##reduce size of [[mat]]:##
if(reduce > 0 ){
  if(is.logical(reduce)) {
    # pixel.per.pic.plan <- width.of.picture * pixel.per.mm / n.icons^0.5
    pixel.per.pic <- 120 * 5 / n.icons^0.5 
    # reduction.factor <- pixel.per.pic.real / pixel.per.pic.plan
    reduce <- ceiling( max(d.mat) / pixel.per.pic )
  }
  if(reduce > 1){
    dim.mat.new <- reduce * (floor(d.mat/reduce))
    mat.new <- mat[1:dim.mat.new[1], 1:dim.mat.new[2]]
    mat.new <- array(mat.new, rbind(reduce, dim.mat.new/reduce))
    mat.new <- apply(mat.new, c(2,4), mean)
    d.mat <- dim(mat.new); mat <- pmin(1,mat.new); dim(mat) <- d.mat 
  }
}
##:reduce size of [[mat]]##
#:235
  if( 1 == length(grey.levels) && 0 == (grey.levels %% 1) ){ # number of levels given
    # find greys by number of greys
    n.greys <- max(round(grey.levels), 2)
    mat <- unclass(mat); min.mat <- min(mat); max.mat <- max(mat)
    grey.levels <- seq(min.mat, max.mat, length = n.greys + 1)[-(n.greys+1)][-1]
    p <- c(min.mat, mat[ min.mat < mat & mat < max.mat], max.mat )
    greys <- c(min.mat, quantile(p, grey.levels))
    greys <- unique(greys[ !is.na(greys) ])
  } else {                   # find greys by quantiles of grey rates
    grey.levels <- unique( pmin(1, pmax(0, sort(c(0, grey.levels, 1)))) )
    greys       <- unique( quantile(mat, grey.levels) )
    if( (h <- length(greys)) > 2 ) greys <- greys[ -h ]
  }  # generate matrix of levels:
  levs <- sapply( mat, function(x) sum( x >= greys ) ) 
  if(max(levs) == 2) levs <- levs + 1 # if 2 cols only use one color and white
  if( 2 == length(h <- unique(levs)) ) levs <- 2 + ( levs == max(h) ) 
  dim(levs) <- d.mat; return(levs)
} 
##:define [[raster.to.greys()]]##
#:233
  
#279:
##prepare color vector in [[puticon()]]:##
# adjust representation of color rgb in decimal form
if( ! is.na(color[1]) ){
  color <- transform.color.to.rgb.integer(color) 
  color <- rbind(color)
  color <- sapply(split(color, row(color)), 
    function(x) paste(sep="", "#", paste(collapse = "", 
              as.character(as.hexmode(c(x,255)))[-4])))
  # generate vectors of colors and icon.cex if 1 < length(x)
  if( 1 < n.items ){ 
    color    <- rep(color, n.items)[1:n.items]
    icon.cex <- rep(icon.cex, n.items)[1:n.items]
  }
}
##:prepare color vector in [[puticon()]]##
#:279
  
#286:
##plot jpg-, png- or pnm-file-icons and [[return()]] in [[puticon()]]:##
if( "" != file ){  # case: icon saved in a file # cat("dim icon", dim(icon))
  
#287:
##read jpeg, png or pnm file in [[puticon()]]:##
  icon <- 0; class(icon) <- "error"
  # JPEG
  if( 0 < length(grep("jp[e]{0,1}g$", file))  || 
      0 < length(grep("JP[E]{0,1}G$", file))){
    if(!"package:jpeg" %in% search()){
      print("puticon() requires package jpeg")
      # library(jpeg, lib.loc = .libPaths()) 
    }
    icon <- try(jpeg::readJPEG(file, native = !TRUE)) #############
  }
  # PNG
  if( 0 < length(grep("png$", file))  || 0 < length(grep("PNG$", file))){
    if(!"package:png" %in% search()){
      print("puticon() requires package png")
      # library(png, lib.loc = .libPaths()) 
    }
    icon <- png::readPNG(file, native = !TRUE)  ##################
  }
  # PNM
  if( 0 < length(grep("pnm$", file))  || 0 < length(grep("PNM$", file))){
    if(!"package:tcltk" %in% search()){
      print("puticon() requires package tcltk")
      # library(tcltk, lib.loc = .libPaths()) 
    }
    
#221:
##define [[get.pnm()]]:##
get.pnm <- function(filename, verbose = FALSE){
  
#222:
##find P* type of pnm file:##
nextline <- scan(filename, what="", n=1)
if( substring(nextline, 1, 1) != "P" || 
    ! ( PType <- substring(nextline, 2, 2) ) %in% as.character(1:6)) 
  return("ERROR: reading pnm file failed: no pnm file")
PType <- as.numeric(PType); if(verbose) cat("PType:", PType); skip <- 0
nextline <- scan(filename, what="", sep="\n", n=1, skip = skip, 
                 blank.lines.skip = FALSE)
nextline <- unlist( strsplit( nextline, "[ \t]"))[-1]
##:find P* type of pnm file##
#:222
  
#223:
##get width of pnm picture:##
#226:
##get next line that is not empty:##
idx <- 20
while(idx > 0 & 0 == length(nextline)){
  idx <- idx - 1; skip <- skip + 1
  nextline <- scan(filename, what="", sep="\n", n=1, skip = skip,
                   blank.lines.skip = FALSE)
  nextline <- sub("[#].*", "", nextline)
  nextline <- unlist( strsplit( nextline, "[ \t]"))
}
if( idx == 0 ) return("ERROR: reading pnm file failed: too many #-lines")
nextline <- as.numeric(nextline)
##:get next line that is not empty##
#:226
width <- nextline[1]; if(verbose) cat("width:",width)
nextline <- nextline[-1]
##:get width of pnm picture##
#:223
  
#224:
##get height of pnm picture:##
if( length(nextline) == 0 ){
  
#226:
##get next line that is not empty:##
idx <- 20
while(idx > 0 & 0 == length(nextline)){
  idx <- idx - 1; skip <- skip + 1
  nextline <- scan(filename, what="", sep="\n", n=1, skip = skip,
                   blank.lines.skip = FALSE)
  nextline <- sub("[#].*", "", nextline)
  nextline <- unlist( strsplit( nextline, "[ \t]"))
}
if( idx == 0 ) return("ERROR: reading pnm file failed: too many #-lines")
nextline <- as.numeric(nextline)
##:get next line that is not empty##
#:226
}
height <- nextline[1]; if(verbose) cat("height:", height)
nextline <- nextline[-1]
##:get height of pnm picture##
#:224
  
#225:
##get colors of pnm picture:##
if(PType == 1 || PType == 4) colors <- 1 else {
  if( length(nextline) == 0 ){
    
#226:
##get next line that is not empty:##
idx <- 20
while(idx > 0 & 0 == length(nextline)){
  idx <- idx - 1; skip <- skip + 1
  nextline <- scan(filename, what="", sep="\n", n=1, skip = skip,
                   blank.lines.skip = FALSE)
  nextline <- sub("[#].*", "", nextline)
  nextline <- unlist( strsplit( nextline, "[ \t]"))
}
if( idx == 0 ) return("ERROR: reading pnm file failed: too many #-lines")
nextline <- as.numeric(nextline)
##:get next line that is not empty##
#:226
  }
  colors <- nextline[1]
}; if(verbose){ cat("colors:", colors); cat("head processed") }
##:get colors of pnm picture##
#:225
  
#228:
##get decpixel of pnm picture:##
if(PType < 4){
  decpixel <- scan(filename, what="", sep="\n", skip = skip+1)
  decpixel <- paste(collapse=" ", decpixel)
  decpixel <- unlist( strsplit( decpixel, " +"))
  decpixel <- as.numeric(decpixel)
  decpixel <- matrix( decpixel, ncol = width, byrow = TRUE)
} else { # P4, P5, P6
  
#227:
##read picture data of P4, P5 or P6 pictures:##
tclcmds <- c('
  # https://de.wikipedia.org/wiki/Portable_Pixmap
  # set fname mm.pnm
  set fname FILENAME       
  set size [ file size $fname ]  
  # puts $size
  set fp [open $fname]
  # http://www.tek-tips.com/viewthread.cfm?qid=1477934  
  fconfigure $fp -translation binary 
  # scan [string range $contents i i] %c strA  
  set binpixel [read $fp $size]
  close $fp
  binary scan $binpixel cu* decpixel
')
tclcmds <- sub("FILENAME", filename, tclcmds)
#require(tcltk)
if( requireNamespace("tcltk") ){ 
  tcltk::.Tcl(tclcmds)
} else { 
  print("Error from get.pnm: tcltk not found!!"); return() 
}
decpixel <- as.character(tclvalue("decpixel"))
##:read picture data of P4, P5 or P6 pictures##
#:227
  decpixel <- unlist( strsplit( decpixel, " "))
  if( PType == 4){
    n.infos <- ceiling(width/8) * height 
    # if(verbose) cat("length(decpixel)", length(decpixel), "n.infos", n.infos)
    decpixel <- decpixel[ -(1 : ( length(decpixel) - n.infos )) ]
    encode <- function(number, base) {
      # simple version of APL-encode / APL-representation "T", pw 10/02
      # "encode" converts the numbers "number" using the radix vector "base"
      n.base <- length(base); result <- matrix(0, length(base), length(number))
      for(i in n.base:1){
        result[i,] <- if(base[i]>0) number %% base[i] else number
        number     <- ifelse(rep(base[i]>0,length(number)),
                             floor(number/base[i]), 0)
      }
      return( if(length(number)==1) result[,1] else result )
    }
    decpixel <- encode(as.numeric(decpixel), rep(2,8))
    decpixel <- matrix(decpixel, nrow = height, byrow = TRUE)[, 1:width]
    # decpixel <- matrix(decpixel, ncol = height, byrow = !TRUE)
    # decpixel <- t(decpixel[(1:width),])
  } else { # P5 or P6
    BigEndian <- colors > 255
    n.infos <- width * height * c(1,3)[1+(PType == 6)] * c(1,2)[1+BigEndian]   
    decpixel <- decpixel[ -(1 : ( length(decpixel) - n.infos )) ]
    if( BigEndian ){ # use the first byte of a pixel only
      decpixel <- matrix(decpixel, ncol = 2, byrow = TRUE)[,1] ### 1 or 2?
    }
    decpixel <- as.numeric(decpixel)
    decpixel <- matrix( decpixel, ncol = width * (1 + 2*(PType == 6)), byrow = TRUE)
  }
  PType <- PType - 3
}
##:get decpixel of pnm picture##
#:228
  
#229:
##define [[decpixel.to.raster()]]:##
decpixel.to.raster <- function(decpixel, PType, width, height, colors){
  HEX <- unlist(strsplit("0123456789ABCDEF",""))
  if(PType < 3){ # black and white or grey -- P1 or P2
    if(PType==1) decpixel <- colors - decpixel
    pixel <- decpixel / colors * 255
    first <- floor( pixel / 16 ); second <- pixel %% 16
    hexpixel <- paste(sep="", HEX[1 + first], HEX[1 + second])
    hexpixel <- paste(sep="", "#", hexpixel, hexpixel, hexpixel)
    hexpixel <- matrix(hexpixel, ncol = width)
  } else {       # colors -- P3
    decpixel <- array(t(decpixel), c(3, width, height))
    if( 255 < colors ) colors <- 255                                 # 160928
    pixel <- decpixel / colors * 255
    first <- floor(pixel / 16); second <- pixel %% 16
    hexpixel <- paste(sep="", HEX[1 + first], HEX[1 + second])
    hexpixel <- array(hexpixel, c(3, width, height))
    hexpixel <- paste(sep="", "#", hexpixel[1,,], hexpixel[2,,], hexpixel[3,,])
    hexpixel <- matrix(hexpixel, ncol = width, byrow = TRUE)
  }
  raster <- hexpixel
}
#table(decpixel.to.raster(a, 6, 724, 561, 65535))
#table(a)
##:define [[decpixel.to.raster()]]##
#:229
  as.raster(decpixel.to.raster(decpixel, PType, width, height, colors))
} #; dump("get.pnm", file = "get.pnm.R")
##:define [[get.pnm()]]##
#:221
    icon <- get.pnm(file)
  }
  if( "try-error" %in% class(icon) ){
    cat("Error in puticon(): file", file, "not readable by puticon()")
    return()
  }
##:read jpeg, png or pnm file in [[puticon()]]##
#:287
  
#289:
##transform colors of [[icon]] to grey levels in [[puticon()]]:##
# transform different representations of colors 
icon <- transform.color.to.rgb.integer(icon.orig <- icon)
# expand a single grey.levels value lower 1
if( length(grey.levels) == 1 && grey.levels < 1 ) 
  grey.levels <- c( grey.levels, 1)
# find the grey levels of the picture
icon <- raster.to.greys(icon, grey.levels = grey.levels, reduce = TRUE, 
              n.icons = max(1, ceiling(pin[1]*25.4 / max(icon.cex))^2))
# Now we have a matrix consisting of the grey levels of the pixels.
# A subset of the grey levels will be changed by the replacement color.
##:transform colors of [[icon]] to grey levels in [[puticon()]]##
#:289
  
#290:
##recolor [[icon]] for position [[i.x]] and plot it in [[puticon()]]:##
# precondition: icon must be a matrix describing 
#               the levels of greys of a raster graphics
simple <- all(grey.levels <= 1) &&  length(grey.levels) == 2 
# cat("simple", simple, "levels", grey.levels)
# recolor icons dependent on color # cat("else", simple, color)
if( is.na(color[1]) ){ # no recoloring
  idx.color.of.icon <- rep(1, length(x))
  pic <- list(as.raster(icon.orig))
} else {  
  pic <- NULL
  color.used <- unique(color)
  idx.color.of.icon <- match(color, color.used)
  for(i.color in color.used){      # cat("i.color", i.color)
    p <- greys.to.colors.puticon(icon, i.color,
                          set.black.and.white = 1.5, simple = simple)
    pic <- c( pic, list( p ))
  }
}
# find size(s) and position(s) of icon(s)
delx <- xwcoor.pmm * icon.cex / 2;  dely <- ywcoor.pmm * icon.cex / 2
dely <- dely * (h <- dim(icon))[1] / h[2] # proportional scaling
xmin <- x - delx; xmax <- x + delx; ymin <- y - dely; ymax <- y + dely
for(i.x in seq(along = x)){        # cat("i.x", i.x, "--------------")
  # get icon and plot
  icon.i.x <- pic[[ idx.color.of.icon[i.x] ]]
  rasterImage(icon.i.x, xmin[i.x], ymin[i.x], xmax[i.x], ymax[i.x], 
              interpolate = FALSE, xpd = xpd)
}
##:recolor [[icon]] for position [[i.x]] and plot it in [[puticon()]]##
#:290
  return()
}
##:plot jpg-, png- or pnm-file-icons and [[return()]] in [[puticon()]]##
#:286
  
#296:
##plot icons from a raster image object and [[return()]] in [[puticon()]]:##
if( is.raster(icon) ){
  
#289:
##transform colors of [[icon]] to grey levels in [[puticon()]]:##
# transform different representations of colors 
icon <- transform.color.to.rgb.integer(icon.orig <- icon)
# expand a single grey.levels value lower 1
if( length(grey.levels) == 1 && grey.levels < 1 ) 
  grey.levels <- c( grey.levels, 1)
# find the grey levels of the picture
icon <- raster.to.greys(icon, grey.levels = grey.levels, reduce = TRUE, 
              n.icons = max(1, ceiling(pin[1]*25.4 / max(icon.cex))^2))
# Now we have a matrix consisting of the grey levels of the pixels.
# A subset of the grey levels will be changed by the replacement color.
##:transform colors of [[icon]] to grey levels in [[puticon()]]##
#:289
  
#291:
##define [[greys.to.colors.puticon()]]:##
greys.to.colors.puticon <- function(grey.idx, color,      # invert = FALSE, 
                         set.black.and.white = FALSE, simple = FALSE){
  # cat("greys.to.colors.puticon")
  # if(invert) grey.idx[] <- max(grey.idx) + 1 - grey.idx # inversion of levels
  if(simple){ # simple case of recoloring
    # set of colors based on color
    color <- c("#000000", color, "#FFFFFF") # ;print(color)
    grey.idx[] <- color[grey.idx]; icon <- grey.idx; return(icon)
  } 
  n <- max(grey.idx)                        # find colors based on color:
  rgb.col1 <- col2rgb(color[1]) / 255; rgb.col2 <- 1 - rgb.col1
  n1 <- round(n / 2) ; n2 <- n - n1 
  f1 <- ((1:n1) - 1/2) / n1; f2 <- ((n2:1) - 1/2) / n2
  rgb.col1 <-     cbind(f1 * rgb.col1[1], f1 * rgb.col1[2], f1 * rgb.col1[3])
  rgb.col2 <- 1 - cbind(f2 * rgb.col2[1], f2 * rgb.col2[2], f2 * rgb.col2[3])
  rgb.col <- rbind(rgb.col1, rgb.col2); colors <- rgb(rgb.col)
  if(0 < set.black.and.white){
    if(set.black.and.white <= 1) colors[1]              <- "#000000"
    if(1 <= set.black.and.white) colors[length(colors)] <- "#FFFFFF"
  }  
  grey.idx[] <- colors[grey.idx]; icon <- grey.idx; return(icon)
} 
##:define [[greys.to.colors.puticon()]]##
#:291
     
  
#290:
##recolor [[icon]] for position [[i.x]] and plot it in [[puticon()]]:##
# precondition: icon must be a matrix describing 
#               the levels of greys of a raster graphics
simple <- all(grey.levels <= 1) &&  length(grey.levels) == 2 
# cat("simple", simple, "levels", grey.levels)
# recolor icons dependent on color # cat("else", simple, color)
if( is.na(color[1]) ){ # no recoloring
  idx.color.of.icon <- rep(1, length(x))
  pic <- list(as.raster(icon.orig))
} else {  
  pic <- NULL
  color.used <- unique(color)
  idx.color.of.icon <- match(color, color.used)
  for(i.color in color.used){      # cat("i.color", i.color)
    p <- greys.to.colors.puticon(icon, i.color,
                          set.black.and.white = 1.5, simple = simple)
    pic <- c( pic, list( p ))
  }
}
# find size(s) and position(s) of icon(s)
delx <- xwcoor.pmm * icon.cex / 2;  dely <- ywcoor.pmm * icon.cex / 2
dely <- dely * (h <- dim(icon))[1] / h[2] # proportional scaling
xmin <- x - delx; xmax <- x + delx; ymin <- y - dely; ymax <- y + dely
for(i.x in seq(along = x)){        # cat("i.x", i.x, "--------------")
  # get icon and plot
  icon.i.x <- pic[[ idx.color.of.icon[i.x] ]]
  rasterImage(icon.i.x, xmin[i.x], ymin[i.x], xmax[i.x], ymax[i.x], 
              interpolate = FALSE, xpd = xpd)
}
##:recolor [[icon]] for position [[i.x]] and plot it in [[puticon()]]##
#:290
  return()
}
##:plot icons from a raster image object and [[return()]] in [[puticon()]]##
#:296
  
#284:
##plot central symbols in [[pch]]-case and [[return()]] in [[puticon()]]:##
if( "" != pch ){
  if( is.numeric(pch) && pch %in% (1:128)){
    cex <- mm.to.cex(icon.cex) 
    cex <- cex / 0.75       # factor proposed by help of points, item 'pch'
    lwd <- list(...)$lwd; if( 0 == length(lwd)) lwd <- cex # lwd set by user 
    points(x, y, pch = pch, cex = cex, col = color, xpd = xpd, lwd = lwd) 
  } else { print("Error in puticon(): plotting symbol must be in 1..127") }
  return()
}
##:plot central symbols in [[pch]]-case and [[return()]] in [[puticon()]]##
#:284
  
#301:
##check pictogram generating function in [[puticon()]]:##
  
#311:
##if [[is.character(icon)]] look for internal generator in [[puticon()]]:##
if(is.character(icon)){ 
  
#351:
##define generator functions:##
h <- "internal generator function"
  
#315:
##define [[BI()]]:##
BI <- function(){
  result <- list()
  res <- cbind( x = c(0, 25, 25, 0), y = c(0, 0, 61, 61), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- cbind( x = c(72, 57, 57, 72), y = c(0, 0, 61, 61), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- cbind( x = c( 0, 72,  72,  56, 56, 44,  44,  29, 29, 16,  16,   0), 
                y = c(72, 72, 100, 100, 87, 87, 100, 100, 87, 87, 100, 100), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- rbind( c(19.0, 43, 24,   43, lwd.mm = 36, color = NA),
                c(19.0, 18, 30.5, 18, lwd.mm = 36, color = NA),
                c(12, 43, 21, 43, lwd.mm = 11, 0),
                c(12, 18, 27, 18, lwd.mm = 11, 0) )
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"
  result <- c(result, list(res))
  res <- cbind( x = c(0, 15, 15, 0), y = c(0, 0, 61, 61), color = NA)
  class(res) <- "polygon"
  result <- c(result, list(res))
  result
}
##:define [[BI()]]##
#:315
h <- "internal generator function"
  
#318:
##define [[TL()]]:##
TL <- function(L = c("AB", "DT", "PW", "NV", "Hello")[1], t.cex.mm = 10, 
               startx, starty, delx, dely, Lcolors,
               pointx = 90, pointy = 90, pointsize = 8, pointcolor = "red" ){
  # L               letters to be used
  # t.cex.mm        letter size: a 'W' will have a width of 'text.cex.mm' mm 
  # startx, starty  x coordinate of first letter in mm
  # delx, dely      shift in x and in y between letters in mm
  # Lcolors         colors of the letters to be used
  # pointx, pointy  x and y coordinate of point
  # pointsize       size of the point
  # pointcolor      color of the point
  if(is.factor(L) || is.numeric(L) ) L <- as.character(L)
  L <- unlist(strsplit(L,""))
  n <- length(L)
  check.num <- function(x, n = 2){
    if(is.factor(x)) x <- as.character(x); x <- as.numeric(x); x <- rep(x, n)[1:n] 
  }
  if(missing(startx)) startx <- 50 / n; if(missing(delx)) delx <- 100 / n  
  if(missing(starty)) starty <- 50 / n; if(missing(dely)) dely <- 100 / n 
  if(missing(t.cex.mm))  t.cex.mm  <- 100 / n 
  if(missing(pointx))   pointx <- 100 - min(60, 5 + n * 2.5)
  if(missing(pointy)){  pointy <-       min(40, 5 + n * 2.5)
                        if(starty[1] > 50) pointy <- 100 - pointy }
  if(missing(pointsize)) pointsize <- min(40, 10 + n * 2.5)
  if(missing(Lcolors)) Lcolors <- 1; Lcolors <- rep(Lcolors, n)[1:n]
  startx <- check.num(startx, n); starty <- check.num(starty, n) 
  delx <- cumsum(c(0, check.num(delx, n - 1))); dely <- cumsum(c(0, check.num(dely, n - 1)))
  startx <- (startx + delx) %% 100; starty <- (starty + dely) %% 100
  result <- list()
  res <- data.frame( x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), color = NA) 
  class(res) <- c( class(res), "polygon"); result <- c(result, list(res))  # box of the icon
  res <- data.frame(x0 = pointx, y0 = pointy, x1 = pointx, y1 = pointy, 
                    lwd.mm = pointsize, color = pointcolor)
  class(res) <- c( class(res), "segments"); result <- c(result, list(res)) # special point
  res <- data.frame(x = startx, y = starty, L = L, text.cex.mm = t.cex.mm, color = Lcolors)
  class(res) <- c( class(res), "text"); result <- c(result, list(res))     # letters
  result
} # ; show.icon.design(TL) #; TL()
##:define [[TL()]]##
#:318
h <- "internal generator function"
  
#325:
##define [[cross.simple()]]:##
cross.simple <- function(){  # print("in cross")
  res <- rbind( c( 05, 05, 95, 95, lwd.mm = 10, NA), 
                c( 05, 95, 95, 05, lwd.mm = 10, NA),
                c( 50, 50, 50, 50, lwd.mm = 30, 2) ) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[cross.simple()]]##
#:325
h <- "internal generator function"
  
#326:
##define [[cross()]]:##
cross <- function(z = 0.30){ # print("in cross")
  if(is.factor(z)){ z <- as.numeric(z); z <- 0.5 * z / length(levels(z)) } 
  z <- z * 100; eps <- 1 # *0.7
  z <- min(100, max(0, z))
  result <- list()
  res <- cbind( x = c(z, 100 - z, 100 - z, z), 
                y = c(0, 0, 100 - z, 100 - z), 
                color = 5) 
  class(res) <- "polygon"
  result <- c(result, list(res))
  res <- rbind( c(eps*c( 5,   5, 95, 95, lwd.mm = 10), NA), 
                c(eps*c( 5,  95, 95,  5, lwd.mm = 10), NA),
                c(eps*c( 50, 50, 50, 50, lwd.mm = 30), 3) ) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"
  result <- c(result, list(res))
  result
}
##:define [[cross()]]##
#:326
h <- "internal generator function"
  
#327:
##define [[circle.simple()]]:##
circle.simple <- function(){ # print("in circle.simple")
  res <- rbind( c( 50, 50, 50, 50, lwd.mm = 100, NA)) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[circle.simple()]]##
#:327
h <- "internal generator function"
  
#328:
##define [[circle()]]:##
circle <- function(whole = 0.50){     # print("in circle")
  if(is.factor(whole)){ 
    whole <- as.numeric(whole); whole <- 0.50 * whole / length(levels(whole))
  }
  whole <- min(1, whole)
  res <- rbind( c( 50, 50, 50, 50, lwd.mm = 100,          NA), 
                c( 50, 50, 50, 50, lwd.mm = whole * 100,  0)) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[circle()]]##
#:328
h <- "internal generator function"
  
#331:
##define [[car.simple()]]:##
car.simple <- function(){ # print("in cross")
  res0 <- cbind(t(cbind( 0.6* c( 05, 05, 95, 95), 0.6* c( 05, 95, 95, 05),
                         0.6* c( 50, 50, 50, 50)) + c(2.7,2.2)) ,
                lwd.mm = c(10,10,30), color =  c(2,5,7) ) 
  colnames(res0) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res0) <- "segments"
  res1 <- cbind( x = c(10, 90, 85, 75, 70, 45, 40, 20, 10), # car polygon
                 y = c(10, 10, 30, 30, 45, 45, 30, 30, 10))
  class(res1) <- "polygon"
  res2 <- cbind(t(cbind( 0.3* c( 05, 05, 95, 95), 0.3* c( 05, 95, 95, 05),
                         0.3* c( 50, 50, 50, 50)) + c(43, 10)) ,
                lwd.mm = 0.3 * c(10,10,30), # cross on door
                color =  c(4,6,2) ) 
  colnames(res2) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res2) <- "segments"
  res3 <- rbind( c(25, 10, 25, 10, 15, 1), c(75, 10, 75, 10, 15, 1)) # wheel
  colnames(res3) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res3) <- "segments"
  list(res1, res1, res2, res3)
} # ; car.simple()
##:define [[car.simple()]]##
#:331
h <- "internal generator function"
  
#332:
##define [[car()]]:##
car <- function(width = .5, height = .0){ # print("in cross")
  width  <- (width  * 2   + 2) / 3.2; height <- (height * 5.0 + 1) / 3.2
  x <- c(-40,  40, 35, 25, 20,-05,-10,-30, -40) * width  + 50
  y <- c(-20, -20,  0,  5, 20, 20,  5,  0, -20) * height + 50
  wheel.size <- height * 10 + 5
  ymin <- min(y); xmin <- min(x); xmax <- max(x)
  res1 <- cbind( x, y) # car polygon
  class(res1) <- "polygon"
  res2 <- rbind( c(h <- 0.75*xmin + 0.25*xmax, ymin, h, ymin, wheel.size, 1), 
                 c(100 - h, ymin, 100 - h, ymin, wheel.size, 1)) # wheel
  colnames(res2) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res2) <- "segments"; list(res1, res2)
} # ; car()
##:define [[car()]]##
#:332
h <- "internal generator function"
  
#334:
##define [[nabla()]]:##
nabla <- function(){ 
  res <- rbind( c( 05, 95, 50, 05, 10), c( 50, 05, 95, 95, 10),
                c( 95, 95, 05, 95, 10) );  class(res) <- "segments"
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm"); res
} # ; nabla()
##:define [[nabla()]]##
#:334
h <- "internal generator function"
  
#363:
##define [[walkman()]]:##
walkman <- function( balpha = 70, col = NA, 
           ll1alpha =  -80, ll2alpha = -120, lr1alpha = -45, lr2alpha = -100,
           al1alpha = -170, al2alpha = -100, ar1alpha = -60, ar2alpha =  +20 ){
  # generates a walking man in a device of pin-sizes: 10cm x 10 cm and lwd = 10 mm
  # col <- sample(1:10, size = 1)
  xy <- c(0,0); dxq <- 10; dyq <- 10; size <- 10; lwd <- 10.5; lw.unit <- 1
  segs.set <- NULL; col.set <- NULL
  scale.xy <- 2.54 
  balpha   <- balpha   / 180 * pi
  ll1alpha <- ll1alpha / 180 * pi;  ll2alpha <- ll2alpha / 180 * pi
  lr1alpha <- lr1alpha / 180 * pi;  lr2alpha <- lr2alpha / 180 * pi
  al1alpha <- al1alpha / 180 * pi;  al2alpha <- al2alpha / 180 * pi
  ar1alpha <- ar1alpha / 180 * pi;  ar2alpha <- ar2alpha / 180 * pi
  
#364:
##define body of [[walkman]]:##
  x <- c(cos(balpha), sin(balpha)) * scale.xy
  ba <- c(0,0); be <- ba + x
  bal <- lwd * lw.unit * 1.7; bac <- col 
  seg.mat <- cbind(a=ba[1], b=ba[2], c=be[1], d=be[2], e=bal)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, bac)
##:define body of [[walkman]]##
#:364
  
#366:
##define head of [[walkman]]:##
  h <- be + ( be - ba) * .75; hl <- lwd * lw.unit * 1.6; hc <- col
  seg.mat <- cbind(a=h[1], b=h[2], c=h[1], d=h[2], e=hl)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define head of [[walkman]]##
#:366
  
#365:
##define legs of [[walkman]]:##
  lbecken <- 0.19; llength <- 1; ll <- lwd * lw.unit * 0.85
  ll1a <- ba +   c(cos(balpha+pi/2), sin(balpha+pi/2)) * scale.xy * lbecken
  ll1e <- ll1a + c(cos(ll1alpha),    sin(ll1alpha))    * scale.xy * llength
  lr1a <- ba +   c(cos(balpha-pi/2), sin(balpha-pi/2)) * scale.xy * lbecken
  lr1e <- lr1a + c(cos(lr1alpha), sin(lr1alpha))  * scale.xy * llength
  ll2a <- ll1e 
  ll2e <- ll2a + c(cos(ll2alpha), sin(ll2alpha)) * scale.xy * llength
  lr2a <- lr1e 
  lr2e <- lr2a + c(cos(lr2alpha), sin(lr2alpha)) * scale.xy * llength
  l <- rbind(cbind(ll1a[1], ll1a[2], ll1e[1], ll1e[2])
            ,cbind(lr1a[1], lr1a[2], lr1e[1], lr1e[2])
            ,cbind(ll2a[1], ll2a[2], ll2e[1], ll2e[2])
            ,cbind(lr2a[1], lr2a[2], lr2e[1], lr2e[2]) )
  seg.mat <- cbind(l, e=ll)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define legs of [[walkman]]##
#:365
  
#367:
##define arms of [[walkman]]:##
  aschulter <- 0.19; alength <- 0.7; al <- lwd * lw.unit * 0.85
  al1a <- be +   c(cos(balpha+pi/2), sin(balpha+pi/2)) * scale.xy * aschulter
  al1e <- al1a + c(cos(al1alpha), sin(al1alpha))       * scale.xy * alength
  ar1a <- be +   c(cos(balpha-pi/2), sin(balpha-pi/2)) * scale.xy * aschulter
  ar1e <- ar1a + c(cos(ar1alpha), sin(ar1alpha))       * scale.xy * alength
  al2a <- al1e 
  al2e <- al2a + c(cos(al2alpha), sin(al2alpha)) * scale.xy * alength
  ar2a <- ar1e 
  ar2e <- ar2a + c(cos(ar2alpha), sin(ar2alpha)) * scale.xy * alength
  a <- rbind( cbind(al1a[1], al1a[2], al1e[1], al1e[2]), 
              cbind(ar1a[1], ar1a[2], ar1e[1], ar1e[2]),
              cbind(al2a[1], al2a[2], al2e[1], al2e[2]),
              cbind(ar2a[1], ar2a[2], ar2e[1], ar2e[2]) )
  seg.mat <- cbind(a, e=al)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define arms of [[walkman]]##
#:367
  segs.set[, 1:4] <- segs.set[, 1:4] + 5 # shift to the center
  segs.set <- cbind(as.data.frame(segs.set), f = col) # set color
  class(segs.set) <- c(class(segs.set), "segments")
  segs.set[, 1:4] <- segs.set[, 1:4] * 10
  colnames(segs.set) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  return(segs.set)
} #; show.icon.design(walkman, balpha = 90) # ; 
# plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
# puticon(5, 5.5, icon = walkman, icon.cex = 160, balpha = 80)
# walkman()
##:define [[walkman()]]##
#:363
h <- "internal generator function"
  
#371:
##define [[smiley.blueeye()]]:##
smiley.blueeye <- function(){
  # output: x0, y0, x1, y1, lwd, col 
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd.mm = 5, # a == radius
                     time.0 = 0, time.1 = 12, n = 30){
    # function to draw a part of a circle line
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0; y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd.mm)
  }
  res <- NULL
  #                               x0   y0   radius lwd.mm
  res <- rbind( res, cbind(circle(50, 49.5, 23,    50), col.code = NA) ) # face
  res <- rbind( res, cbind(       50, 45,   50,    50,  15,         1) ) # nose
  res <- rbind( res, cbind(circle(50, 49.5, 47.5,   5), 1) ) # margin
  res <- rbind( res, cbind(circle(35, 65,  2.5,    10), 4) ) # eye left
  res <- rbind( res, cbind(circle(65, 65,  2.5,    10), 1) ) # eye right
  res <- rbind( res, cbind(circle(50, 50,   27,     8,  7.50, 4.50), 3) ) # mouth
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
} # ; show.icon.design(smiley.blueeye) # ; smiley.blueeye()
##:define [[smiley.blueeye()]]##
#:371
h <- "internal generator function"
  
#372:
##define [[smiley.normal()]]:##
smiley.normal <- function(){
  # output: x0, y0, x1, y1, lwd, col 
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd.mm = 5, # a = radius
                     time.0 = 0, time.1 = 12, n = 30){
    # function to draw a part of a circle line
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0; y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd.mm)
  }
  res <- NULL # res <- rbind( res, cbind( 50, 45, 50, 50, 15,      1) ) # nose
  res <- rbind( res, cbind(circle(50, 49.5, 23,   50), col.code = NA) ) # face
  res <- rbind( res, cbind(circle(50, 49.5, 47.5,  5), 1) ) # rand
  res <- rbind( res, cbind(circle(35, 60.5,  3.0, 10), 1) ) # eye
  res <- rbind( res, cbind(circle(65, 60.5,  3.0, 10), 1) ) # eye 
  res <- rbind( res, cbind(circle(50, 50,   27,    8, 7.50, 4.50),1) ) # mouth
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
} #; show.icon.design(smiley.normal)
##:define [[smiley.normal()]]##
#:372
h <- "internal generator function"
  
#377:
##define [[smiley()]]:##
smiley <- function(smile = 0.8){
  if(is.factor(smile)) smile <- as.numeric(smile) / length(levels(smile))
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd = 5, 
                     time.0 = 0, time.1 = 12, n = 60){
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0;               y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd)
  }
  res <- NULL
  # res <- rbind( res, cbind(circle(50, 49.5,23,   50), col.code = NA) ) # face
  # res <- rbind( res, cbind(circle(50, 49.5,47,   60),  1) )            # rand
  res <- rbind( res, cbind(         50, 50,  50,   50, 100, 1 ))         # face+rand
  res <- rbind( res, cbind         (50, 50,  50,   50,  88, NA))         # face 
  res <- rbind( res, cbind(circle(  35, 60.5, 3.0, 10),  1) )            # eye
  res <- rbind( res, cbind(circle(  65, 60.5, 3.0, 10),  1) )            # eye 
  if(is.na(smile)){
    res <- rbind( res, cbind(circle(50, 50, 27,  7.5, 7.50, 4.50),1) ) # mouth
  } else {
    #             x0  y0            a lwd time.0  time.1
    # hs <- circle(50,50,         27,  10,   7.5,    4.5) # mouth laughing
    # hn <- circle(50,10,         27,  10,  10.5,   13.5) # mouth not laughing    
    hs <- circle( 50, 40,         17,  10,   8.5,    3.5) # mouth laughing
    hn <- circle( 50, 20,         17,  10,   9.5,   14.5) # mouth not laughing    
    s <- smile; n <- 1 - s
    h <- cbind( hs[,1], s*hs[,2]+n*hn[,2], hs[,3], s*hs[,4]+n*hn[,4], hs[,5])
    res <- rbind( res, cbind(h, 1) )                      # mouth
  }
  class(res) <- "segments"; res
  return(res)
}
##:define [[smiley()]]##
#:377
h <- "internal generator function"
  
#373:
##define [[smiley.sad()]]:##
smiley.sad <- function(){
  # output: x0, y0, x1, y1, lwd, col 
  circle <- function(x0 = 1, y0 = 1, a = 3, lwd.mm = 5, 
                     time.0 = 0, time.1 = 12, n = 30){
    # function to draw a part of a circle line
    alpha <- seq(time.0, time.1, length = n); alpha <- alpha * (2*pi/12)
    x <- a * sin(alpha) + x0; y <- a * cos(alpha) + y0
    cbind(x[-n],y[-n], x[-1],y[-1], lwd.mm)
  }
  res <- NULL #; res <- rbind( res, cbind(50, 45, 50, 50, 15,      1) ) # nose
  res <- rbind( res, cbind(circle(50, 49.5, 23,   50), col.code = NA) ) # face
  res <- rbind( res, cbind(circle(50, 49.5, 47.5,  5), 1) ) # rand
  res <- rbind( res, cbind(circle(35, 60.5,  3.0, 10), 1) ) # eye
  res <- rbind( res, cbind(circle(65, 60.5,  3.0, 10), 1) ) # eye 
  res <- rbind( res, cbind(circle(50, 10,   27,    8, 10.50, 13.50),1) ) # mouth
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
} # ; show.icon.design(smiley.sad)
##:define [[smiley.sad()]]##
#:373
h <- "internal generator function"
  
#381:
##define [[mazz.man()]]:##
mazz.man <- function(Mean = 100, Penalty = 1, Region = "region:", 
                     expo = 1/(1:3)[3], Mean.max = 107, Mean.half = 90, 
                     Penalty.max = 5, Penalty.min = 0, 
                     x.text = 70, y.text = 10, text.cex.mm = 10){ 
  # bag.size %in% [0,1] # idea of the icon: Adriano Pareto, Matteo Mazziotta
  Mean.min <- Mean.half - (Mean.max - Mean.half) / ((h <- 2^(1/expo)) - 1)
  Mean.min <- min(Mean.min, Mean)
  fac      <- 0.95 * ((h * (Mean - Mean.min)) / Mean.max) ^ expo
  bag.size <- 0.80 * ((Penalty - Penalty.min) / Penalty.max )^expo /2
  res <- rbind(
    c(50,            77.5*fac + 5, 50,          77.5 *fac + 5), #head
    c(50,            35  *fac + 5, 50,          60   *fac + 5), #body
    c(50,            32  *fac + 5, 50,           0   *fac + 5), #leg in white
    c(50,            32  *fac + 5, 50,           0   *fac + 5), #leg
    c(50 + 30*fac,   55  *fac + 5, 50 + 25*fac, 75   *fac + 5), #tape2
    c(50 - 20*fac,   65  *fac + 5, 50 + 30*fac, 70   *fac + 5), #stick
    c(50,            64  *fac + 5, 50 - 15*fac, 45   *fac + 5), #arm one
    c(50 - 20*fac,   65  *fac + 5, 50 - 15*fac, 45   *fac + 5), #arm
    c(50 + 27.5*fac, 50  *fac + 5 - 20*bag.size ,
      50 + 27.5*fac, 50  *fac + 5 - 20*bag.size),               #bag
    c(50 + 25*fac,   55  *fac + 5, 50 + 30*fac, 75   *fac + 5)) #tape1
  colnames(res) <- c("x0", "y0", "x1", "y1")
  lwd.mm <-  c( c(17, 14, 12, 10, 2.5, 2, 6, 6) * fac / 0.927042
                ,   31 * bag.size / 0.2924, 2.5 * fac / 0.927042 ) 
  colors <- c("#3377BB", "white", "brown", "orange")[c(1,1,2,1,4,3,1,1,4,4)]
  res <- data.frame(res, lwd.mm = lwd.mm, color = colors)
  class(res) <- c(class(res), "segments"); result <- list(res)
  res <- data.frame(x = x.text, y = y.text, L = Region, text.cex.mm = text.cex.mm, color = 1) 
  class(res) <- c(class(res), "text"); res <- list(res) 
  result <- c(result, res)                   
  return(result)
} # ; show.icon.design(mazz.man) # Mazzi.Pareto
  # res1 <- rbind(c(0,0,100,100)); class(res1)<-c("segments"); res1 <- list(res1) 
  # res2 <- rbind(c(100,0,0,100)); class(res2)<-c("segments"); res2 <- list(res2) 
##:define [[mazz.man()]]##
#:381
h <- "internal generator function"
  
#389:
##define [[bike()]]:##
bike <- function(){
   res.liste <- NULL; a <- 1.5
   res <- rbind( c(20, 20, 20,    20, 40, 1),  # wheel front
                 c(20, 20, 20,    20, 30, NA), # wheel front
                 c(80, 20, 80,    20, 40, 1),  # wheel back
                 c(80, 20, 80,    20, 30, NA), # wheel back
                 c(50, 20, 80,    20,  3*a, 1),  # ---
                 c(50, 20, 65,    50,  3*a, 1),  # /
                 c(80, 20, 32.5,  45,  3*a, 1),    # \
                 c(50, 20, 32.5,  45,  3*a, 1),    # \
                 c(60, 50, 70,    50,  5*a, 1),  # seat
                 c(20, 20, 40,    60,  3*a, 1),  # /
                 c(40, 60, 45,    60,  5*a, 1) # control
   )
   res[, c(2,4)] <- res[, c(2,4)] + 20; class(res) <- "segments"
   colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
   res.liste <- c(res.liste, list(res))
}
##:define [[bike()]]##
#:389
h <- "internal generator function"
  
#390:
##define [[bike2()]]:##
bike2 <- function() {
   res.liste <- NULL; a <- 1.5
   res <- rbind( c(20, 20, 20,    20, 40, 1),  # wheel front
                 c(20, 20, 20,    20, 30, NA), # wheel front
                 c(80, 20, 80,    20, 40, 1),  # wheel back
                 c(80, 20, 80,    20, 30, NA), # wheel back
                 c(50, 20, 80,    20,3*a, 1),  # ---
                 c(50, 20, 65,    50,3*a, 1),  # /
                 c(80, 20, 32.5,  45,3*a, 1),  # \
                 c(50, 20, 32.5,  45,3*a, 1),  # \
                 c(60, 50, 70,    50,5*a, 1),  # seat
                 c(20, 20, 40,    60,3*a, 1),  # /
                 c(40, 60, 45,    60,5*a, 1))  # control  
   res[, c(2,4)] <- (res[, c(2,4)] - 9.3) * 10/5.3; class(res) <- "segments"
   colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
   res.liste <- c(res.liste, list(res))
}
##:define [[bike2()]]##
#:390
h <- "internal generator function"
  
#391:
##define [[heart()]]:##
heart <- function(txt = "xy"){
   txt <- substring(paste(txt, " "), 1:2, 1:2)
   res1 <- cbind( x = c(50, 80, 90, 70, 50, 50, 30, 10, 20, 50),
                  y = c(05, 30, 60, 85, 50, 50, 85, 60, 30, 05) + 05, color = NA)
   class(res1) <- c(class(res1), "polygon"); res1 <- list(res1)
   res2 <- cbind( x = c(50, 90, 70, 50, 50, 30, 10, 50),
                  y = c(05, 60, 85, 50, 50, 85, 60, 05) + 05)
   res2 <- data.frame( res2, lwd.mm = 19.5, color = NA)
   class(res2) <- c(class(res2), "spline"); res2 <- list(res2)
   res3 <- data.frame( x = c(27, 73), y = c(65, 65), txt = txt, 40, 1)
   class(res3) <- c(class(res3), "text");   res3 <- list(res3)
   result <- c(res1, res2, res3) 
} # ; show.icon.design(heart)()
##:define [[heart()]]##
#:391
h <- "internal generator function"
  
#392:
##define [[bend.sign()]]:##
bend.sign <- function(txt = "xy"){
   txt <- substring(paste(txt, " "), 1:2, 1:2)
   ground <- 6; top <- 90; center <- 55.5; size <- 25
   res0o <- c(50, top, 50, ground + 3, 7, 1) # Pfahl outer
   res0i <- c(50, top, 50, ground + 2, 3, 3) # Pfahl inner
   res1  <- c(30, ground, 70, ground, 2, 1)  # Fundament
   res2  <- rbind( c(50, center+size, 50-size, center), c(50-size, center, 50, center-size),
                   c(50, center-size, 50+size, center), c(50+size, center, 50, center+size))
   res2 <- cbind(res2, lwd.mm = 15, color = 1)    # Schildrand
   size <- size - 0                               # Innenrand:
   res3 <- rbind( c(50, center+size, 50-size, center), c(50-size, center, 50, center-size),
                  c(50, center-size, 50+size, center), c(50+size, center, 50, center+size))
   res3 <- cbind(res3, lwd.mm = 10, color = 2)
   res <- rbind(res0o, res0i, res1, res2, res3); rownames(res) <- NULL
   res <- as.data.frame(res)
   colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
   res$color <- c("black", NA, "gray")[res$color]
   class(res) <- c(class(res), "segments"); res <- list(res)
   size <- size - 2
   res1b <- rbind( c(50, center+size), c(50-size, center),
                   c(50, center-size), c(50+size, center))
   res1b <- cbind(res1b, color = NA)
   class(res1b) <- c(class(res1b), "polygon"); res1b <- list(res1b) # Innenflaeche
   f <- size / 25
   res2b <- cbind( x = 50     + f*c( h <- c(- 9,  5, 12), rev(-h)),
                   y = center + f*c( h <- c( 16, 13,  6), rev(-h)), 
                   lwd.mm = f^0.6*6.5, color = 1)
   class(res2b) <- c(class(res2b), "spline"); res2b <- list(res2b)
   #res3 <- data.frame( x = c(27, 73), y = c(65, 65), txt = txt, 40, 1)
   #class(res3) <- c(class(res3), "text");   res3 <- list(res3)
   result <- c(res, res1b, res2b) #, res1) 
}  # ; show.icon.design(bike2)# bend.sign) #; bend.sign()
##:define [[bend.sign()]]##
#:392
h <- "internal generator function"
  
#398:
##define [[fir.tree()]]:##
fir.tree <- function(height = 1, width = 1, txt = ".....", t.cex.mm = 10){ 
  fac.x <- width * 100/60; fac.y <- height * 100/70
  # build standardized elements of pictogram
  res <- data.frame( 
    x = c(20, 40, 25, 45, 35, 50, 65, 55, 75, 60, 80),
    y = c(20, 40, 40, 60, 60, 80, 60, 60, 40, 40, 20) + 5,
    color = NA)
  class(res) <- c(class(res), "polygon")
  res.liste <- c(list(res))
  res <- data.frame( 
    x = c(55, 55, 45, 45),
    y = c(20, 10, 10, 20) + 5 ,
    color = "brown")
  class(res) <- c(class(res), "polygon")
  res.liste <- c(res.liste, list(res))
  # integrate effects of arg1 and arg2
  res.liste <- lapply( res.liste, function(xyc){
                         xyc$x <- fac.x * (xyc$x - 50) + 50; xyc} )
  res.liste <- lapply( res.liste, function(xyc){
                         xyc$y <- fac.y * (xyc$y - 50) + 50; xyc} )
  # append text element # res <- data.frame( x = 20, y = 2, txt = txt, t.cex.mm, color = "1") 
  res <- data.frame( x =  fac.x * (30 - 50) + 50, y = fac.y * (10 - 50) + 50, 
                     txt = txt, t.cex.mm, color = "1") #180327
   class(res) <- c(class(res), "text")
  res.liste <- c(res.liste, list(res))
  res.liste
}  # ; show.icon.design(fir.tree)
##:define [[fir.tree()]]##
#:398
#343:
##define [[find.smooth.curve()]] and [[find.brush.polygon()]]:##
#342:
##define [[bs.approx()]] and [[loess.approx()]]:##
bs.approx <- function(x, y, x.new, degree = 3, knots = 10, df = NULL){
  # library(splines)                                          # check package splines
  if(is.matrix(x) || is.data.frame(x)){y <- x[,2];x <- x[,1]} # check x,y input
  n <- length(x); idx <- order(x); x <- x[idx]; y <- y[idx]   # order by x
  y.new <- rep(NA, length(x.new))                             # init y result
  x.all <- c(x, x.new);   y.all <- c(y, y.new)                # combine old and new points
  basis <- splines::bs(x.all, degree = degree, df = df, knots = knots) # find design matrix 
  res <- lm(y.all ~ basis); coef.ok <- !is.na(res$coeff)      # estimate spline coefficients
  X     <- cbind(1, basis[  1:n ,])[,coef.ok]                 # extract design matrix for old 
  X.new <- cbind(1, basis[-(1:n),])[,coef.ok]                 # extract design matrix for new
  y.dach     <- X     %*% res$coefficients[coef.ok]           # compute spline of old points 
  y.new.dach <- X.new %*% res$coefficients[coef.ok]           # compute spline of new points
  list(cbind(x, y.dach), cbind(x.new, y.new.dach))            # compose result
}
loess.approx <- function(x, y, x.new, span = 0.6, degree = 2){
  smooth.curve <- loess(y ~ x, span = span, degree = degree)
  res.new <- predict(smooth.curve, x.new)
  res.old <- predict(smooth.curve, x)
  return(list(cbind(x, res.old), cbind(x.new, res.new)))
}
##:define [[bs.approx()]] and [[loess.approx()]]##
#:342
find.smooth.curve <- function(x.in, y.in, n.new = 100, method = c("bs", "loess")[1],
                              degree = 3, knots = 50, span = 0.75){
  if(is.matrix(x.in) || is.data.frame(x.in)){y.in <- x.in[,2]; x.in <- x.in[,1]} # check input
  n <- length(x.in)
  dx.min <- 0.1 * diff(range(x.in)) / length(x.in)            # set minimal dx of spline
  x.h <- cumsum(c(1, pmax(dx.min, (diff(x.in)^2 + diff(y.in)^2)^0.5))) # find x of spline
  x.new <- seq(x.h[1], x.h[n], length = n.new)                # find new x for spline eval 
  if( method == "bs" ){
    res.x <- bs.approx(x = x.h, y = x.in, x.new = x.new, degree = degree, knots = knots)
    res.y <- bs.approx(x = x.h, y = y.in, x.new = x.new, degree = degree, knots = knots)
  } else {
    res.x <- loess.approx(x = x.h, y = x.in, x.new = x.new, span = span, degree = min(2, degree))
    res.y <- loess.approx(x = x.h, y = y.in, x.new = x.new, span = span, degree = min(2, degree))
  }
  return(cbind(x = res.x[[2]][,2], y = res.y[[2]][,2]))       # compose result
}
find.brush.polygon <- function(x, y, hwd = 10){
  # find area along the polygon of points (x, y) with width 2*hwd 
  if(is.matrix(x) || is.data.frame(x)){ y <- x[,2]; x <- x[,1] } # check input
  dy <- diff(x); dx <- -diff(y); h <- length(dx)            # find orthogonal vectors to segments
  dx <- c(dx[1], 0.5 * (dx[-1] + dx[-h]), dx[h])            # find means of neighbours 
  dy <- c(dy[1], 0.5 * (dy[-1] + dy[-h]), dy[h])            #    of orthogonal vectors
  d <- hwd / sqrt(dx^2 + dy^2); dy <- d * dy; dx <- d * dx  # scale orthognal vectors
  xy <- rbind(cbind(x = x + dx, y = y + dy), cbind(x = rev(x - dx), y = rev(y - dy))) 
  rbind(xy, xy[1,])                                         # copy first point to the end
}
##:define [[find.smooth.curve()]] and [[find.brush.polygon()]]##
#:343
h <- "internal generator function"
  
#406:
##define [[comet()]]:##
comet <- function(comet.color = NA){ 
  t2xy <- function(t,radius,init.angle=0) {
        t <- t / 360
        t2p <- 2*pi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  center <- c(17, 30); fac <- 1.2 #; fac <- .2
  # c.color <- 4; s.color <- 3; comet.bg.color <- 7;  bg.color <- 0; t.color <- 5
  # c.color <- "gold"; s.color <- "red"; comet.bg.color <- "green"; 
  # t.color <- "gold";  bg.color <- "lightgrey"
  comet.bg.color <- "white"; t.color <- NA;  bg.color <- "white"; s.color <- "white"
  c.color <- comet.color
  res.liste <- NULL
  # aera of icon -----------------------------------------------------------------------------
  res <- data.frame(c(1, 99, 99, 1, 1), c(1, 1, 99, 99, 1), color = bg.color)
  class(res) <- c(class(res), "polygon"); res.liste <- c(res.liste, list(res)) 
  # aera of comet ----------------------------------------------------------------------------
  width <- 20 * fac
  res <- data.frame(center[1], center[2], center[1], center[2], width, color = comet.bg.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
  # letter C of Comet -----------------------------------------------------------------------
  width <- 3 * fac; radius <- 10 * fac
  P <- t2xy( 90:-45 , radius, 0)
  res <- data.frame(P$x + center[1], P$y + center[2]); res <- cbind(res, res, 
                    width, color = bg.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # C missing part
  P <- t2xy( 67.5:180 , radius, 0)
  res <- data.frame(P$x + center[1], P$y + center[2]); res <- cbind(res, res, 
                    width, color = c.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # C
  P <- t2xy( -180:-22.5 , radius, 0)
  res <- data.frame(P$x + center[1], P$y + center[2]); res <- cbind(res, res, 
                    width, color = c.color)
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # C
  # letter M of coMet ----------------------------------------------------------------------
  if( TRUE){
    width <- 2.5 * fac; shift <- c(1, 0.5) * 2 * fac; h <- 22.5 / 360 * 2 * pi
    xy <- cbind(c(-1, -1, 0, 1, 1), c(-1, 1, -1, 1, -1))
    xy <- xy %*% matrix( c( cos( h ), -sin(h), sin(h), cos(h)), 2, 2)
    x <- shift[1] + xy[,1] * 4 * fac; y <- shift[2] + xy[,2] * 4 * fac
    res <- data.frame(x[-5] + center[1], y[-5] + center[2], x[-1] + center[1], y[-1] + center[2],  
                      width, color = c.color)
    class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # M
  }
  # tail of comet with letter T of comeT --------------------------------------------------
  radius <- c(1, 5) * fac; width <- 3 * fac
  for(i in 1:6){
    radius <- radius + 10 * fac
    P1 <- t2xy(c(0, 22.5, 45), radius[1], 0)
    P2 <- t2xy(c(0, 22.5, 45), radius[2], 0)
    res <- data.frame( P1$x + center[1], P1$y + center[2], P2$x + center[1], P2$y + center[2], 
                       width, color = t.color)
    class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
    if(i == 1){
      res <- data.frame(P1$x[1] + center[1], P1$y[1] + center[2], 
                        P1$x[3] + center[1], P1$y[3] + center[2], 
                        width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
      res <- data.frame( P1$x + center[1], P1$y + center[2], P2$x + center[1], P2$y + center[2], 
                        width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
    } else {
      res <- data.frame( P1$x + center[1], P1$y + center[2], P2$x + center[1], P2$y + center[2], 
                        width, color = t.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
    }
    # T of comeT
    if(i == 5){
      h <- c(P2$x[2] - P1$x[2], P2$y[2] - P1$y[2]) * 2.5
      res <- data.frame( P1$x[2] + center[1], P1$y[2] + center[2], 
                         P2$x[2] + h[1] + center[1], P2$y[2] + h[2] +  center[2], 
                         width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
      h <- h * 0.6
      res <- data.frame( P1$x[2] + center[1], P1$y[2] + center[2], 
                         P1$x[2] - h[2] + center[1], P1$y[2] + h[1] +  center[2], 
                         width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb
      res <- data.frame( P1$x[2] + center[1], P1$y[2] + center[2], 
                         P1$x[2] + h[2] + center[1], P1$y[2] - h[1] +  center[2], 
                         width, color = c.color)
      class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # comet gb     
    }
  }
  # letter O of cOmet
  width <- 2.5 * fac; radius <- 7.5 * fac; shift <- c(1, 0.5) * 1.2 * fac
  P <- t2xy( 0:359 , radius, 0)
  res <- data.frame(P$x + shift[1] + center[1], P$y + shift[2] + center[2])
  res <- cbind(res, res, width, color = "white")
  class(res) <- c(class(res), "segments"); res.liste <- c(res.liste, list(res)) # O by white
  res.liste
} # ; print(comet());  show.icon.design(comet)
##:define [[comet()]]##
#:406
h <- "internal generator function"
  
#337:
##define [[coor.system()]]:##
coor.system <- function(xxx, yyy, pcex = 5, xrange, yrange, axes = FALSE){ 
  shift <- 0.5; lwd <- .25
  x <- c(.1, 3, 6, 9.9); y <- c(0, 0.1, -0.1, 0) + shift
  xy <- spline(x,y); x <- xy$x; y <- xy$y; n.1 <- length(x) - 1
  res <-            cbind(x[1:n.1], y[1:n.1], x[-1], y[-1], lwd)  # x line
  res <- rbind(res, cbind(y[1:n.1], x[1:n.1], y[-1], x[-1], lwd)) # y line
  res <- rbind(res, c(9.5, shift - 0.4, 9.9, shift, lwd),         # x arrow
                    c(9.4, shift + 0.3, 9.9, shift, lwd),         # x arrow
                    c(shift - 0.4, 9.5, shift, 9.9, lwd),         # y arrow
                    c(shift + 0.3, 9.4, shift, 9.9, lwd)) * 10    # y arrow
  res <- cbind(res, 1)
  class(res) <- "segments";   res.list <- list(res)
  if( !missing(xxx) ){
    if(missing(xrange)){ xrange <- range(xxx); xrange <- xrange + c(-0.1,0.1)*diff(xrange) }
    if(missing(yrange)){ yrange <- range(yyy); yrange <- yrange + c(-0.1,0.1)*diff(yrange) }
    xticks <- pretty(xrange); xticks <- xticks[ xrange[1] < xticks & xticks < xrange[2]]
    yticks <- pretty(yrange); yticks <- yticks[ yrange[1] < yticks & yticks < yrange[2]]
    xtickspos <- (xticks - xrange[1]) / (xrange[2] - xrange[1]) * 95 + 5
    ytickspos <- (yticks - yrange[1]) / (yrange[2] - yrange[1]) * 95 + 5
    res <- rbind(
             cbind(xtickspos, 5, xtickspos, 2, 2, 1)   # x ticks
            ,cbind(2, ytickspos, 5, ytickspos, 2, 1)   # y ticks
           )
    class(res) <- "segments"; res.list <- c(res.list, list(res))
    x <- c(-3, -1.5, 0, 1.5, 3); y <- c(-2.5, -0.5, 0.4, 1.5, 2.5) 
    xy <- spline(x,y); x <- xy$x; y <- xy$y
    xxx <- (xxx - xrange[1]) / (xrange[2] - xrange[1]) * 95 + 5
    yyy <- (yyy - yrange[1]) / (yrange[2] - yrange[1]) * 95 + 5
    res <- NULL; h <- length(x)
    for( i in seq(along = xxx) ){
      res <- rbind(res, 
             cbind(xxx[i] + x[-1], yyy[i] + y[-1], 
                   xxx[i] + x[-h], yyy[i] + y[-h], pcex * 0.2, NA), # points xxx, yyy
             cbind(xxx[i] + x[-1], yyy[i] + rev(y)[-1], 
                   xxx[i] + x[-h], yyy[i] + rev(y)[-h], pcex * 0.2, NA) # points xxx, yyy
      )
    }
    class(res) <- "segments"; res.list <- c(res.list, list(res))
    h <- length(xticks)  # x axis
    res <- data.frame( xtickspos, rep(-4, h), paste(xticks), rep(6, h),rep(1, h))
    class(res) <- c(class(res), "text"); if(axes) res.list <- c(res.list, list(res))
    h <- length(yticks); hh <- max(nchar(yticks)) # y axis
    res <- data.frame( rep(-2*hh, h), ytickspos, paste(yticks), rep(6, h),rep(1, h))
    class(res) <- c(class(res), "text"); if(axes) res.list <- c(res.list, list(res))
  } 
  res.list
}  ; coor.system()
##:define [[coor.system()]]##
#:337
##:define generator functions##
#:351
  if( icon %in% ls() ) icon <- get(icon)
  if( !is.function(icon) ) {
    cat("Error in puticon():", icon,"not implemented yet!")
    return()
  }
}
##:if [[is.character(icon)]] look for internal generator in [[puticon()]]##
#:311
  if( !is.function(icon) ){ # then: icon is not a generator function
    if( is.list(icon) ){
      txt <- c("icon <- function(){", deparse(icon), "}")
      h <- try(eval(parse(text = txt)))
      if("try-error" %in% class(h)){
        cat("Error in puticon(): definition of icon generating function failed\n")
        return( cat("Error in puticon(): argument icon must be a function or a list\n", 
                    "or an integer to specify a plotting character\n") )
      }
    } 
  } 
  icon.gen.args  <- formals(icon)
  n.icon.gen.args <- length(icon.gen.args)
##:check pictogram generating function in [[puticon()]]##
#:301
  
#302:
##extract additional args in [[puticon()]]:##
  args <- list(); n.args <- 0 # no additional args to concern
  if( n.icon.gen.args > 0 ){ 
    # extract relevant args of argument "..." 
    args <- list(...); n.args <- length(args)
    if( 0 < n.args ){ # match names of "..."-args and icons args   
      idx <- match(names(icon.gen.args), names(args))
      idx <- idx[!is.na(idx)]
      args <- args[idx]
      n.args <- length(args)
    }
    if( 0 == n.args ){ # explizit assignment of args if empty
      args <- list()
    } else { # expand args[[i]] cyclically to get n.items elements
      for( i in seq(n.args) ){ # for each position xy[i, ] ...
        if( length(args[[i]]) < n.items )
          args[[i]] <- rep(args[[i]], n.items)[1:n.items]
      }
    }
  } # relevant args found and expanded
##:extract additional args in [[puticon()]]##
#:302
  
#303:
##call icon without arguments in case of no args in [[puticon()]]:##
  if( 0 == n.args ){ 
    pic.sets <- try(do.call(icon, args)) 
    if("try-error" %in% class(pic.sets) ){ 
      cat("ERROR in call.icon.generator: generator function failed!\n"); return()
    }
    if( !is.list(pic.sets) || is.data.frame(pic.sets) ) pic.sets <- list(pic.sets)
  }
##:call icon without arguments in case of no args in [[puticon()]]##
#:303
  
#304:
##loop along the positions in [[puticon()]]:##
  type.set <- c("polygon", "segments", "text", "spline") # constant vector
  for(i in 1:n.items){        # loop along each of the positions: xy[i, ]   
    
#305:
##compute [[pic.sets]] in case of arguments in [[puticon()]]:##
    if( 0 < n.args ){
      # call pictogram generating function with element i of each of the args-vectors
      a <- lapply(args, function(x) x[[i]]) # remark: a is a list 
                                  # x[[i]] not x[i] #180514
      pic.sets <- try(do.call(icon, a))  # pic.sets is a list of descriptions
      if("try-error" %in% class(pic.sets) ){ 
        cat("!ERROR in call.icon.generator: generator function failed!\n"); return()
      }
      if( !is.list(pic.sets) || is.data.frame(pic.sets) ) pic.sets <- list(pic.sets)
    }
##:compute [[pic.sets]] in case of arguments in [[puticon()]]##
#:305
    
    for( pic.i in pic.sets ){ # loop along the elements of pic.sets
      # find type (last element of class vector) and dimensions of pic.i
      type <- rev(class(pic.i))[1]; type <- type[ type %in% type.set ]
      h <- dim(pic.i); rows.pic.i <- h[1]; cols.pic.i <- h[2] # s
      
#306:
##add missing colums in [[puticon()]]:##
if( type == "polygon" ){
  if( cols.pic.i < 3 ) pic.i <- cbind( pic.i, color.vec = NA )
}
if( type == "segments" ){
  if( cols.pic.i < 5 ) pic.i <- cbind( pic.i, lwd.mm = 10 )
  if( cols.pic.i < 6 ) pic.i <- cbind( pic.i, color.vec = NA )
}
if( type == "text" ){
  if( cols.pic.i < 4 ) pic.i <- cbind( pic.i, text.cex.mm = 10 )
  if( cols.pic.i < 5 ) pic.i <- cbind( pic.i, color.vec = NA )
}
if( type == "spline" ){
  if( cols.pic.i < 3 ) pic.i <- cbind( pic.i, lwd.mm = 10 )
  if( cols.pic.i < 4 ) pic.i <- cbind( pic.i, color.vec = NA )
}
##:add missing colums in [[puticon()]]##
#:306
      
#307:
##prepare colors in [[puticon()]]:##
# cat("--- prepare colors -------------")
# print(type); print(type.set); print(pic.i); print(cols.pic.i)
# extract color column: color.vec of group of descriptions
col.no <- c(3, 6, 5, 4)[ match( type, type.set ) ]
color.vec <- pic.i[, col.no]
# standardize format of color.vec
if(is.factor (color.vec)) color.vec <- as.character(color.vec)
if(is.numeric(color.vec)) color.vec <- c("white", palette("default"))[1+color.vec]
# <=> if(is.numeric(color.vec)) color.vec <- colornames[1 + color.vec]
# replace NA entries by argument color[i] of puticon call
res.color <- ifelse( is.na(color.vec), color[i], color.vec ) 
##:prepare colors in [[puticon()]]##
#:307
      
#308:
##transform coordinates in [[puticon()]]:##
      n.cols <- c(2, 4, 2, 2)[ match(type, type.set) ]
      adj.h <- matrix( adj, nrow(pic.i), n.cols, byrow = TRUE ) #180327
      res <- # shift because of design size
             # (pic.i[, 1:n.cols] - 100 * adj) *
             (pic.i[, 1:n.cols] + 100 * (adj.h - 1)) *  #180327
             # scaling: rescaling design size 0..100 -> 0..1: factor = 0.01
             0.01 * matrix(c(xsize[i], ysize[i]), rows.pic.i, n.cols, byrow = TRUE) + 
             # shift because of desired position
             matrix(xy[i,],                       rows.pic.i, n.cols, byrow = TRUE)
 
##:transform coordinates in [[puticon()]]##
#:308
      
#310:
##activate plotting commands in [[puticon()]]:##
      switch(type, 
        "polygon"  = polygon (res[,1], res[,2], col = res.color, xpd = NA, border = NA),
        "segments" = segments(res[,1], res[,2], res[,3], res[,4], col = res.color, 
                            # lwd = mm.to.lwd(pic.i[, 5]  * xsize[i] / xwcoor.pmm),
                              lwd = mm.to.lwd(pic.i[, 5]) * icon.cex[i] / 100,
                              xpd = NA),
        "text"     = text(res[,1], res[,2], as.character(pic.i[,3]),
                       col = res.color, cex = mm.to.cex(pic.i[,4]) * icon.cex[i] / 100, 
                       # adj = c(0,0), # adjust for text implemented by coordinates
                       xpd = NA),
        "spline"   = {
                       n.h <- length(res[,1]); z <- seq(n.h); n <- 10
                       xy.h <- cbind(spline( z, res[,1], n = n * n.h)$y, 
                                     spline( z, res[,2], n = n * n.h)$y)
                       lines(xy.h, col=res.color, 
                             lwd=mm.to.lwd(pic.i[,3]) * icon.cex[i] / 100, 
                             xpd = NA)
        }
      )
##:activate plotting commands in [[puticon()]]##
#:310
    }
  } # end of loop along the positions
##:loop along the positions in [[puticon()]]##
#:304
  return(NULL) 
}
#:275
##:define [[puticon()]]##
