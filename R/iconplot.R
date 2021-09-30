##define [[iconplot]]:##
#2:
iconplot <- function( data
  
#10:
##arguments of [[iconplot]]:##
#3:
##argument for xy-grouping:##
, grp.xy = 2 ~ 1
##:argument for xy-grouping##
#:3
#4:
##arguments for color and pictogram grouping:##
, grp.color = NULL
, grp.icon = NULL
##:arguments for color and pictogram grouping##
#:4
#36:
##argument for defining the set of colors:##
, colors
##:argument for defining the set of colors##
#:36
#38:
##argument for defining the set of pictograms:##
, icons 
##:argument for defining the set of pictograms##
#:38
#5:
##argument for transformation:##
, vars.to.factors
##:argument for transformation##
#:5
#33:
##argument for reversing y labeling:##
, panel.reverse.y = FALSE
##:argument for reversing y labeling##
#:33
#48:
##argument for panel space ratio:##
, panel.space.factor = 0.05
##:argument for panel space ratio##
#:48
#60:
##argument for panels proportional to sizes:##
, panel.prop.to.size = c(FALSE, FALSE)
##:argument for panels proportional to sizes##
#:60
#103:
##argument for panel margin:##
, panel.margin = 0.03
##:argument for panel margin##
#:103
#85:
##argument for panel frames:##
, panel.frame = TRUE
##:argument for panel frames##
#:85
#54:
##argument for panel adjustment:##
, panel.adjust = c(0.5, 0.5)
##:argument for panel adjustment##
#:54
#67:
##arguments to specify pictogram layout:##
, icon.horizontal = TRUE
, icon.stack.type = c("lt","lb","rt","rb")[1]
##:arguments to specify pictogram layout##
#:67
#77:
##arguments to specify pictogram layout:##
, icon.cex = NA
, icon.aspect = 1 # NA
, icon.stack.len = NA
##:arguments to specify pictogram layout##
#:77
#78:
##arguments to specify pictogram layout:##
, icon.space.factor = .3
, icon.grey.levels = 2
##:arguments to specify pictogram layout##
#:78
#115:
##arguments to specify pictogram layout:##
, icon.frame = TRUE
, icon.draft = TRUE
##:arguments to specify pictogram layout##
#:115
#104:
##argument to specify labels:##
, lab.side   = c("bl","br","tl","tr")[1]
##:argument to specify labels##
#:104
#118:
##argument to specify labels:##
, lab.parallel = c(TRUE, TRUE)
, lab.cex = 1
, lab.boxes = 2
, lab.color = c("#CCCCCC", "white")
, lab.type = c("expanded", "compact")[2]
##:argument to specify labels##
#:118
#127:
##argument to specify labels:##
, lab.n.max = c(20, 30)
##:argument to specify labels##
#:127
#132:
##argument to specify labels:##
, lab.legend = c("cols","rows","skewed","horizontal","vertical")[2]
##:argument to specify labels##
#:132
#9:
##argument to specify icons packer:##
, packer = c("icons", "numbers", "panel.legend", "stars")[1]
, panel.text = NULL
##:argument to specify icons packer##
#:9
#96:
##argument to specify graphics parameter:##
, mar = rep(1, 4)   # R default for mar: .1 + c(5, 4, 4, 2)
##:argument to specify graphics parameter##
#:96
#112:
##argument to specify graphics parameter:##
, main
##:argument to specify graphics parameter##
#:112
#6:
##argument for debugging:##
, verbose = !TRUE
##:argument for debugging##
#:6
, ...
##:arguments of [[iconplot]]##
#:10
){
  
#8:
##body of [[iconplot]]:##
#7:
##check arguments of [[iconplot]]:##
if(missing(data)){ 
  
#152:
##show arguments of [[iconplot]]:##
h <- noquote(cbind("arg names" = names(formals(iconplot)),
                   "default values" = as.character(formals(iconplot))))
h <- substring(h[ seq(h[,1]), ], 1, 56); rownames(h) <- rep(" ",nrow(h))
arg.list <- h
# old<-options(width = 95)
# print(noquote(cbind("\\\\ \\tt", arg.list[,1], "&", arg.list[,2])))
# options(old)
##:show arguments of [[iconplot]]##
#:152
 
  
#1:
##show info about version and arguments of [[iconplot()]]:##
cat("iconplot, version 170815, list of arguments")
#152:
##show arguments of [[iconplot]]:##
h <- noquote(cbind("arg names" = names(formals(iconplot)),
                   "default values" = as.character(formals(iconplot))))
h <- substring(h[ seq(h[,1]), ], 1, 56); rownames(h) <- rep(" ",nrow(h))
arg.list <- h
# old<-options(width = 95)
# print(noquote(cbind("\\\\ \\tt", arg.list[,1], "&", arg.list[,2])))
# options(old)
##:show arguments of [[iconplot]]##
#:152
 (144)
print(arg.list)
##:show info about version and arguments of [[iconplot()]]##
#:1
  return()
}
args <- list(...)
##:check arguments of [[iconplot]]##
#:7
#18:
##check arguments of [[iconplot]]:##
limit <- 9999 
limit.msg <- paste("error in iconplot: number of observations of table exceeds",
                   "the limit of", limit)
if(is.table(data) && limit < sum(data) ){
  print(limit.msg); return("sorry, no graphical output")
}
if(!is.table(data) && ( is.matrix(data) | is.data.frame(data) ) && 
   limit < nrow(data) ){
     print(limit.msg); return("sorry, no graphical output")
}
if(!is.table(data) && !( is.matrix(data) | is.data.frame(data) )&& 
   limit < sum(data) ){
     print(limit.msg); return("sorry, no graphical output")
}
##:check arguments of [[iconplot]]##
#:18
#128:
##check arguments of [[iconplot]]:##
if(1 == length(lab.n.max)) lab.n.max <- c(lab.n.max, 30) #add number of margin level names
if(2 == length(lab.n.max)) lab.n.max <- c(lab.n.max, 20) #add number of legend labels
if(missing(lab.legend) && 3 == length(lab.parallel)){ 
  lab.legend <- c("cols", "rows")[2] 
} # to be compatible to older versions
lab.parallel <- rep(lab.parallel, 2)[1:2]
##:check arguments of [[iconplot]]##
#:128
#133:
##check arguments of [[iconplot]]:##
if( is.numeric(lab.legend) ){
  lab.legend <- max(1, min(5, lab.legend[1]))
  lab.legend <- c("cols","rows","skewed","horizontal","vertical")[lab.legend]
}
##:check arguments of [[iconplot]]##
#:133
#154:
##check arguments of [[iconplot]]:##
if( missing(data) ){ 
  
#152:
##show arguments of [[iconplot]]:##
h <- noquote(cbind("arg names" = names(formals(iconplot)),
                   "default values" = as.character(formals(iconplot))))
h <- substring(h[ seq(h[,1]), ], 1, 56); rownames(h) <- rep(" ",nrow(h))
arg.list <- h
# old<-options(width = 95)
# print(noquote(cbind("\\\\ \\tt", arg.list[,1], "&", arg.list[,2])))
# options(old)
##:show arguments of [[iconplot]]##
#:152
  return(arg.list)
}
data.call <- deparse(substitute(data))
if(!missing(grp.color)){
  grp.color.call <- as.character(substitute(grp.color))
  ## cat("grp.color.call:", grp.color.call)
  grp.color <- as.character(grp.color.call)
  ## cat("grp.color", grp.color)
} else grp.color.call <- NULL
if(!missing(grp.icon  )){
  grp.icon <- as.character(substitute(grp.icon)) 
}
arguments <- c( #  data = deparse(substitute(data)),
  grp.xy = as.character(grp.xy), # colors = as.character(colors),
  grp.color = as.character(grp.color.call), # icons = as.character(icons),
  pa.space = paste(collapse = ",",as.character(panel.space.factor)),
  panel.adj = panel.adjust,
  stacks = paste(collapse = ",", 
                 c(icon.stack.type, c("hori","verti")[2-icon.horizontal])),
  cex = icon.cex,
  asp = icon.aspect,
  space = paste(icon.space.factor, collapse=","),
  panel.margin = paste(panel.margin, collapse=","),
  labels = paste(collapse = ",", c(lab.side, 
                 c("parallel", "upright")[2-lab.parallel[1]],
                 c("parallel", "upright")[2-lab.parallel[2]]))
)
arguments <- paste(names(arguments), as.vector(arguments), sep = " : ")
if((length(arguments) %% 2) == 1) arguments <- c(arguments, " ")
arguments <- matrix(arguments, ncol = 2, byrow = !TRUE)
arguments <- paste( arguments[,1], arguments[,2], sep = "  //  ")
arguments <- paste( arguments, collapse = "\n")
if(verbose) cat("=======\n", arguments, "=======\n")
##:check arguments of [[iconplot]]##
#:154
#14:
##define local functions:##
#13:
##define find.xy.groupings:##
find.xy.groupings <- function(groupings, verbose = FALSE){
  g.o <- grps <- groupings
  if(verbose){ cat("----- input:", as.character(groupings)) }
  if(is.character(grps) && 0 < length(names(grps)) 
      && all(names(grps) %in% c("x","y")) ){ # convert named char vec to list   
    x <- unlist(strsplit(grps["x"], "[+*]")); if(any(is.na(x))) x <- "0"
    y <- unlist(strsplit(grps["y"], "[+*]")); if(any(is.na(y))) y <- "0"
    grps <- list( x = as.vector(x), y = as.vector(y) ) 
    grps <- lapply( grps, function(x) gsub(" ","",x))
  } 
  if(is.list(grps)){ # argument is in the desired form
    if( 0 == length(grps) || ! all(names(grps) %in% c("x","y")) ){ 
      print(paste("no correct definition of groups:",g.o)); return(NULL) 
    } else { "ok" }
  } else { # cases: formal maybe as character
    if( is.character(grps) ){ # formal in char form
      if( 0 == length(grep("[~]", grps))){ 
        print(paste("no correct formula:",g.o)); return(NULL)
      }
      grps <- unlist(strsplit(grps, "[~]"))
      grps[ grps == "" ] <- "0"
      if( 2 == length(grps)){ grps <- c("~", grps) 
      } else { print(paste("no correct formula:",g.o)); return(NULL) }     
    } 
    grps <- gsub(" ","", as.character( grps ))
    grps <- if( length(grps) == 2 ) c(grps[2], "0") else grps[3:2];  
    grps <-  strsplit(grps, "[+*]"); names(grps) <- c("x", "y")
  }
  if(verbose){ cat("----- result:"); print(grps) }
  grps
} #; find.xy.groupings("0 ~ Mean+Penalty+Region")# "0~asdfa+br")
##:define find.xy.groupings##
#:13
##:define local functions##
#:14
#16:
##define local functions:##
expand.table.short <- function(x){
   combinations <- expand.grid(dimnames(x)); num <- as.vector( x )
   # if(0 == length(num)||any(is.na(num))||any(num<0)||all(round(num)==0))
   #   return(NULL)
   combinations <- combinations[rep(seq(along = num), num),, drop=FALSE]
   rownames(combinations) <- seq( nrow(combinations) )
   combinations
} # ; expand.table.short(as.table(12))
##:define local functions##
#:16
#43:
##define local functions:##
#281:
##define [[transform.color.to.rgb.integer()]]:##
transform.color.to.rgb.integer <- function(x, to = c("#rrggbb", "rgb"), 
                                           debug = FALSE){
  dim.x <- n <- length(x)
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
#:281
##:define local functions##
#:43
#47:
##define local functions:##
find.n.intervals <- function(ints, props = FALSE, vmin = 0, vmax = 1,
                             panel.space.factor = 0.1){ 
  # print(ints); print(props) # 161212
  ints <- pmax(ints, .03 * max(1, sum(ints)))  # limit for security .1* #161212
  props <- if( 0 < props ) ints^props else rep(1, ints[1])       # props is 
    # a number of intervals or a vector of proportions of the desired sizes
  cum.rel.dist <- cumsum(props / sum(props))
  n <- length(props); space <- (vmax - vmin)
  p.margins <- (space / n) * panel.space.factor / 2
  space <- space - 2 * n * p.margins
  lims <- c(vmin, vmin + cum.rel.dist * space + 2 * p.margins * ( 1:n ) )
  mids <- 0.5 * (lims[-1] + lims[-(n+1)]); sizes <- diff(lims)
  mins <- lims[-(n+1)] + p.margins;        maxs <- lims[ -1   ] - p.margins
  return(cbind(mins = mins, maxs = maxs, mids = mids, p.margins = p.margins,
               sizes = sizes, lo.limit = lims[-(n+1)], up.limit = lims[-1]))
} #;  find.n.intervals(1, FALSE, 1, 2, .9)
if(!exists("raster.to.greys")){ 
#233:
##define [[raster.to.greys()]]:##
raster.to.greys <- function(pic, grey.levels = c(0.05, 0.95),  # 2 :: black + white  
                            reduce = TRUE, n.icons = 1){
  # prepare mat: cat("hallo: raster.to.greys", grey.levels); print(table(as.vector(pic)))
  
#235:
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
#:235
  
#236:
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
    fn <- if(length(table(mat)) < 3) max else mean ## mean !!!!!! rounding effect
    mat.new <- apply(mat.new, c(2,4), fn) # 190815
    d.mat <- dim(mat.new); mat <- pmin(1,mat.new); dim(mat) <- d.mat 
  }
}
##:reduce size of [[mat]]##
#:236
  mat <- unclass(mat); range.mat <- range(mat)
  # cat("mat::"); print(table(as.vector(mat))); cat("grey.levels:", (grey.levels))
  # expand a single fractional grey.level value: -1 < grey.levels[1] < 1
  if( length(grey.levels) == 1 && abs(grey.levels) < 1 ){ 
    # negative fraction induce coloring "color"+"black"
    if(0 <= grey.levels){ # positive fraction induces coloring "color"+"white":
      grey.levels <- c(grey.levels,  1)
    } else {              # negative fraction induces coloring "black"+"color": 
      grey.levels <- c(0, -grey.levels)
    }
    # cat("one value", grey.levels)
  }
  # case of two values in (0,1)
  if( 2 == length(grey.levels) && max(abs(grey.levels) <= 1) ){
    greys <- sort(abs(grey.levels))
    
#242:
##find levels if the two [[greys]] are in (0,1):##
# generate matrix of levels
levs <- 1 + (greys[1] < mat) + (greys[2] < mat)   
dim(levs) <- d.mat  
##:find levels if the two [[greys]] are in (0,1)##
#:242
    # cat("two levels given"); print(table(levs))
    
#234:
##raster.to.greys(): compose return values and return:##
  levs <- sapply( mat, function(x) sum( x >= greys ) ) 
  if(max(levs) == 2) levs <- levs + 1 # if 2 cols only use one color and white
  if( 2 == length(h <- unique(levs)) ) levs <- 2 + ( levs == max(h) ) 
  # cat("raster.to.greys-end, greys", greys);  print(table(levs))
  dim(levs) <- d.mat; return(list(levs, greys))
##:raster.to.greys(): compose return values and return##
#:234
    return(list(levs, greys)) # nicht notwendig
  }
  # case of number of grey levels are given
  if( 1 == length(grey.levels) && 0 == (grey.levels %% 1) ){ 
    n.greys <- (1 + abs(grey.levels)) # add 1 for white
    if( grey.levels > 0 ){ # type 1 because of positive value: equal spacing
      # h <- range.mat + c(0.8, -0.2) * diff(range.mat) / n.greys
      # greys <- c(range.mat[1], seq(h[1], h[2], length = n.greys - 1))
      # greys <- seq(range.mat[1], range.mat[2], length = n.greys)[-c(1, n.greys)] / (n.greys - 2)
      # delta <- 1 / grey.levels; greys <- seq(0.3, 0.7, length = grey.levels)
      h <- quantile(mat, c(0.0, 1 - 1 / grey.levels)) #190819
      if( h[1] == h[2] ) h[2] <- quantile(mat, 1)
      greys <- seq(h[1], h[2], length = grey.levels)
      # cat("equal spacing, n.greys", n.greys, "greys", greys, "grey.levels", grey.levels)
    } else { # type 2 because of negative value: equal frequencies in classes
      greys <- seq(range.mat[1], range.mat[2], length = n.greys)#[2:n.greys]
      # extract relevant grey values and count extremes one time only 
      greys.obs <- c(range.mat[1], mat[range.mat[1] < mat & mat < range.mat[2]], range.mat[2])
      greys.obs <- mat
      # find quantiles of observed grey values
      # greys <- c(range.mat[1], quantile(greys.obs, greys)) 
      greys <- c(range.mat[1], quantile(greys.obs, greys)[-1]) 
      # remove NAs (usually not relevant) and remove multiple greys
      greys <- unique(greys[ !is.na(greys) ])
      # cat("equal frequencies, n.greys", n.greys, "greys", greys)
    }
  } else {                   # find greys by quantiles of grey rates
    greys <- unique( pmin(1, pmax(0, sort(c(0, 1, abs(grey.levels))))) )
    if(grey.levels[1] < 0){
      greys <- unique( quantile(mat, greys) )
      # handle strange cases:
      if(length(greys) == 1 && greys[1] == 0) greys <- c(greys, 1)
      if(length(greys) == 1 && greys[1] == 1) greys <- c(0, greys)
      if( (h <- length(greys)) > 2 && greys[h] == 1) greys <- greys[ -h ]
    }
  }  # generate matrix of levels:
  
#234:
##raster.to.greys(): compose return values and return:##
  levs <- sapply( mat, function(x) sum( x >= greys ) ) 
  if(max(levs) == 2) levs <- levs + 1 # if 2 cols only use one color and white
  if( 2 == length(h <- unique(levs)) ) levs <- 2 + ( levs == max(h) ) 
  # cat("raster.to.greys-end, greys", greys);  print(table(levs))
  dim(levs) <- d.mat; return(list(levs, greys))
##:raster.to.greys(): compose return values and return##
#:234
} 
##:define [[raster.to.greys()]]##
#:233
 }
##:define local functions##
#:47
#71:
##define local functions:##
size.usr.to.mm <- function(size, horizontal = TRUE){
  # transforms x- or y-length to mm
  # usr <- par()$usr; pin <- par()$pin      # exists globally
  usr <- usr[(3:4) - 2 * horizontal]
  mm  <- size / (usr[2]-usr[1]) * pin[2 - horizontal] * 25.4
}
size.mm.to.usr <- function(mm, horizontal = TRUE){
  # transforms mm to x- or y-length 
  # usr <- par()$usr; pin <- par()$pin      # exists globally
  usr <- usr[(3:4) - 2 * horizontal]
  size <- mm * (usr[2]-usr[1]) / pin[2 - horizontal] / 25.4
}
##:define local functions##
#:71
#86:
##define local functions:##
place.icons <- function( job, dm.of.job 
  , icon.stack.type = c("l", "b", "r", "t")[1:2], icon.stack.len
  , verbose = TRUE, frame = FALSE
){
  n.items <- nrow(dm.of.job) # -------------- find number of icons 
  # find x and y values of icons
  if( !any(c("t", "b") %in% icon.stack.type)){
    icon.stack.type <- c("b", icon.stack.type)  
  }
  if( !any(c("r", "l") %in% icon.stack.type)){
    icon.stack.type <- c( "l",  icon.stack.type)  
  }
  if(icon.horizontal){ # ------------------------- icon.horizontal stacks
    # x : fill from left or right
    x <- ((0:(n.items-1)) %% icon.stack.len) * dx.pic.area
    if("l" %in% icon.stack.type){ x <- job[,"xmins"] + x; xfac <-  1 
    } else {                     x <- job[,"xmaxs"] - x; xfac <- -1  }
    # y : fill from top or bottom
    y <- floor((0:(n.items-1)) / icon.stack.len) * dy.pic.area
    if("b" %in% icon.stack.type){ y <- job[,"ymins"] + y; yfac <-  1
    } else {                     y <- job[,"ymaxs"] - y; yfac <- -1  }
  } else { # -------------------------------- vertical stacks
    # y : fill from top or bottom
    y <- ((0:(n.items-1)) %% icon.stack.len) * dy.pic.area
    if("b" %in% icon.stack.type){ y <- job[,"ymins"] + y; yfac <-  1  
    } else {                     y <- job[,"ymaxs"] - y; yfac <- -1  }
    # x : fill from left or right
    x <- floor((0:(n.items-1)) / icon.stack.len) * dx.pic.area
    if("l" %in% icon.stack.type){ x <- job[,"xmins"] + x; xfac <-  1
    } else {                     x <- job[,"xmaxs"] - x; xfac <- -1  }
  }
  if( verbose ){ 
#88:
##debugging statements for [[place.icons()]]:##
show.frame <- TRUE; show.number <- TRUE # - for debugging
if(show.frame)  rect(job[,"xmins"], job[,"ymins"],
                     job[,"xmaxs"], job[,"ymaxs"])
if(show.number) text(job[,"xmaxs"], job[,"ymins"],
                     as.character(n.items), 
                     adj = c(1, 0), col = "blue", cex = 1)
if(show.number) text(job[,"xmins"], job[,"ymins"],
                     paste("j:", job[,".job.no"]), 
                     adj = c(0, 0), col = "blue", cex = 1)
##:debugging statements for [[place.icons()]]##
#:88
  }
  if(frame) rect(job[,"xmins"], job[,"ymins"], job[,"xmaxs"], job[,"ymaxs"]) 
  x <- cbind(x,  x + xfac * dx.pic); y <- cbind(y, y + yfac * dy.pic)
  
#87:
##modify because of fractional part of [[place.icons]]:##
x0 <- pmin(x[,1], x[,2]); x1 <- pmax(x[,1], x[,2])
y0 <- pmin(y[,1], y[,2]); y1 <- pmax(y[,1], y[,2])
f  <- dm.of.job[, ".fraction"]^0.5
if( !icon.horizontal & "b" %in% icon.stack.type){
  if("s" %in% icon.stack.type){ ### || is.matrix(icons[[1]])){ 
    y1 <- y0 + (y1 - y0) * f          # "s"hrinking rectangle 
    if("r" %in% icon.stack.type) x0 <- x1 - (x1 - x0) * f else
                                x1 <- x0 + (x1 - x0) * f
  } else { y1 <- y0 + (y1 - y0) * f^2 }
}
if( !icon.horizontal & "t" %in% icon.stack.type){
  if("s" %in% icon.stack.type){ ### || is.matrix(icons[[1]])){ 
    y0 <- y1 - (y1 - y0) * f          # "s"hrinking rectangle 
    if("r" %in% icon.stack.type) x0 <- x1 - (x1 - x0) * f else
                                x1 <- x0 + (x1 - x0) * f
  } else { y0 <- y1 - (y1 - y0) * f^2 }
}
if(  icon.horizontal & "l" %in% icon.stack.type){ # "s"hrinking rectangle 
  if("s" %in% icon.stack.type){ ### || is.matrix(icons[[1]])){  
    x1 <- x0 + (x1 - x0) * f
    if("t" %in% icon.stack.type) y0 <- y1 - (y1 - y0) * f  else 
                                y1 <- y0 + (y1 - y0) * f
  } else { x1 <- x0 + (x1 - x0) * f^2 }
}
if(  icon.horizontal & "r" %in% icon.stack.type){  # "s"hrinking rectangle 
  if("s" %in% icon.stack.type){ ### || is.matrix(icons[[1]])){ 
    x0 <- x1 - (x1 - x0) * f
    if("t" %in% icon.stack.type) y0 <- y1 - (y1 - y0) * f  else 
                                y1 <- y0 + (y1 - y0) * f
  } else { x0 <- x1 - (x1 - x0) * f^2 }
}
##:modify because of fractional part of [[place.icons]]##
#:87
  return(data.frame(.x0 = x0, .y0 = y0, .x1 = x1, .y1 = y1,
                 .color = dm.of.job[, ".color"], .pic = dm.of.job[, ".icon"]))
}
##:define local functions##
#:86
#109:
##define local functions:##
#239:
##define [[greys.to.col.pic()]]:##
greys.to.col.pic <- function(levs, col, invert = FALSE, 
                             set.black.and.white = FALSE, simple = FALSE){
  # print("hallo: greys.to.col.pic"); cat("simple", simple)
  d.mat <- dim(levs); n <- max(levs) #; cat("simple", simple)
  if(simple){
    
#243:
##find pic if [[simple]] is set:##
# set colors based on col
colors <- c("#000000", col, "#FFFFFF")
# if(any(1 == levs)) levs <- levs + 1 # zu klein ??
#   print(table(as.vector(levs)))
# cat("colors", colors)
# build pictogram  
pic <- matrix(colors[levs], ncol = d.mat[2]) 
##:find pic if [[simple]] is set##
#:243
    return(as.raster(pic))
  } #;  cat("greys.to.col.pic")
  # inversion of levels
  if(invert) levs <- n + 1 - levs
  # find colors based on col
  rgb.col1 <- col2rgb(col) / 255; rgb.col2 <- 1 - rgb.col1
  n1 <- round(n / 2) ; n2 <- n - n1 # n1 <- round(mean(rgb.col1) * n)
  f1 <- ((1:n1) - 1/2) / n1; f2 <- ((n2:1) - 1/2) / n2
  rgb.col1 <-     cbind(f1 * rgb.col1[1], f1 * rgb.col1[2], f1 * rgb.col1[3])
  rgb.col2 <- 1 - cbind(f2 * rgb.col2[1], f2 * rgb.col2[2], f2 * rgb.col2[3])
  rgb.col <- rbind(rgb.col1, rgb.col2)
  colors <- rgb(rgb.col)
  if(0 < set.black.and.white){
    if(set.black.and.white <= 1) colors[1]              <- "#000000"
    if(1 <= set.black.and.white) colors[length(colors)] <- "#FFFFFF"
  } ; # build pictogram  
  pic <- matrix(colors[levs], ncol = d.mat[2]); as.raster(pic)
} 
##:define [[greys.to.col.pic()]]##
#:239
##:define local functions##
#:109
#141:
##define local functions:##
find.xlab.coor <- function(labs, y0, skip = 0){
  # global variables: cin, lab.legend, x.per.row, dx, dy, x0
  idx <- which( lab.legend == c("rows", "skewed", "horizontal"))
  cos.fac <- c(1, 0.7071, 0)[idx]^2
  width.labs <- (max( strwidth(labs, cex = lab.cex) ) + dx/2) * cos.fac 
  if(idx == 2) width.labs <- (strwidth("MM", cex = lab.cex) + dx/2) * cos.fac 
  width.labs <- rep(width.labs, length(labs) ) 
  lab.idx <- seq(n <- length(labs)) - 1
  xr0 <- x0 + ( lab.idx * 1.2 * dx + c(0, cumsum(width.labs))[-(n+1)] ) / (1 + skip)
  xr1 <- xr0 + dx; yr1 <- y0 + dy
  xlab <- xr0 + 1.5 * x.per.row
  xlab <- xr0 + c(-0.7071 * dx, dx/2, dx)[idx]
  xlab <- xlab + 2 * dx * cos.fac # x.per.row * (1 - cos.fac) 
  ylab <- y0 + c(0, 0.6, 0.9)[idx] * y.per.row # horizontal old: 0.8 instead of 0.9
  if( missing(y0) ) { usr <- par()$usr; y0 <- usr[3] - 0.1 * diff(usr[3:4]) }
  cbind(xr0 = xr0, yr0 = y0, xr1 = xr1, yr1 = yr1, xlab = xlab, ylab = ylab)
}       
##:define local functions##
#:141
#153:
##define local functions:##
show.obj <- function(x){ 
  name <- deparse(substitute(x))
  if(is.list(x) || is.matrix(x)) {
    cat(name,":"); print(x)
  } else { cat(name,":", x) }
}
##:define local functions##
#:153
#363:
##define local functions:##
#355:
##define [[call.icon.generator()]]:##
call.icon.generator <- function(icons, xy = c(0,0), dxy = c(1, 0.5)[1], 
                               color = "red", pic.args = 50, ...){
  # check structure of xy and dxy
  xy <- xy[1:2]; dxy <- c(dxy, 1)[1:2]
  if(is.character(icons)){  # case: name of an internal icon generator
    
#356:
##look for internal generator in [[call.icon.generator()]]:##
  
#317:
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
#:317
  
#320:
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
#:320
  
#327:
##define [[cross.simple()]]:##
cross.simple <- function(){  # print("in cross")
  res <- rbind( c( 05, 05, 95, 95, lwd.mm = 10, NA), 
                c( 05, 95, 95, 05, lwd.mm = 10, NA),
                c( 50, 50, 50, 50, lwd.mm = 30, 2) ) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[cross.simple()]]##
#:327
  
#328:
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
#:328
  
#329:
##define [[circle.simple()]]:##
circle.simple <- function(){ # print("in circle.simple")
  res <- rbind( c( 50, 50, 50, 50, lwd.mm = 100, NA)) 
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm", "color")
  class(res) <- "segments"; res
}
##:define [[circle.simple()]]##
#:329
  
#330:
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
#:330
  
#333:
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
#:333
  
#334:
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
#:334
  
#336:
##define [[nabla()]]:##
nabla <- function(){ 
  res <- rbind( c( 05, 95, 50, 05, 10), c( 50, 05, 95, 95, 10),
                c( 95, 95, 05, 95, 10) );  class(res) <- "segments"
  colnames(res) <- c("x0", "y0", "x1", "y1", "lwd.mm"); res
} # ; nabla()
##:define [[nabla()]]##
#:336
  
#365:
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
  
#366:
##define body of [[walkman]]:##
  x <- c(cos(balpha), sin(balpha)) * scale.xy
  ba <- c(0,0); be <- ba + x
  bal <- lwd * lw.unit * 1.7; bac <- col 
  seg.mat <- cbind(a=ba[1], b=ba[2], c=be[1], d=be[2], e=bal)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, bac)
##:define body of [[walkman]]##
#:366
  
#368:
##define head of [[walkman]]:##
  h <- be + ( be - ba) * .75; hl <- lwd * lw.unit * 1.6; hc <- col
  seg.mat <- cbind(a=h[1], b=h[2], c=h[1], d=h[2], e=hl)
  segs.set <- rbind(segs.set, seg.mat); col.set <- c(col.set, hc)
##:define head of [[walkman]]##
#:368
  
#367:
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
#:367
  
#369:
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
#:369
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
#:365
  
#373:
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
#:373
  
#374:
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
#:374
  
#379:
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
#:379
  
#375:
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
#:375
  
#383:
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
#:383
  
#391:
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
#:391
  
#392:
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
#:392
  
#393:
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
#:393
  
#394:
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
#:394
  
#400:
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
#:400
  if( icons %in% ls() ) icons <- get(icons)
  if( !is.function(icons) ) {
    cat("Error in call of call.icon.generator in iconplots():\n  ", 
        icons,"not implemented yet!")
    return()
  }
##:look for internal generator in [[call.icon.generator()]]##
#:356
  }
  if( is.function(icons) ){ # case: user defined generator function ---
    
#357:
##find set [[pic.sets]] by calling [[icons()]] in [[call.icon.generator()]]:##
    # check arguments of icons # print(pic.args)
    icon.gen.args  <- formals(icons); n.icon.gen.args <- length(icon.gen.args)
    # Are there some arguments of icons to modify the pictogram? -----------
    args <- list()                 # no additional args to concern -----------
    if( n.icon.gen.args > 0 ){     
      # at first check whether there are arguments in "..." to use for icons
      ppp.args <- list(...)
      if( 0 < length(ppp.args) ){ 
        # find args of "..." which can be used in icons()
        idx <- match(names(ppp.args), names(icon.gen.args))
        args <- ppp.args[!is.na(idx)]
        # remove args of icon.gen.args that have been found in "..."
        idx <- match(names(icon.gen.args), names(args))
        icon.gen.args <- icon.gen.args[is.na(idx)] 
      }
      # append args given via grp.design and stored in pic.args
      if( (h1 <- length(pic.args)) > 0 && (h2 <- length(icon.gen.args)) > 0 ){
        pic.args <- pic.args[1 : min(h1, h2)]
        args <- c( args, as.list(pic.args) )     
      } 
      if( 0 == length(args) ) args <- list() # explizit assignment of args if empty
    } # => relevant args found, expanded and stored in args
    # conversion of factors to numbers 
    for( i in seq(along = args)){ 
      h <- args[[i]]
      if(is.factor(h)){ # case: levels are numbers
        nums <- try(as.numeric(levels(h)))
        if( (!"try-error" == class(nums)) && (!any( is.na(nums) ) ) ){ # nums is number 
          args[[i]] <- nums[as.numeric(h)] 
        } else {  # case: levels are no numbers
                  # job of generator function: args[[i]] <- as.numeric(h) / length(levels(h))    
        }
      }
    }
    # call icons to build an concrete pictogram blank
    pic.sets <- try(do.call(icons, args)) 
    if("try-error" %in% class(pic.sets) ){ 
      cat("ERROR in call.icon.generator: generator function failed!\n"); return()
    }
    if( !is.list(pic.sets) || is.data.frame(pic.sets) ) pic.sets <- list(pic.sets)
##:find set [[pic.sets]] by calling [[icons()]] in [[call.icon.generator()]]##
#:357
  } 
  # now pic.sets stores the actual pictogram blank
  # check list structure of the pictogram to be build
  if( !is.list(pic.sets) || is.data.frame(pic.sets) ) pic.sets <- list(pic.sets)
  
#358:
##find parameters for constructing pictograms in [[call.icon.generator()]]:##
  colornames <- c("white", "black", "red", "green", "blue",
                  "lightblue", "magenta", "yellow", "grey")
  if(is.numeric(color)) color <- colornames[1 + color]
  h <- par(); usr <- h$usr; pin <- h$pin; cin <- h$cin 
  xwcoor.pmm <- diff(usr[1:2]) / pin[1] / 25.4 # xrange per mm of plot-region
  ywcoor.pmm <- diff(usr[3:4]) / pin[2] / 25.4 # yrange per inch of plot-region
  # correction because of uncorrect display sizes may by processed by:
  dev.fac <- c(0.8, 1, 1)[ c(dev.cur() == c("postscript", "pdf"), TRUE) ][1]
  aspect     <- xwcoor.pmm / ywcoor.pmm  # width / height concerning plot-region
  # if(length(icon.cex) < n.items) icon.cex <- rep(icon.cex, n.items)[1:n.items]
  # xrange of symbol of size because of 
  #   "xsize <- icon.cex * xwcoor.pmm" => "icon.cex == xsize / xwcoor.pmm dxy[1]" => 
  icon.cex <- dxy[1] / xwcoor.pmm;   xsize  <- icon.cex * xwcoor.pmm
  ysize  <- icon.cex * ywcoor.pmm         # yrange of symbol of size icon.cex 
  xwcoor.pcm <- 10 * xwcoor.pmm         # xrange per cm of plot-region 
  mm.to.lwd <- function(lwd.mm) lwd.mm * 3.787878 * dev.fac 
  mm.to.cex <- function(text.cex.mm) text.cex.mm / (cin[1] * 25.4)
##:find parameters for constructing pictograms in [[call.icon.generator()]]##
#:358
  type.set <- c("polygon", "segments", "text", "spline")
  for( pic.i in pic.sets ){ # loop along the elements of list "pic.sets"
    # plot segments, polygons and texts in [[call.icon.generator()]]
    
#359:
##add missing infos of [[pic.i]] in [[call.icon.generator()]]:##
# find type (last element of class vector) and dimensions of pic.i
type <- rev(class(pic.i))[1]; type <- type[ type %in% type.set ]
h <- dim(pic.i); rows.pic.i <- h[1]; cols.pic.i <- h[2] 
# add missing infos
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
##:add missing infos of [[pic.i]] in [[call.icon.generator()]]##
#:359
    
#360:
##prepare colors for use in [[call.icon.generator()]]:##
# extract color column: color.vec of group of descriptions
col.no <- c(3, 6, 5, 4)[ match( type, type.set ) ]
color.vec <- pic.i[, col.no]
# standardize format of color.vec
if(is.factor (color.vec)) color.vec <- as.character(color.vec)
if(is.numeric(color.vec)) color.vec <- colornames[1 + color.vec]
# replace NA entries by argument color of puticon call
res.color <- ifelse( is.na(color.vec), color, color.vec ) 
##:prepare colors for use in [[call.icon.generator()]]##
#:360
    
#361:
##transform coordinates in [[call.icon.generator()]]:##
n.cols <- c(2, 4, 2, 2)[ match(type, type.set) ]
adj.h <- matrix( 1, nrow(pic.i), n.cols, byrow = TRUE ) #180327
res <- # shift because of design size
       # (pic.i[, 1:n.cols] - 100 * adj) *
       (pic.i[, 1:n.cols] + 100 * (adj.h - 1)) *  #180327
       # scaling: rescaling design size 0..100 -> 0..1: factor = 0.01
       0.01 * matrix(c(xsize, ysize), rows.pic.i, n.cols, byrow = TRUE) + 
       # shift because of desired position
       matrix(xy,                     rows.pic.i, n.cols, byrow = TRUE)
##:transform coordinates in [[call.icon.generator()]]##
#:361
    
#362:
##construct and activate plotting commands in [[call.icon.generator()]]:##
## if(verbose){ cat("activate plotting commands"); print(res.color) }
switch(type, 
       "polygon"  = polygon (res[,1], res[,2], col = res.color, xpd = NA, border = NA),
       "segments" = segments(res[,1], res[,2], res[,3], res[,4], col = res.color, 
                              lwd = mm.to.lwd(pic.i[, 5]) * icon.cex / 100,
                              xpd = NA),
       "text"     = text(res[,1], res[,2], as.character(pic.i[,3]),
                         col = res.color, cex = mm.to.cex(pic.i[,4]) * icon.cex / 100, 
                         # adj = c(0,0), # adjust argument for text not implemented yet
                         xpd = NA),
       "spline"   = {
                       n.h <- length(res[,1]); z <- seq(n.h); n <- 10
                       xy.h <- cbind(spline( z, res[,1], n = n * n.h)$y, 
                                     spline( z, res[,2], n = n * n.h)$y)
                       lines(xy.h, col = res.color, 
                             lwd = mm.to.lwd(pic.i[,3]) * icon.cex / 100, 
                             xpd = NA)
        }
)
##:construct and activate plotting commands in [[call.icon.generator()]]##
#:362
  }
  return()
}
##:define [[call.icon.generator()]]##
#:355
##:define local functions##
#:363
#12:
##find positioning vars:##
if(verbose) cat("find positioning vars")
if(is.null(grp.xy)) grp.xy <- "0~0"
grp.xy <- find.xy.groupings(grp.xy)
##:find positioning vars##
#:12
#15:
##find pic vars:##
if(verbose) cat("grp.icon of call:", grp.icon)
grp.design <- NULL
if( !is.null(grp.icon) ){ # 170817
  grp.icon <- gsub(" ", "", grp.icon)             # remove blanks
  grp.icon <- unlist(strsplit(grp.icon, "[+]"))   # split string  
  grp.icon <- grp.icon[ grp.icon != "" ]          # remove empty entries
  if( 1 < length(grp.icon) ){                     # further specifications found
    grp.design <- grp.icon[-1]                   # extract specifications 
    grp.icon <- grp.icon[1]                       # extract pic choise variable 
  }
} 
if(verbose){ cat("grp.icon interpreted"); print(grp.icon); print(grp.design)}
##:find pic vars##
#:15
#19:
##build data matrix:##
if(verbose) cat("build data matrix")
if(length(data) == 0 || all(unlist(data) == 0)) return("nothing to do")
if(is.vector(data) && !is.table(data)){      # case 1
  data <- as.table(data)
  if( 1 == length(data) && (0 == length(grp.xy$x) || any(0 < grp.xy$x)) ) 
    grp.xy$x <- "0"
  if( 0 == length(grp.xy$y) || any(1 < grp.xy$y) ) grp.xy$y <- "0"  
}
if(is.table(data)){                          # case 2
  
#20:
##check and possibly set dim names of table and save them:##
data.mat.cn.orig <- names(dimnames(data)) # set dim names if missing:
if( 0 == length(data.mat.cn.orig) || any("" == data.mat.cn.orig) ){ 
  h <- length(dim(data))
  data.mat.cn.orig <- c(data.mat.cn.orig, rep("",h))[1:h]
  h <- paste("Var", 1:h, sep = "")
  data.mat.cn.orig <- ifelse(data.mat.cn.orig != "", data.mat.cn.orig, h)
  names(dimnames(data)) <- data.mat.cn.orig
  cat("Note: names for dims have been set:", data.mat.cn.orig, "!\n")
}
names(dimnames(data)) <- data.mat.cn <- make.names(data.mat.cn.orig, TRUE)
##:check and possibly set dim names of table and save them##
#:20
  
#21:
##replace numbers in grouping definitions:##
local.debug <- !TRUE
if(local.debug){  if(is.matrix(data)) print(data[1:2,]); 
  print(data.mat.cn); cat("length(grp.color)", length(grp.color))
  cat("grp.color", grp.color, "grp.icon", grp.icon); print(grp.xy) }
find.col.name <- function( cname, dims ){ # cat("csname"); print(cname)
  if(1 == length(cname) && 0 < length(grep("^[1-9][0-9]*$", cname)) &&   
    is.na(match(cname, dims)) ){ cname <- as.character(dims[ as.numeric(cname) ]) }
  return(cname) # as.character() # 180607
}  
if( 0 < length(grp.icon) ){ # must be of length 1
  grp.icon <- find.col.name(grp.icon[1], data.mat.cn)
}
if( 0 < length(grp.design) ){
  grp.design <- sapply( grp.design, find.col.name, data.mat.cn)
}
grp.color <- find.col.name(grp.color, data.mat.cn)
if( "0" != grp.xy$x[1] ) grp.xy$x <- sapply( grp.xy$x, find.col.name, data.mat.cn)
if( "0" != grp.xy$y[1] ) grp.xy$y <- sapply( grp.xy$y, find.col.name, data.mat.cn)
if(verbose){ 
  cat("grp.color",grp.color, "grp.icon", grp.icon); print(grp.design); print(grp.xy)}
##:replace numbers in grouping definitions##
#:21
  
#22:
##reduce data table if not all dimensions are used:##
vars.used <- unique(c(unlist(grp.xy), grp.color, grp.icon, grp.design))
if(verbose){ 
  cat("vars.used", vars.used, "dimnames(data)"); print(dimnames(data)) 
}
found <- match(vars.used, data.mat.cn)
h <- vars.used
h <- h[ is.na(found) & h != "|"]
h <- h[ h != "0" & h != ".sign"]
h <- h[ !is.na(h) ]
if( 0 < length(h) ) cat("Warning: grouping vars ", h, "not found -> ignored!")
found <- found[ !is.na(found) ]
if( 1 < length(found) ) found <- sort(found)
relevant.columns <- rep(FALSE, length(data.mat.cn))
relevant.columns[found] <- TRUE
if( length(found) < length(dimnames(data)) ){ 
  data <- margin.table(data, found); if(!is.table(data)) data <- as.table(data)
  data.mat.cn      <- data.mat.cn[found]
  data.mat.cn.orig <- data.mat.cn.orig[found]
  if( 0 == length(found)) data.mat.cn <- data.mat.cn.orig <- "Var1"
}
if(local.debug){ 
  print(grp.xy); print(grp.color); cat("vars.used"); print(vars.used)
  cat("found"); print(found); # str(data); print(data.mat.cn); print(data.mat.cn.orig)
}
##:reduce data table if not all dimensions are used##
#:22
  
#23:
##check and possibly create new dimension due to negative cell entries:##
if( any( data < 0 ) ){
  data1 <- - data * (data <= 0); data2 <- data * (0 <= data); h <- c("-1","+1")
  # data1 <- data * (0 <= data); data2 <- -data* (data <= 0); h <- c("+1","-1")
  data3 <- array(c(data1, data2), c(dim(data),2))
  dimnames(data3) <- c(dimnames(data), list(".sign" = h) )
  cat("Note: new dimension '.sign' constructed to separate \n", 
      "the absolute values of negative and positive entries!\n")
  data <- data3
}
##:check and possibly create new dimension due to negative cell entries##
#:23
  
#24:
##expand table to data matrix and add sign and fraction column:##
if( all(0<=data) & all(data<=1) ) data <- data*0.99999 # force rel.freq. to be < 1
if( any( 0 < (dat  <- floor(data)) ) ){ # some entries > 0 after rounding down
  data.mat <- cbind(expand.table.short(dat), .fraction = 1)
} else data.mat <- NULL                 # maybe relative frequencies only
if( any(data != dat) ){                 # fractional entries observed
  fs <- .5 + 0 * data; h <- as.vector(data - dat) 
  # h <- as.vector(aperm(f, rev(seq(along = dim(f))))) # alternative sorting
  # h <- h[h>0]                         # values greater 0 should be handled only
  d <- cbind(expand.table.short(ceiling(fs)), .fraction = h) 
  idx <- h > 0 | as.vector(data) == 0   # fractional part positive or cell entry 0
  if( h <- (!is.null(data.mat)) )  cn <- colnames(data.mat)
  data.mat <- rbind(data.mat, d[idx,, drop = FALSE])
  if(h) colnames(data.mat) <- cn
}
if( !(".sign" %in% colnames(data.mat)) ){
  data.mat <- cbind(data.mat[, -(h <- dim(data.mat)[2]), drop = FALSE], .sign = 1, 
                    .fraction = data.mat[, h])
}
data.mat <- data.mat[ data.mat[, ".fraction"] != 0, ]                ## 161215
data.mat.cn      <- c(data.mat.cn,      ".sign", ".fraction") 
data.mat.cn.orig <- c(data.mat.cn.orig, ".sign", ".fraction")
# in case of 1-dim tables there is no dimname so we set name "Var1"
# if( "" == colnames(data.mat)[1] ) colnames(data.mat)[1] <- "Var1"
data.mat <- data.frame(data.mat)
##:expand table to data matrix and add sign and fraction column##
#:24
 
} else {                                     # case 3
  
#25:
##check and possibly set col names of data matrix and save them:##
data.mat <- cbind(data) 
data.mat.cn.orig <- colnames(data.mat) # set col names if missing:
if( 0 == length(data.mat.cn.orig) || any("" == data.mat.cn.orig) ){       
  h <- dim(data)[2]
  data.mat.cn.orig <- c(data.mat.cn.orig, rep("",h))[1:h]
  h <- paste("Var", 1:h, sep = "")
  data.mat.cn.orig <- ifelse(data.mat.cn.orig != "", data.mat.cn.orig, h)
  colnames(data) <- data.mat.cn.orig
  cat("Warning: col names of data matrix have been set:",
      data.mat.cn.orig, "!\n")
}
data.mat.cn <- make.names(data.mat.cn.orig, TRUE)
##:check and possibly set col names of data matrix and save them##
#:25
  
#21:
##replace numbers in grouping definitions:##
local.debug <- !TRUE
if(local.debug){  if(is.matrix(data)) print(data[1:2,]); 
  print(data.mat.cn); cat("length(grp.color)", length(grp.color))
  cat("grp.color", grp.color, "grp.icon", grp.icon); print(grp.xy) }
find.col.name <- function( cname, dims ){ # cat("csname"); print(cname)
  if(1 == length(cname) && 0 < length(grep("^[1-9][0-9]*$", cname)) &&   
    is.na(match(cname, dims)) ){ cname <- as.character(dims[ as.numeric(cname) ]) }
  return(cname) # as.character() # 180607
}  
if( 0 < length(grp.icon) ){ # must be of length 1
  grp.icon <- find.col.name(grp.icon[1], data.mat.cn)
}
if( 0 < length(grp.design) ){
  grp.design <- sapply( grp.design, find.col.name, data.mat.cn)
}
grp.color <- find.col.name(grp.color, data.mat.cn)
if( "0" != grp.xy$x[1] ) grp.xy$x <- sapply( grp.xy$x, find.col.name, data.mat.cn)
if( "0" != grp.xy$y[1] ) grp.xy$y <- sapply( grp.xy$y, find.col.name, data.mat.cn)
if(verbose){ 
  cat("grp.color",grp.color, "grp.icon", grp.icon); print(grp.design); print(grp.xy)}
##:replace numbers in grouping definitions##
#:21
  
#26:
##reduce data frame if not all variables are used:##
vars.used <- unique(c(unlist(grp.xy), grp.color, grp.icon, grp.design,
                    ".fraction", ".icon")) #170818
found <- match(vars.used, data.mat.cn)
h <- vars.used
h <- h[ is.na(found) & h != "|"]
h <- h[ h != "0" & h != ".sign"]
h <- h[ !is.na(h) ]
hh <- h[ ! (h %in% c(".fraction", ".icon")) ] #170818
if( 0 < length(hh) ) cat("Warning: grouping vars ", hh, "not found -> ignored!")
found <- found[ !is.na(found) ]
if( 1 < length(found) ) found <- sort(found)
relevant.columns <- rep(FALSE, length(data.mat.cn))
relevant.columns[found] <- TRUE
if( length(found) < length(colnames(data)) ){ 
  data.mat <- data.mat[, found, drop = FALSE]
  data.mat.cn      <- data.mat.cn[found]
  data.mat.cn.orig <- data.mat.cn.orig[found]
} 
##:reduce data frame if not all variables are used##
#:26
  
#27:
##append fraction and sign column:##
if( !".sign" %in% colnames(data.mat) ){
  data.mat <- cbind(data.mat, .sign = 1)
  data.mat.cn      <- c(data.mat.cn,      ".sign")
  data.mat.cn.orig <- c(data.mat.cn.orig, ".sign")
}
if( !".fraction" %in% colnames(data.mat) ){
  data.mat <- cbind(data.mat, .fraction = 1)
  data.mat.cn      <- c(data.mat.cn,      ".fraction")
  data.mat.cn.orig <- c(data.mat.cn.orig, ".fraction")
}
colnames(data.mat) <- data.mat.cn
data.mat <- data.frame(data.mat) ## 160829
##:append fraction and sign column##
#:27
}
#28:
##check and possibly modify grouping definitions:##
rm.0 <- function(x) x[ x != "0" ]
grp.design <- rm.0(grp.design)
grp.icon <-   rm.0(grp.icon)
grp.color <-  rm.0(grp.color)
grp.x <-      rm.0(grp.xy$x)
grp.y <-      rm.0(grp.xy$y) # testet
h <- c(grp.x, grp.y, grp.icon, grp.color, grp.design)
grp.vars <- if( is.null(h) ) h else unique(sort(h)) # grp.vars show grouping vars
if(verbose) cat("after check\ngrp.icon:", grp.icon, "grp.design:", grp.design) 
if(verbose) cat("grp.x", grp.x, "grp.y", grp.y, "grp.color", grp.color)
# find original grouping variable names:
f <- function(obj) substring(obj, 1, pmin(nchar(obj), lab.n.max[1]))
grp.x.o <-     f(data.mat.cn.orig[ match(grp.x,     data.mat.cn) ])
grp.y.o <-     f(data.mat.cn.orig[ match(grp.y,     data.mat.cn) ])
grp.color.o <- f(data.mat.cn.orig[ match(grp.color, data.mat.cn) ])
grp.icon.o <-   f(data.mat.cn.orig[ match(grp.icon,   data.mat.cn) ])
grp.design.o <-f(data.mat.cn.orig[ match(grp.design,data.mat.cn) ])
##:check and possibly modify grouping definitions##
#:28
##:build data matrix##
#:19
#30:
##transform variables:##
if(verbose) cat("transform variables")
# remove rows with NA entries
if( any(is.na(data.mat)) ){ 
  idx <- apply(data.mat, 1, function(x) all( !is.na(x) ) )
  data.mat <- data.mat[ idx,, drop = FALSE ]
  cat("NA rows have been removed")
}    
if(missing(vars.to.factors)) 
  vars.to.factors <- rep(TRUE, length(relevant.columns))
if( any(0 < vars.to.factors) ){                 # prepair vars.to.factors
  if(is.null(h <- names(vars.to.factors))){
    # case: no names of variables given
    if( 0 < ( h <- length(relevant.columns) - length(vars.to.factors) ) ) 
      vars.to.factors <- c(vars.to.factors, rep(1, h)) # not (0,h) #170102
    vars.to.factors <- vars.to.factors[relevant.columns]
    v.idx <- seq(along=vars.to.factors)
  } else {
    v.idx <- match(names(vars.to.factors), colnames(data.mat))
    v.idx <- v.idx[!is.na(v.idx)]
  }
  for(i in seq(along = v.idx)){                 # perform transformation:
    if(!is.factor(v <- data.mat[, v.idx[i] ]) 
       && data.mat.cn[v.idx[i]] != ".fraction"){
      if(0 == vars.to.factors[i]) next
      if(1 > vars.to.factors[i]){
        b <- seq(0,1, 1/round(1/vars.to.factors[i]))[-1]
        if( .95 < b[h <- length(b)] ) b <- b[-h]
        mi <- min(v); ma <- max(v)
        b <- unique(c(mi - 0.01*(ma - mi), quantile(v, b, na.rm = TRUE), ma))        
        v <- cut(v, breaks = b)
      }
      if(1 == vars.to.factors[i]) v <- factor(v)
      if(1 < vars.to.factors[i])  v <- cut(v, vars.to.factors[i]) 
      data.mat[, v.idx[i] ] <- v
    }
  }
}
##:transform variables##
#:30
#31:
##transform variables:##
if( 0 < length(grp.y) ){ 
  for(j in seq(along = grp.y)){ # reverse y 
    d <- data.mat[[grp.y[j]]]
    if( panel.reverse.y && is.factor(d) && grp.y[j] != ".sign" ) {
      h <- rev(levels(d))
      d <- factor(1 + length(h) - as.numeric(d)); levels(d) <- h
      data.mat[[grp.y[j]]] <- d
    }
  } # data.mat <- data.frame(data.mat[, grp.vars, drop=FALSE])
}
##:transform variables##
#:31
#32:
##transform variables:##
if(verbose) cat("grp.x:",        grp.x,    " / grp.y:",  grp.y,
                " / grp.color:", grp.color," / grp.icon:",grp.icon,
                " / colnames(data.mat):",  colnames(data.mat), sep="|")
##:transform variables##
#:32
#34:
##transform variables:##
# find objects to transform variables
#   args <- list(...) in <check arguments of [[iconplot]]>
if( 0 < length( h <- grep("^transform[.]", names(args))) ){
  if(verbose) cat("transform variable", h)
  args.list <- args[h]
  var.names <- sub("^transform[.]", "", names(args.list))
  # extract var.names which are found in the data
  col.no <- match(var.names, colnames(data.mat))
  idx <- !is.na(col.no)
  args.list <- args.list[idx]
  var.names <- var.names[idx]
  col.no <- col.no[idx]
  # loop along the columns to be transformed
  for(i in seq(along = col.no)){
    f <- args.list[[i]]
    if( is.character(f) ){
      txt <- c( "f <- function(x){", f, "}")
      res <- try(eval(parse(text = txt)))
      if( class(res) == "try-error" ){
        cat("Error during transformation of", var.names[i], "by\n  ", txt)
        next
      }
    }
    res <- try( f( data.mat[, col.no[i]] ) )
    if( class(res) == "try-error" ){
      cat("Error during transformation of", var.names[i], "by\n  " ); print(f) 
      if( is.factor(var.names[i]) ) 
        cat("-- Remark:", var.names[i], "is a factor variable!")
      next
    } else {
      data.mat[, col.no[i]] <- res
      if( verbose ){ cat( var.names[i], "has been transformed by\n  " ); cat(" ", deparse(f)) }
    }
  }
}
##:transform variables##
#:34
#37:
##fix colors:##
if( 0 < length(grp.color) ){
  values <- data.mat[, grp.color]
  lev.color <- if(is.factor(values)) levels(values) else unique(sort(values)) 
  n.values <- length(lev.color) 
  if(missing(colors)) colors <- substring(rainbow(n.values),1,7)
  if( (h <- length(colors)) < n.values ) colors <- c(colors, rep(colors, n.values))
  colors <- colors[1:n.values]
  Colors <- match(values, lev.color) # colors[ .. ]
  if(is.character(lev.color)) 
    lev.color <- substring(lev.color, 1, pmin(lab.n.max[1], nchar(lev.color)))
} else { 
  Colors <- rep(1, nrow(data.mat)); lev.color <- 1
  if(missing(colors)) colors <- 1
}
data.mat <- cbind(data.mat, .color = Colors)
##:fix colors##
#:37
#39:
##fix icons:##
if(verbose) print("fix icons -- begin -------------------")
if(!missing(icons)){ #170718
  # find set of icons to be used
  if(is.raster(icons)) icons <- list(icons)
  if( is.character(icons) ){
    h <- substring(h <- icons[[1]], nchar(h) - 3, nchar(h)) %in% 
       c(".jpg", ".JPG", "jpeg", "JPEG", ".pnm", ".PNM", ".png", ".PNG", ".ppm", ".PPM")
    if( h  ){
      
#42:
##read raster image if file names are given:##
if(is.character(icons)){ # names of files with images
  
#41:
##find file type and download file via internet:##
  file.type <- rep("no file", length(icons))
  download.list <- NULL
  for(i in seq(along = icons)){
    icon <- icons[i]; is.http <- substring(icon, 1, 4) == "http"
    icon.ext <- substring(icon, (h <- nchar(icon)) - 3, h)
    if( icon.ext %in% c(".jpg", ".JPG", "jpeg", "JPEG") ) file.type[i] <- "jpg"
    if( icon.ext %in% c(".pnm", ".PNM", ".ppm", ".PPM") ) file.type[i] <- "pnm"
    if( icon.ext %in% c(".png", ".PNG") )                 file.type[i] <- "png"
    if( file.type[i] != "no file" ){
      if( is.http ){ # download file as tmp file via internet
        fname <- sub(".*[.]", ".", icon)
        fname <- tempfile("icon-pic", fileext = fname)
        ok <- try( utils::download.file(url = icon, destfile = fname) )
        if( "error" %in% class(ok) ){ 
          cat("Error in iconplot: Download of file", icon, "failed.\n")
          return()
        } 
        icon <- fname; download.list <- c(download.list, fname)
        cat("internet file\n ", icon, "\ntemporarily stored in tmp file:\n ", 
            fname, "\n")
      } 
      if( !file.exists(icon) ){
        cat("Error in iconplot: reading of file", icon, "failed.\n")
        return()
      }
      icons[i] <- icon
    }
  } # now icons contains correct file names and file.type the file types
  if(verbose){ cat("files downloaded:\n"); print(download.list) }
##:find file type and download file via internet##
#:41
  if( any(file.type == "pnm") ){ 
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
 }
  n.icons <- dim(data.mat)[1]^0.5; p <- icons; icons <- NULL
  ok <- try(for(ip in seq(along = p)){ # read file  
              if(file.type[ip] == "jpg"){
                if(!"package:jpeg" %in% search()){
                  print("ERROR: iconplot() requires package jpeg; load it by: library(jpeg) and try again.")
                }
                pic <- jpeg::readJPEG(p[ip], native = !TRUE) 
              } 
              if(file.type[ip] == "png"){
                if(!"package:png" %in% search()){
                  print("ERROR: iconplot() requires package png; load it by: library(png) and try again.")
                }
                pic <- png::readPNG(p[ip], native = !TRUE)  
              }
              if(file.type[ip] == "pnm"){
                if(!"package:tcltk" %in% search()){
                  print("ERROR: iconplot() requires package tcltk; load it by: library(tcltk) and try again.")
                }
                pic <- get.pnm(p[ip]) 
              }
              if(verbose){ cat("file has been read:", p[ip], "\n") }
              # get grey.levels
              if(is.list(icon.grey.levels)){
                grey.levels <- icon.grey.levels[[ip]]
                if(is.null(grey.levels)) grey.levels <- icon.grey.levels[[1]]
              } else grey.levels <- icon.grey.levels 
              # cat("read: grey.levels", grey.levels)
              # transform color to grey
              if( !is.null(grp.color) || !identical(1, colors) ){ 
                # do raster.to.greys if grp.col is not NULL or colors != 1
                pic <- transform.color.to.rgb.integer(pic)
                #if( identical(grey.levels, 2) ) grey.levels <- c(0.05, 0.7)
                h <- raster.to.greys(pic, grey.levels = grey.levels,
                         # grey.levels = c(0.05, 0.7),
                         reduce = TRUE, n.icons = n.icons)  #180619
                pic <- h[[1]]; grey.levels <- h[[2]]
                if(is.list(icon.grey.levels)){
                  icon.grey.levels[[ip]] <- grey.levels
                } else icon.grey.levels <-grey.levels 
              } else pic <- as.raster(pic)
              # cat("read-end: grey.levels", grey.levels)
      icons <- c(icons, list(pic)); "ok"
  })
  if(0 < length(download.list)) file.remove(download.list)
  
#46:
##handle try reading error:##
  if(class(ok) == "try-error"){
    icons <- seq(along = p)
    cat("Warning: reading of file(s)", p[ip] ,"failed, plotting symbols will be used!\n")
  }
##:handle try reading error##
#:46
}
##:read raster image if file names are given##
#:42
    }
  } # other icons of mode char must store names of icons to be generated
  if(is.function(icons)) icons <- list(icons) else # otherwise build rect()-icons
    if( length(icons) == 0 || is.na(icons)[[1]] )  icons <- 0
  if(!is.list(icons)) icons <- split(icons, seq(along=icons)) 
}
if( 0 < length(grp.icon) ){ 
  # find levels for each of the vars of grp.icon
  values <- lapply(grp.icon, function(x) data.mat[, x])
  lev.pic <- lapply(values, function(x) 
               if(is.factor(x)) levels(x) else unique(sort(x))) 
  # find number of icons needed 
  n.values <- prod( lev.pic.len <- unlist(lapply(lev.pic, length)) )
  # find set of icons to be used
  if(missing(icons)){
    icons <- seq(n.values)
    if(!is.list(icons)) icons <- split(icons, seq(along=icons)) 
  }
  # expand icons to get n.values elements
  h <- length(icons); 
  if( h < n.values){
    if( h == 1 ) icons <- rep(icons, n.values) else # one element is repeated
      icons <- c(icons, rep(list(0), n.values - h)) # 0s are appended
  } else  icons <- icons[1:n.values]                                     #180312
  pic.legend.ok <- 1 < length(unique(icons)) # & h<=n.values: condition removed #180312
  # construct vector of pic no -----------------------------------------------
  pic.vec <- 1; h <- c(1, cumprod( lev.pic.len )) # last element unnecessary
  for(i in seq(along = grp.icon)) # array index formula
    pic.vec <- pic.vec + (match(values[[i]], lev.pic[[i]])-1) * h[i]
  # construct combinated level labels for the pic legend
  if( 1 < length(grp.icon) ) {
    h <- lev.pic; lev.pic <- lev.pic[[length(h)]]
    for(i in (length(h)-1):1) 
      lev.pic <- sapply(lev.pic, function(x) paste( h[[i]], x, sep = "*") )
  } else lev.pic <- unlist(lev.pic) 
  lev.pic <- as.vector(lev.pic)
  if(is.character(lev.pic)) 
    lev.pic <- substring(lev.pic, 1, pmin(lab.n.max[1], nchar(lev.pic)))
  # construct the combinated var labels for the pic legend
  grp.icon.o <- paste(grp.icon.o, collapse = "*")
} else {
  if(missing(icons)) icons <- 0; pic.legend.ok <- FALSE
  pic.vec <- rep(1, nrow(data.mat)); lev.pic <- 1 
}
##:fix icons##
#:39
#40:
##fix icons:##
data.mat <- cbind(data.mat, .icon = pic.vec); if(verbose) show.obj(grp.icon) 
if(verbose){ 
  cat("lev.pic"); print(lev.pic);  cat("pic.vec", pic.vec); cat("grp.icon.o", grp.icon.o)
  print("fix icons -- end --------------------")
}
##:fix icons##
#:40
 
#49:
##construct jobs by panel packer:##
if(verbose) cat("construct jobs by panel packer")
xmin <- 0; xmax <- 100; ymin <- 0; ymax <- 100   # initialization of limits
##:construct jobs by panel packer##
#:49
#50:
##construct jobs by panel packer:##
# "jobs" will store the jobs of printing panels of symbols
# no position grouping => one job
XINTS <- YINTS <- NULL; n.z <- n.s <- 1
XINTS <- if(0 == length(grp.x)) 
           list(cbind(0, xmin, xmax, mean(xmin + xmax)))
YINTS <- if(0 == length(grp.y)) 
           list(cbind(0, ymin, ymax, mean(ymin + ymax)))
if( 0 < length(c(grp.x, grp.y)) ){
  # idx <- match(unique(c(grp.x, grp.y)), data.mat.cn) # doesn't work
  idx <- match((c(grp.x, grp.y)), data.mat.cn) 
  h <- data.mat[, idx, drop=FALSE]; cn <- colnames(h)
  dm.df <- data.frame(h); names(dm.df) <- cn
  # find number of items of combinations of positioning variables
  dm.table      <- table     (dm.df) # contingency table of positioning vars
  dm.fraction <- data.mat[, ".fraction"]                                    ## 161212
  
#51:
##define [[table.freq()]]:##
table.freq <- function(DM.DF, freq = ".fraction"){ # 161214
  # DM.DF :: data frame of a data set
  # freq  :: vector showing the frequencies of the observations of DM.DF
  #                 or name of column of DM.DF of frequencies
  # uses: expand.table.short
  # pw 161213
  freq.name <- if( length(freq) == 1 ) freq else "no freq col"
  # extract frequencies from DM.DF
  if( freq.name %in% colnames(DM.DF) ){ 
    freq <- DM.DF[, freq.name]
    DM.DF <- DM.DF[, colnames(DM.DF) != freq.name, drop = FALSE ]
  } 
  if( length(freq) == 1 ) freq <- rep(1, length(DM.DF[,1]))
  # construct empty table DM.TABLE from data frame
  if( all(0 == ( freq %% 1)) ) return(table(DM.DF))  
  DM.TABLE <- do.call(table, DM.DF) * 0
  # find all combinations of levels
  level.comb <- cbind(expand.table.short(1 + DM.TABLE))
  # initialize new fraction vector
  counts2 <- rep(0, length(level.comb[,1]))
  # clear but to slow: 
  # for all rows of DM.DF do 
  #   find combination of levels that matches with row of data frame by:
  #   for all combinations of levels compare them with row of data frame
  #     if found add fraction of line i to frac.vec[ii]
  # counts <- rep(0, length(level.comb[,1]))
  # for(i in seq( along = DM.DF[,1])){
  #   for( ii in seq(along = level.comb[,1]) ){    
  #     if( any( DM.DF[i,] != level.comb[ii,] ) ) next
  #     counts[ii] <- counts[ii] + freq[i]; break
  #   }
  # }  # .. cat("DIFF", sum(counts- counts2))
  h.dmdf <- sapply(split(unlist(DM.DF),      row(DM.DF)),      paste, collapse = "|")
  h.lvlc <- sapply(split(unlist(level.comb), row(level.comb)), paste, collapse = "|")
  where <- match(h.dmdf, h.lvlc)
  for(i in 1:length(DM.DF[,1])) counts2[ where[i] ] <- counts2[ where[i] ] + freq[i]
  DM.TABLE <- DM.TABLE + counts2
  return( DM.TABLE ) 
}
##:define [[table.freq()]]##
#:51
  DM.TABLE.FREQ <- table.freq(dm.df, dm.fraction)                             ## 161213
## print(data.mat[ data.mat[, ".fraction"] < 1,])                             ## 161212
## DM.TABLE <<- dm.table; DM.DF <<- dm.df; FRAC <<- data.mat[ , ".fraction" ] ## 161213
## print(dm.table); print(dm.df)
  # set xmin, xmax, ymin, ymax in case of grouping 
  
#55:
##find x and y ranges in case of grouping by x or y:##
# according data type we choose ranges for x 
#53:
##set variables [[panel.adjust]] and [[panel.space.factor]]:##
panel.adjust       <- c(panel.adjust,       panel.adjust)   [1:2] 
panel.space.factor <- c(panel.space.factor, panel.space.factor)[1:2]
##:set variables [[panel.adjust]] and [[panel.space.factor]]##
#:53
if( 0 < length(grp.x) ){
  idx <- match(grp.x[1], names(dm.df))
  if(is.factor(h <- dm.df[[ idx ]])) { 
    xmin <- 1; xmax <- 1 + length(levels(h)) 
    xmin <- xmin - panel.adjust[1]
    xmax <- xmax - panel.adjust[1] 
  } else {
    hh <- panel.space.factor[1] * 0.5 * (max(h) - min(h))
    xmin <- min(h) - hh * 2 *      panel.adjust[1]
    xmax <- max(h) + hh * 2 * (1 - panel.adjust[1])
  }
}
##:find x and y ranges in case of grouping by x or y##
#:55
#56:
##find x and y ranges in case of grouping by x or y:##
# according data type we choose ranges for y
if( 0 < length(grp.y) ){
  idy <- match(grp.y[1], names(dm.df))
  if(is.factor(h <- dm.df[[ idy ]])) { 
    ymin <- 1; ymax <- 1 + length(levels(h))
    ymin <- ymin - panel.adjust[2]
    ymax <- ymax - panel.adjust[2] 
  } else {
    hh <- panel.space.factor[2] * 0.5 * (max(h) - min(h))
    ymin <- min(h) - hh * 2 *      panel.adjust[2]
    ymax <- max(h) + hh * 2 * (1 - panel.adjust[2])
  }
}
##:find x and y ranges in case of grouping by x or y##
#:56
 
  
#57:
##build job matrix jobs:##
if(verbose) cat("build job matrix") # initialize job matrix
jobs <- cbind(xmins = xmin, xmaxs = xmax, ymins = ymin, ymaxs = ymax) 
##:build job matrix jobs##
#:57
#58:
##build job matrix jobs:##
# find intervals for each of the x-grouping variables
panel.prop.to.size <- rep(panel.prop.to.size, 2)
if(0 < length(grp.x)){
  for(iv in seq(along=grp.x)){ # find new x partitions
    jobs.new <- NULL           # initialize new job matrix
    idx <- match(grp.x[iv], names(dm.df))
    if(0 < panel.prop.to.size[1]){
      ## freq.x <- margin.table(dm.table,1:iv)
      freq.x <- margin.table(DM.TABLE.FREQ,1:iv)                     # 161213
      d.freq.x <- dim(freq.x); l.d.freq.x <- length(d.freq.x)
       freq.x <- aperm(freq.x, l.d.freq.x:1)
       freq.x <- array(freq.x, c(d.freq.x[l.d.freq.x],prod(d.freq.x[-l.d.freq.x])))
    } else freq.x <- rbind(rep(dim(dm.table)[idx], nrow(jobs)))
    for( no in 1:nrow(jobs) ){ 
      # find intervals basing on the limits of big x-interval
      xints <- find.n.intervals( freq.x[,no], props = panel.prop.to.size[1], 
                                 jobs[no, 1], jobs[no, 2], 
                                 panel.space.factor[1] ) 
      XINTS <- c(XINTS, 
                 list(cbind(iv, rbind(xints[,c("mins", "maxs", "mids")]))))
      # add y coordinates to the new intervals found by "find.n.intervals"
      jobs.new <- rbind(jobs.new, 
                        cbind(xints[, c("mins", "maxs"), drop = FALSE],
                              jobs[no, 3], jobs[no, 4]))
    }
    jobs <- jobs.new     # store new job matrix in jobs
  } 
}
n.s <- dim(jobs)[1]
##:build job matrix jobs##
#:58
#59:
##build job matrix jobs:##
# find intervals for each of the y-grouping variables
if(0 < length(grp.y)){
  for(iv in seq(along=grp.y)){ # refine y range(s)
    jobs.new <- NULL     # initialize new job matrix
    idy <- match(grp.y[iv], names(dm.df))
    # print("HALLO"); print(dm.table) # 161212
    if(0 < panel.prop.to.size[2]){
      # freq.y <- margin.table(dm.table,(length(grp.x)+1):(length(grp.x)+iv))
      freq.y <- margin.table(DM.TABLE.FREQ,(length(grp.x)+1):(length(grp.x)+iv))
      d.freq.y <- dim(freq.y); l.d.freq.y <- length(d.freq.y)
      freq.y <- aperm(freq.y, l.d.freq.y:1)
      # print(freq.y) # 161212
      freq.y <- array(freq.y, c(d.freq.y[l.d.freq.y],prod(d.freq.y[-l.d.freq.y])))
    } else freq.y <- rbind(rep(dim(dm.table)[idy], nrow(jobs)))
    for( no in 1:nrow(jobs) ){ 
      yints <- find.n.intervals(freq.y[, 1 + ((no-1) %% dim(freq.y)[2])],
                                props = panel.prop.to.size[2], 
                                jobs[no, 3], jobs[no, 4],
                                panel.space.factor[2] )
      YINTS <- c(YINTS, 
                 list(cbind(iv, rbind(yints[,c("mins", "maxs", "mids")]))))
      jobs.new <- rbind(jobs.new, 
                        cbind(jobs[no,1], jobs[no,2], 
                              yints[, c("mins", "maxs"), drop = FALSE]))
    }
    jobs <- jobs.new     # store new job matrix in jobs
  } 
} #; cat("jobs"); print(head(jobs))
n.z <- dim(jobs)[1]/n.s
##:build job matrix jobs##
#:59
  colnames(jobs) <- c("xmins", "xmaxs", "ymins", "ymaxs")
} else {                                           # one job only:
  dm.df <- data.frame(rep(1, nrow(data.mat)))
  dm.table <- table(dm.df)  
  jobs <- cbind(xmins = xmin, xmaxs = xmax, ymins = ymin, ymaxs = ymax)
} # cat("jobs:"); print(jobs)
##:construct jobs by panel packer##
#:50
#61:
##link jobs to entries of the data matrix:##
if(verbose) cat("link jobs to data matrix by icons packer")
# find job number of each of the items and of each of the jobs of jobs
# step 1: find levels of xy grouping variables
levs <- dimnames(dm.table) #; cat("levs"); print(levs)
##:link jobs to entries of the data matrix##
#:61
#62:
##link jobs to entries of the data matrix:##
# step 2: find job.no of jobs of job matrix by restructing the row indices 
n.levs <- unlist(lapply(levs, length))          #; cat("n.levs", n.levs)
job.no.table <- array(seq(nrow(jobs)), n.levs)  # == 1:prod(n.levs)
if( 0 < length(job.no.table) ){
  job.no.table <- aperm(job.no.table,       rev(seq(along=n.levs)))
  counts       <- as.vector(aperm(dm.table, rev(seq(along=n.levs))))
} else { job.no.table <- 1; counts <- 0 } 
jobs <- cbind(jobs, .job.no = as.vector(job.no.table), counts = counts)
#; print(jobs)
z <- 1 + floor( (0:( n.s * n.z - 1 )) / n.s); z <- n.z + 1 - z
s <- 1 + ( 0:( n.s * n.z - 1) ) %% n.s
z <- as.vector(t(matrix(z, n.s, n.z)))
s <- as.vector(t(matrix(s, n.s, n.z)))
jobs <- cbind(jobs, "row" = z, "col" = s)
##:link jobs to entries of the data matrix##
#:62
#63:
##link jobs to entries of the data matrix:##
# step 3: find values of variables in level vectors and store position
pos.mat <- dm.df
for( co in seq(length(dm.df)) ){
  values <- dm.df[, co] # values <-> levels comparison
  pos.mat[,co] <- match(values, levs[[co]])
} 
##:link jobs to entries of the data matrix##
#:63
#64:
##link jobs to entries of the data matrix:##
# step 4: find job number of an item by recoding position infos
code <- cumprod(c(1, dim(dm.table)[ -length(dim(dm.table)) ]))
job.of.item <- apply(pos.mat, 1, function(it) 1 + sum((it - 1) * code))
if( 0 == length(job.of.item) ) job.of.item <- 1
data.mat <- cbind(data.mat, .job.no = job.of.item) 
##:link jobs to entries of the data matrix##
#:64
#93:
##construct raw plot and set graphics parameter:##
# find over all extrems of panels -----------------------------------
panel.margin <- rep(panel.margin,4)[1:4]
h <- if(!exists("XINTS")){
       c(min(jobs[, 1]), max(jobs[, 2]))
     } else {
       range(XINTS[[1]][, -1])
     }
xmin <- h[1] - panel.margin[2] * diff(h)
xmax <- h[2] + panel.margin[4] * diff(h)
h <- if(!exists("YINTS")){
       c(min(jobs[, 3]), max(jobs[, 4]))
     } else {
       range(YINTS[[1]][, -1])
     }
ymin <- h[1] - panel.margin[1] * diff(h)
ymax <- h[2] + panel.margin[3] * diff(h)
##:construct raw plot and set graphics parameter##
#:93
#94:
##construct raw plot and set graphics parameter:##
x.lab.side <- y.lab.side <- 0 # ; x.lab.side <- 1; y.lab.side <- 2
lab.side <- unlist(strsplit(lab.side,""))
if("b" %in% lab.side) x.lab.side <- 1; if("l" %in% lab.side) y.lab.side <- 2
if("t" %in% lab.side) x.lab.side <- 3; if("r" %in% lab.side) y.lab.side <- 4
##:construct raw plot and set graphics parameter##
#:94
#95:
##construct raw plot and set graphics parameter:##
# set inner margin and initialize raw plot --------------------------
oldpar <- par(no.readonly = TRUE) # 161012
not.save <- c("fig", "fin", "mfcol", "mfg", "mfrow", "oma", "omd", "omi")
idx <- ! (names(oldpar) %in% not.save)
oldpar <- oldpar[idx] # 161202
par(mar=mar)
plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax),
     xlab="", ylab="", axes=FALSE)
par(usr = c(xmin, xmax, ymin, ymax))
# margin management needs some graphics parameters
pin <- par()$pin; cin <- par()$cin
##:construct raw plot and set graphics parameter##
#:95
#97:
##construct raw plot and set graphics parameter:##
# initialization of some variables
delta.rot <- lab.mar1 <- cols.icons.mar <- 0
legend.n.limit <- lab.n.max[3]    # limit legend up to 20
# number of color legend labels to great => number of labels to skip between two labels
legend.color.skip <- ceiling(length(colors) / legend.n.limit) - 1
# resulting number of color labels
legend.n.color <- ceiling(length(colors) / (1 + legend.color.skip))
legend.labels <- c(lev.color, lev.pic)
l.w <- max(strwidth (legend.labels, units = "inches")) / cin[2] # unit: lines
l.h <- max(strheight(legend.labels, units = "inches")) / cin[2] # unit: lines
# find inch height of a character
y.per.row.in <- cin[2] * lab.cex * 1.1          # slightly greater for savety  
##:construct raw plot and set graphics parameter##
#:97
#98:
##construct raw plot and set graphics parameter:##
# margin for var x labeling in lines: bottom or top -------------------------
lab.type <- if( substring(lab.type,1,1) == "c" ) "compact" else "expanded"
if( 0 < length(grp.x) ){
  if(lab.type != "compact"){ lab.mar1 <- length(grp.x) * 3 + 1 } else {
                             lab.mar1 <- length(grp.x) * 1 + 1 + 1 }
} # check whether legends are needed and length doesn't exceed legend.n.limit:
color.legend.ok <- 0 < length(grp.color) && 1 < length(colors)                  # CCL
pic.legend.ok   <- 0 < length(grp.icon)   && pic.legend.ok  && 
                   (!identical(icons[[1]],0)) && length(icons) <= legend.n.limit
# compute [[cols.icons.mar]]:  margin in text lines to be needed for legends
if(color.legend.ok || pic.legend.ok){ #               compute cols.icons.mar    
  if( lab.legend == "cols" ){ # legend vertical below plot: 1+number of items
    cols.icons.mar <- 1 + max( legend.n.color * color.legend.ok,                # CCL
                              length(icons)   * pic.legend.ok )
  }
  if( lab.legend %in% c("rows", "skewed", "horizontal") ){  # legend parallel
    
#99:
##compute [[delta.rot]] and [[cols.icons.mar]] for rotated legend texts:##
# additional space needed by rotation of label texts
if( lab.legend == "rows" ) # case 2: 2 lines: labels and var names per legend 
  cols.icons.mar <- 2 * (color.legend.ok + pic.legend.ok)
if( lab.legend == "skewed" ){ # case 3: 1 line for symbols per legend 
  delta.rot <- l.w * 0.7071 + l.h * 0.7071 # width*sin(pi/4)*height*cos(pi/4)
  delta.rot <- delta.rot - .2 - 1 # height -.2 space - 1 space of var name
  delta.rot <- max( 0, delta.rot) * 1.1 # to increase space a little bit
  cols.icons.mar <- (2 + delta.rot) * (color.legend.ok + pic.legend.ok) 
}
if( lab.legend == "horizontal" ){ # case 4: 1 line for symbols per legend 
  delta.rot <- l.w + 1 - 1 # string width + 1 space - 1 space of var name # 0.5 space
  delta.rot <- max( 0, delta.rot) 
  cols.icons.mar <- 2 + delta.rot
}
##:compute [[delta.rot]] and [[cols.icons.mar]] for rotated legend texts##
#:99
  
  }
  if( lab.legend == "vertical" ) "relax"                    # nothing to do
} # space for subtitle in cols.icons.mar in lines: 
space.sub <- if( x.lab.side == 3 ) space.sub <- 1.5 else 0
##:construct raw plot and set graphics parameter##
#:98
#100:
##construct raw plot and set graphics parameter:##
# y-space in inches = number of rows * height of row in inches
lab.mar1.in      <- lab.mar1      * y.per.row.in # margin labels (grp.x)
cols.icons.mar.in <- cols.icons.mar * y.per.row.in # margin legend
space.sub.in     <- space.sub     * y.per.row.in # margin subtitle
# mar in inches
mar1.in <- mar[1] * cin[2]    # additional margin bottom
mar3.in <- mar[3] * cin[2]    # additional margin top
# space for plotting: viewport in inches
# viewport.in   <- y-space for plot - margins - subtitle-space
viewport1.in    <- pin[2] - mar1.in - mar3.in - cols.icons.mar.in + 
                          - lab.mar1.in - space.sub.in
# factor to transform inches to user coordinates: (range of y)/(viewport.in)
fac.in.usr <- (ymax - ymin) / viewport1.in
# y-space in user coordinates
y.lab.mar1      <- lab.mar1.in      * fac.in.usr # margin labels (grp.x)
y.cols.icons.mar <- cols.icons.mar.in * fac.in.usr # margin legend
y.space.sub     <- space.sub.in     * fac.in.usr # margin subtitle
y.mar1          <- mar1.in          * fac.in.usr # additional margin
y.mar3          <- mar3.in          * fac.in.usr # additional margin
y.delta.rot     <- (delta.rot * y.per.row.in) * fac.in.usr
##:construct raw plot and set graphics parameter##
#:100
#101:
##construct raw plot and set graphics parameter:##
# (left or right) margin for var y labeling: left or right  
lab.mar2 <- 0 # space in lines used for labels left or right
if( 0 < length(grp.y) ){
  if(lab.type != "compact"){ lab.mar2 <- length(grp.y) * 3 + 1 } else {
                             lab.mar2 <- length(grp.y) + 1 }
}
x.per.row.in <- y.per.row.in # find inch height of a rotated character   
#        number of rows * height of row = margin in inches
lab.mar2.in <- lab.mar2 * x.per.row.in  
if( lab.legend == "vertical" ){ # have the legend occur on the right side?
  legend.right.in <- (l.w + 1) * cin[2] * lab.cex + 1 * x.per.row.in
  legend.right.in <- max(legend.right.in, strwidth(c(grp.icon.o, grp.color.o), 
                            units = "inches") * lab.cex - 0.5 * x.per.row.in)
} else legend.right.in <- 0
mar2.in <- mar[2] * cin[2]; mar4.in <- mar[4] * cin[2] 
viewport2.in <- (pin[1] - mar2.in - mar4.in - lab.mar2.in - legend.right.in)
fac.in.usr2 <- (xmax - xmin) / viewport2.in
# margin in usr (margin in inch) * fac.in.usr2
x.lab.mar2 <- lab.mar2.in * fac.in.usr2
x.legend.right <- legend.right.in * fac.in.usr2
x.mar2 <- mar2.in * fac.in.usr2; x.mar4 <- mar4.in * fac.in.usr2
##:construct raw plot and set graphics parameter##
#:101
#102:
##construct raw plot and set graphics parameter:##
# compute and set over all extremes 
gxmin <- xmin; gxmax <- xmax; gymin <- ymin; gymax <- ymax 
gymin <- gymin - y.mar1 - y.cols.icons.mar - y.space.sub
gymax <- gymax + y.mar3
gxmin <- gxmin - x.mar2             
gxmax <- gxmax + x.mar4 + x.legend.right
if(x.lab.side==1) gymin <- gymin - y.lab.mar1
if(x.lab.side==3) gymax <- gymax + y.lab.mar1
if(y.lab.side==2) gxmin <- gxmin - x.lab.mar2
if(y.lab.side==4) gxmax <- gxmax + x.lab.mar2
par(usr = c(gxmin, gxmax, gymin, gymax)) 
# get some graphics parameter that will be used later
mar <- par()$mar; mai <- par()$mai; usr <- par()$usr
pin <- par()$pin; cxy <- par()$cxy
# compute size of characters for text lines at the top or bottom side
y.per.row <- size.mm.to.usr(y.per.row.in * 25.4, FALSE) 
# compute size of characters left or right of vertical lines
x.per.row <- size.mm.to.usr(x.per.row.in * 25.4, TRUE ) 
##:construct raw plot and set graphics parameter##
#:102
if("icons" %in% packer){     
  
#82:
##pack icons by icons packer:##
if(verbose) cat("pack icons")
#114:
##find graphics parameters:##
# args <- list(...) # in (7) # 170612
if(any(h <- (!(names(args) %in% c("cex")))) ) 
  cat("Remark: '...'-arguments found:", paste(names(args)[h], collapse = " "), "\n")
cex.local <- if( "cex" %in% names(args) ) args$cex else 1 # 161012
##:find graphics parameters##
#:114
#68:
##find general pic parameter dependent on scaling case:##
#72:
##compute some general variables for finding the x.y.coordinates:##
panel.size <- cbind(x = jobs[, 2] - jobs[, 1], y = jobs[, 4] - jobs[, 3])
# if(!panel.prop.to.size) panel.size <- panel.size[1,,drop=FALSE]
panel.size.mm <- cbind(size.usr.to.mm(panel.size[,1], horizontal = TRUE),
                       size.usr.to.mm(panel.size[,2], horizontal = FALSE))
panel.size.mm[] <- ifelse(panel.size.mm[] < 1, 1, panel.size.mm[]) 
n.max <- max(dm.table)
if(any(0 < panel.prop.to.size)) n.max <- jobs[, "counts"] 
icon.space.factor <- c(icon.space.factor, icon.space.factor)[1:2]
# show.obj(panel.size); show.obj(panel.size.mm)
# show.obj(n.max); print(dm.table); # show.obj(icon.space.factor)
##:compute some general variables for finding the x.y.coordinates##
#:72
if( is.na(icon.aspect) ){ 
#79:
##try to compute asp by mean(icons.asp):##
if( !missing(icons )){ 
  if( !(is.numeric(icons) && is.vector(icons)) && 
      !is.function(icons[[1]]) ){ # should be a list
    if( !is.list(icons) ) icons <- list(icons)  # to be shure here
    if( 0 < length(icons) && is.matrix(icons[[1]])){
      d <- unlist(lapply(icons, function(x) dim(x)))
      d <- matrix(d, nrow = 2)
      icon.aspect <- mean(d[2,])/ mean(d[1,]) 
    }             # mean width / mean height
  }
}
##:try to compute asp by mean(icons.asp)##
#:79
 }
## case 1: ------------------------------ icon.cex defined: size of icons fix
if( !is.na(icon.cex)){                   if(verbose) cat("scaling case 1")
  if( is.na(icon.aspect) ) icon.aspect <- 1
  if( is.na(icon.stack.len) ){ 
    
#73:
##case 1: compute icon.stack.len by panel size and icon.cex:##
if(verbose) cat("scaling case 1")
div.factor <- (1 - icon.space.factor[2 - icon.horizontal]) * 
              (if(icon.horizontal) 1 else icon.aspect)
pic.extension.mm <- icon.cex / div.factor
panel.extension.mm <- panel.size.mm[, 2 - icon.horizontal] #  is vector
icon.stack.len <- pmax(1, floor(panel.extension.mm / pic.extension.mm))
# print(jobs)
# show.obj(panel.extension.mm); show.obj(pic.extension.mm)
# show.obj(icon.stack.len)
##:case 1: compute icon.stack.len by panel size and icon.cex##
#:73
  }
  n.stacks <- ceiling(n.max / icon.stack.len)
}
##:find general pic parameter dependent on scaling case##
#:68
#69:
##find general pic parameter dependent on scaling case:##
## case 2: ---- icon.cex undefined and icon.aspect defined:
##              size of icons scalable
if( is.na(icon.cex) && !is.na(icon.aspect) ){ 
  if(verbose) cat("scaling case 2")
  if( !is.na(icon.stack.len) ){ #   icon.stack.len defined
    n.stacks <- ceiling(n.max / icon.stack.len) 
    
#75:
##case 2: compute icon.cex by icon.stack.len and n.stacks:##
if(verbose) cat("case 2 -- not optim")
if(icon.horizontal){ # find icon.cex for x and y direction 
  cex.raw <- min(a1 <- panel.size.mm[, 1] / icon.stack.len, 
                 a2 <- panel.size.mm[, 2] / n.stacks     * icon.aspect
                       * (1 - icon.space.factor[2]) / (1 - icon.space.factor[1]))
} else {
  cex.raw <- min(panel.size.mm[, 2] / icon.stack.len      * icon.aspect
                       * (1 - icon.space.factor[2]) / (1 - icon.space.factor[1]),
                 panel.size.mm[, 1] / n.stacks)
}
icon.cex <- cex.raw * (1 - icon.space.factor[1])
##:case 2: compute icon.cex by icon.stack.len and n.stacks##
#:75
  } else { #                      icon.stack.len undefined
    
#74:
##case 2: compute icon.cex, icon.stack.len and n.stacks by optimization:##
if(verbose) cat("case 2 optim")
panel.size.mm[] <- ifelse(panel.size.mm[] < 1, 1, panel.size.mm[])
# ----- find initial icon.stack.len -----
if(icon.horizontal){                  # floor is more save than ceiling!
  icon.stack.len <- floor( sqrt( panel.size.mm[, 1] / panel.size.mm[, 2] 
                     * (1 - icon.space.factor[1]) / (1 - icon.space.factor[2]) 
                     / icon.aspect * n.max ) )
} else {
  icon.stack.len <- floor( sqrt( panel.size.mm[, 2] / panel.size.mm[, 1] 
                     * (1 - icon.space.factor[2]) / (1 - icon.space.factor[1]) 
                     * icon.aspect * n.max ) )
}
icon.stack.len <- pmax(1, icon.stack.len)
# ----- find suitable cex.raw -----
# floor() may be suboptimal, therefore check some icon.stack.len++ 
cex.raw <- 0
for(i in 1:5){ # 160930 old solution by exchange 5 by 2
  icon.stack.len <- icon.stack.len + (1 < i)
  n.stacks <- ceiling(n.max / icon.stack.len)
  if(icon.horizontal){ # find icon.cex for x and y direction and their minima
    cex.raw1 <- pmin(panel.size.mm[, 1] / icon.stack.len, 
                     panel.size.mm[, 2] / n.stacks     * icon.aspect 
                     * (1 - icon.space.factor[2]) / (1 - icon.space.factor[1])) 
  } else {
    cex.raw1 <- pmin(panel.size.mm[, 2] / icon.stack.len * icon.aspect
                     * (1 - icon.space.factor[2]) / (1 - icon.space.factor[1]),
                     panel.size.mm[, 1] / n.stacks)
  }
  cex.raw <- pmax(cex.raw, cex.raw1)
}
idx <- which.min(cex.raw)[1]  
icon.cex <- cex.raw[idx] * (1 - icon.space.factor[1]) * 0.999 # to be numerical safe
# ----- find final icon.stack.len -----
# now icon.cex is defined and icon.stack.len can be found as in case 1
#73:
##case 1: compute icon.stack.len by panel size and icon.cex:##
if(verbose) cat("scaling case 1")
div.factor <- (1 - icon.space.factor[2 - icon.horizontal]) * 
              (if(icon.horizontal) 1 else icon.aspect)
pic.extension.mm <- icon.cex / div.factor
panel.extension.mm <- panel.size.mm[, 2 - icon.horizontal] #  is vector
icon.stack.len <- pmax(1, floor(panel.extension.mm / pic.extension.mm))
# print(jobs)
# show.obj(panel.extension.mm); show.obj(pic.extension.mm)
# show.obj(icon.stack.len)
##:case 1: compute icon.stack.len by panel size and icon.cex##
#:73
n.stacks <- ceiling(n.max / icon.stack.len) 
if(verbose) cat("icon.cex", icon.cex, "idx", idx, icon.stack.len)
##:case 2: compute icon.cex, icon.stack.len and n.stacks by optimization##
#:74
  } 
}
##:find general pic parameter dependent on scaling case##
#:69
#70:
##find general pic parameter dependent on scaling case:##
## case 3: ------------------- icon.cex and icon.aspect undefined:
##                             x and y scalable
if( is.na(icon.cex) && is.na(icon.aspect) ){ 
  if(verbose) cat("scaling case 3")
  if( is.na(icon.stack.len) ) icon.stack.len <- pmax(1,n.max)
  n.stacks <- pmax(1,ceiling(n.max / icon.stack.len))
  
#76:
##case 3: compute icon.cex, asp:##
## cat("case 3")
if(icon.horizontal){ # find icon.cex for x and y direction 
  cex.raw   <- min(panel.size.mm[, 1]  / icon.stack.len)
  cex.y.raw <- min(panel.size.mm[, 2]  / n.stacks)
} else {
  cex.raw   <- min(panel.size.mm[, 1]  / n.stacks)
  cex.y.raw <- min(panel.size.mm[, 2]  / icon.stack.len)
}
icon.cex <- cex.raw * (1 - icon.space.factor[1])
icon.aspect  <- icon.cex / (cex.y.raw * (1 - icon.space.factor[2]))
##:case 3: compute icon.cex, asp##
#:76
}
##:find general pic parameter dependent on scaling case##
#:70
#83:
##compute x.y.coordinates according to space and panel.adjust:##
# step 1: find size of a standard picture element
dx.pic      <- size.mm.to.usr(icon.cex, horizontal = TRUE)  
dx.pic.area <- dx.pic / (1 - icon.space.factor[1]) 
dy.pic      <- size.mm.to.usr(icon.cex / icon.aspect, horizontal = FALSE)  
dy.pic.area <- dy.pic / (1 - icon.space.factor[2]) 
##:compute x.y.coordinates according to space and panel.adjust##
#:83
#84:
##compute x.y.coordinates according to space and panel.adjust:##
# step 2: loop over icons
sta.type <- icon.stack.type; job.plot.cmds <- NULL
data.mat <- cbind(data.mat, .x0 = 0, .y0 = 0, .x1 = 0, .y1 = 0)
data.mat <- data.frame(data.mat) #### !!!!!!!!!!!!!!!!!!!
for(no in seq(nrow(jobs))){
  idx <- data.mat[,".job.no"] == jobs[no,".job.no"] # find items of jobs.no
  if(1 < length(icon.stack.type)){ # if icon.stack.type is a vectir use one element
    sta.type <- icon.stack.type[1 + ((no-1) %% length(icon.stack.type))]
  } 
  sta.type <- unlist(strsplit(sta.type,""))
  if(any(idx)){                
    idx.icon.stack.len <- c(1,no)[1+(1 < length(icon.stack.len))]
    xy.color.icons <- place.icons(    # elements
          jobs[no,, drop = FALSE], 
          data.mat[idx, c(".icon", ".color", ".fraction"), drop = FALSE],
          icon.stack.type = sta.type,
          icon.stack.len = icon.stack.len[ idx.icon.stack.len ],
          frame = FALSE, verbose = verbose
    ) # verbose for debugging
    h <- list(xy.color.icons); names(h) <- as.character(jobs[,".job.no"][no])
    job.plot.cmds <- c(job.plot.cmds, h); h <- c(".x0",".y0",".x1",".y1")
    data.mat[idx, h] <- xy.color.icons[, h]
  } else { "create empty panel" }
}
idx <- 2 - icon.horizontal
h <- (dx.pic.area * cbind(icon.stack.len, n.stacks)[,idx]) >
     (panel.size[,1]*1.01); h <- h[!is.na(h)]
if( 0 < length(h) && any(h) ) 
      cat("WARNING: horizontal space of panels too small!\n")
h <- (dy.pic.area * cbind(n.stacks, icon.stack.len)[,idx]) >
     (panel.size[,2]*1.01); h <- h[!is.na(h)]
if( 0 < length(h) && any(h) ) 
      cat("WARNING: vertical space of panels too small!\n")
##:compute x.y.coordinates according to space and panel.adjust##
#:84
#106:
##execute plot statements of [[iconplot]]:##
#107:
##prepare set of pictograms for plotting: [[pic.tr.set]]:##
color.used <- if(0<length(grp.color)) unique(data.mat[,".color"]) else 1
pic.used   <- if(0<length(grp.icon))  unique(data.mat[,".icon"])  else 1
pic.tr.set <- NULL
if(is.numeric(icons) && !is.matrix(icons)) icons <- as.list(icons)
if(!is.list(icons)) icons <- list(icons)
for(ip in pic.used){ 
  pic <- NULL
  
#108:
##prepare [[pic]] for picture [[ip]]:##
if( is.matrix(icons[[ip]]) ){ # case: raster graphics of grey levels
  if(is.list(icon.grey.levels)){
    grey.levels <- icon.grey.levels[[ip]]
    if(is.null(grey.levels)) grey.levels <- icon.grey.levels[[1]]
  } else grey.levels <- icon.grey.levels 
  # cat("prepare grey.levels: ", grey.levels)
  simple <- length(grey.levels) == 2 || 
           (length(grey.levels) == 1 && grey.levels <= 2)
  if( is.raster(icons[[ip]]) && # don't recolor if no grp.col and colors != 1:
      (!is.null(grp.color) || !identical(1, colors)) ){ 
    icons[[ip]] <- raster.to.greys(icons[[ip]], grey.levels = grey.levels,
                    reduce = TRUE, n.icons = dim(data.mat)[1])[[1]]
  }
  for(icolor in color.used){ # don't recolor if no grp.col and colors != 1
    if( (!is.null(grp.color) || !identical(1, colors)) || 
        (is.numeric(icons[[ip]]) && 1 < max(icons[[ip]]))){ 
      p <- greys.to.col.pic(icons[[ip]], colors[icolor], 
                            set.black.and.white = 1.5, simple = simple)
    } else { p <- as.raster(icons[[ip]]) }
    pic <- c( pic, list( p ))
  } 
} 
if( is.numeric(icons[[ip]]) ){ # case of central symbols
  for(icolor in color.used)
    pic <- c( pic, list( c(icons[[ip]], colors[icolor]) ))
} 
if( is.function(icons[[ip]]) ){ # case of pic by generator function
  for(icolor in color.used){
    pic <- c(pic, list( c(icons[[ip]], colors[icolor]) ))
  }
}
if( is.character(icons[[ip]]) ){ # case element of set of icons to be used
  for(icolor in color.used){
    pic <- c( pic, list( c(icons[[ip]], colors[icolor]) ))
  }
}
##:prepare [[pic]] for picture [[ip]]##
#:108
  pic.tr.set <- c( pic.tr.set, list(pic) )
}
##:prepare set of pictograms for plotting: [[pic.tr.set]]##
#:107
if(exists("job.plot.cmds")){
  for(i in seq(along=job.plot.cmds)){  pic.rect <- pic.color <- NULL
    cmds <- job.plot.cmds[[i]]; panel.no <- as.numeric(names(job.plot.cmds[i]))
    color.idx <- match(cmds$.color, color.used); pic.idx <- match(cmds$.pic, pic.used)
    for( pic.no in seq(along = cmds$.x0) ) { 
      pic <- pic.tr.set[[ pic.idx[pic.no] ]][[ color.idx[pic.no] ]]
      if(is.null(pic)) next
      if(is.raster(pic)){                     # case of raster graphics
        rasterImage(pic, cmds$.x0[pic.no], cmds$.y0[pic.no],
          cmds$.x1[pic.no], cmds$.y1[pic.no], interpolate = icon.draft) 
      } else {
        if( is.function( unlist( pic )[[1]] ) ){ # case generator >>fun<<
          
#354:
##generate icons by [[call.icon.generator()]]:##
pic <- unlist(pic); fn <- pic[[1]]; color <- pic[[2]]
dxy <- c(cmds$.x1[pic.no], cmds$.y1[pic.no]) - c(cmds$.x0[pic.no], cmds$.y0[pic.no])
idx <- which(data.mat[, ".job.no" ] == panel.no)[pic.no]
# additional infos for modifying
pic.args <- data.mat[idx, grp.design]; names(pic.args) <- NULL
# cat("==========="); print(grp.design); print(data.mat[idx, grp.design])
call.icon.generator(fn, xy = cbind(cmds$.x0[pic.no], cmds$.y0[pic.no]), dxy = dxy, 
                   color = color, pic.args = pic.args, ...)
##:generate icons by [[call.icon.generator()]]##
#:354
        } else {
          if( !(substring(unlist( pic )[1],1,1) %in% 0:9) ){# case generator >>name<<
            
#354:
##generate icons by [[call.icon.generator()]]:##
pic <- unlist(pic); fn <- pic[[1]]; color <- pic[[2]]
dxy <- c(cmds$.x1[pic.no], cmds$.y1[pic.no]) - c(cmds$.x0[pic.no], cmds$.y0[pic.no])
idx <- which(data.mat[, ".job.no" ] == panel.no)[pic.no]
# additional infos for modifying
pic.args <- data.mat[idx, grp.design]; names(pic.args) <- NULL
# cat("==========="); print(grp.design); print(data.mat[idx, grp.design])
call.icon.generator(fn, xy = cbind(cmds$.x0[pic.no], cmds$.y0[pic.no]), dxy = dxy, 
                   color = color, pic.args = pic.args, ...)
##:generate icons by [[call.icon.generator()]]##
#:354
           
          } else {                            #  case central symbol
            pic <- unlist(pic); pic.color <- c(pic.color, pic[2])
            pic.rect <- rbind(pic.rect,c(cmds$.x0[pic.no], cmds$.y0[pic.no],
              cmds$.x1[pic.no], cmds$.y1[pic.no], as.numeric(pic[1]))) 
          }
        }
      }
      if(icon.frame) rect(cmds$.x0[pic.no], cmds$.y0[pic.no],
                         cmds$.x1[pic.no], cmds$.y1[pic.no])
    }
    
#113:
##plot rectangles or symbols:##
if( 0 < length(pic.rect) ){ # plot boxes instead of raster images
  rect(pic.rect[,1],pic.rect[,2],pic.rect[,3],pic.rect[,4], 
       border = if(icon.frame) NULL else NA,
       col = ifelse( 0 == pic.rect[,5], pic.color, "white" ) )
  h <- abs( (pic.rect[,1] - pic.rect[,3]) / (pic.rect[,2] - pic.rect[,4] ))/
      icon.aspect * fac.in.usr/fac.in.usr2                 # 161010
  h <- pmin(h, 1/h)^0.5 # resizing of fractional elements # 161010
  points( 0.5 * (pic.rect[,1] + pic.rect[,3]),      # characters instead boxes
          0.5 * (pic.rect[,2] + pic.rect[,4]), col = pic.color,
          pch = 1 + ((pic.rect[,5] - 1) %% 18), # maximal 18 symbols available
          cex = ifelse( 0 == pic.rect[,5], 0, icon.cex/3 * h) ) #161012
}
## cat("fac.in", fac.in.usr, fac.in.usr2)
##:plot rectangles or symbols##
#:113
  }
}
##:execute plot statements of [[iconplot]]##
#:106
##:pack icons by icons packer##
#:82
}
if("numbers" %in% packer || "panel.legend" %in% packer || 0 < length(panel.text)){  
  
#89:
##pack numbers, legends or texts into panels:##
if(verbose) cat("pack numbers or text into panels by packers")
# find text positions of panels
n <- nrow(jobs) #; print(data.mat); print(jobs[1:3,])
x0 <- 0.5 * (jobs[, 1] + jobs[, 2]) 
y0 <- 0.5 * (jobs[, 3] + jobs[, 4])
sta.type <- unlist(strsplit(icon.stack.type[1],""))
if("b" %in% sta.type) y0 <- jobs[, 4]
if("t" %in% sta.type) y0 <- jobs[, 3]
if("l" %in% sta.type) x0 <- jobs[, 2]
if("r" %in% sta.type) x0 <- jobs[, 1]
job.text.cmds <- cbind(.x0=x0, .y0=y0)
txt <- NULL
# compute of numbers of panels
if( "numbers" %in% packer ){ 
  n.job <- 0 * x0
  for(no in seq(nrow(jobs))){
    idx <- data.mat[, ".job.no"] == jobs[no, ".job.no"]
    n.job[no] <- round(sum(data.mat[idx, ".fraction"]), digits = 2)
  }
  txt <- n.job
}
if( 0 < length(panel.text) ){
  p.text <- c(as.vector(panel.text), rep("", n))[1:n]
  txt <- paste(p.text, txt)
} 
if( "panel.legend" %in% packer ){ 
# compose labels combination of panels
  p.text <- "X"
  for( i in seq(along = grp.y) ){ 
    h <- substring(levels(data.mat[ , grp.y[i] ]), 1, lab.n.max[1])
    p.text <- as.vector(t(outer(p.text, h, FUN = paste, sep = "*" )))
  }
  for( i in seq(along = grp.x) ){ 
    h <- substring(levels(data.mat[ , grp.x[i] ]), 1, lab.n.max[1])
    p.text <- as.vector(t(outer( p.text, h, FUN = paste, sep = "*" )))
  }
  p.text <- substring(p.text, 3) # p.text <- letters[1:n]
  txt <- paste(p.text, txt)
} 
panel.text <- txt
#116:
##execute text statements of [[iconplot]]:##
#114:
##find graphics parameters:##
# args <- list(...) # in (7) # 170612
if(any(h <- (!(names(args) %in% c("cex")))) ) 
  cat("Remark: '...'-arguments found:", paste(names(args)[h], collapse = " "), "\n")
cex.local <- if( "cex" %in% names(args) ) args$cex else 1 # 161012
##:find graphics parameters##
#:114
  # 161012
adj <- c(0.5, 0.5)
#if("b" %in% sta.type) adj[2] <- 1; if("t" %in% sta.type) adj[2] <- 0
#if("l" %in% sta.type) adj[1] <- 1; if("r" %in% sta.type) adj[1] <- 0
if("b" %in% sta.type) adj[2] <- 0.2; if("t" %in% sta.type) adj[2] <- 0.8
if("l" %in% sta.type) adj[1] <- 0.5; if("r" %in% sta.type) adj[1] <- 0.5
if(exists("job.text.cmds") && 0 < length(job.text.cmds) ){ # pack panel.text
  if( 0 < length(panel.text) ){
    text(job.text.cmds[,".x0"], job.text.cmds[,".y0"], panel.text, 
         cex = cex.local, adj = adj)
    job.text.cmds <- NULL
  }
}
##:execute text statements of [[iconplot]]##
#:116
##:pack numbers, legends or texts into panels##
#:89
}
if("stars" %in% packer){
 
#91:
##pack stars only:##
if(verbose) cat("pack stars by stars packer")
# number of jobs
n <- nrow(jobs) 
# mids of panels
x0 <- 0.5 * (jobs[, 1] + jobs[, 2]); y0 <- 0.5 * (jobs[, 3] + jobs[, 4])
# sizes of panels in x and y
mm.h <- size.usr.to.mm(jobs[, 2] - jobs[, 1], horizontal = TRUE)
mm.v <- size.usr.to.mm(jobs[, 4] - jobs[, 3], horizontal = FALSE)
# suitable length of line segments
mm <- min( mm.h, mm.v ) * 0.5
if( is.na(icon.cex) ) icon.cex <- 1
usr.h <- size.mm.to.usr(mm, horizontal = TRUE)  * icon.cex
usr.v <- size.mm.to.usr(mm, horizontal = FALSE) * icon.cex
sta.type <- unlist(strsplit(icon.stack.type[1],""))
if("b" %in% sta.type) y0 <- jobs[, 3] + usr.v
if("t" %in% sta.type) y0 <- jobs[, 4] - usr.v
if("l" %in% sta.type) x0 <- jobs[, 1] + usr.h
if("r" %in% sta.type) x0 <- jobs[, 2] - usr.h
pi2 <- 2 * pi; shrink <- 1 - icon.space.factor
for(no in seq(nrow(jobs))){
  # find elements of panel
  idx <- data.mat[, ".job.no"] == jobs[no, ".job.no"]
  # find fracs
  fracs <- rev(data.mat[idx, ".fraction"])
  # next if nothing to do
  if( (n <- length(fracs)) == 0) next
  # end points of line segments
  x <- sin(- pi2 * (1:n)/n) * usr.h * shrink * fracs + x0[no]  
  y <- cos(  pi2 * (1:n)/n) * usr.v * shrink * fracs + y0[no]
  # color of line segments
  color.idx <- data.mat[idx, ".color"]
  # plot cmds
  segments(rep(x0[no], n), rep(y0[no], n), x, y, col = colors[color.idx])
  points(x0[no], y0[no], col = "black", cex = 1)
}
##:pack stars only##
#:91
 
}
#110:
##finish plot:##
if(verbose) cat("finish pictogram plot")
if(panel.frame)
  rect(jobs[,"xmins"], jobs[,"ymins"], jobs[,"xmaxs"], jobs[,"ymaxs"]) 
#111:
##add title:##
if( missing(main) )
  main <- paste(if("icons" %in% packer) "iconplot of", data.call)
# if( nchar(main) > 75 ) main <- substring(main,1,75) # 160927
if( x.lab.side == 3 ){
  mtext( main, side = 1, line = -0.5, 
         cex = 1.4 * (lab.cex + (lab.cex < 0.4)) ) 
} else { 
  title( paste("\n", sep = "", main )) # 160927
} 
##:add title##
#:111
if( 0.4 <= lab.cex ){
  
#119:
##add x labels:##
lab.color <- c(lab.color, lab.color)[1:2]
if( !is.null(XINTS) && x.lab.side %in% c(1,3)){
  # create matrix of intervals of separation levels 0, 1, ..
  INTS <- c(list(rbind(c(0, xmin, xmax, .5*(xmin+xmax)))), unique(XINTS))
  INTS <- matrix(unlist(lapply(INTS, t)), nrow=4)
  rownames(INTS) <- c("no", "mins", "maxs", "mids")
  ix.set <- rev(seq(along=grp.x))
  for(ix in ix.set){ # loop for labels over variables
    
#120:
##put variable name and labels of levels of [[var ix]]:##
# get x positions -------------------------------------
var.name  <- grp.x[ix]                                   # var name of      no ix
x.prior   <- INTS["mids", INTS["no",]==ix-1, drop=FALSE] # pos of var name: no ix-1
x.current <- INTS["mids", INTS["no",]==ix,   drop=FALSE] # pos of labels:   no ix
x.1.2     <- INTS[2:3,    INTS[1,]   ==ix-1, drop=FALSE] # separation lines of ix-1 
##:put variable name and labels of levels of [[var ix]]##
#:120
#121:
##put variable name and labels of levels of [[var ix]]:##
# get y positions: find line numbers and transform them
if(lab.type != "compact"){
  line.no <- lab.mar1 - 3 * ix + 1
} else {
  line.no <- lab.mar1 - ix + 1 - 1 
}
b.t.idx <- 1 + (x.lab.side == 3)
p.p.idx <- 1 + (0 == lab.parallel[1]) #161007
fac.side  <- c(-1, 1)[b.t.idx]
y         <- c(ymin, ymax)[b.t.idx] + fac.side * y.per.row * line.no
y.size.box.h   <- 1.1 * y.per.row
y.pos.var.name <- y + fac.side * y.per.row * 0.5 
y.pos.levels   <- y - fac.side * y.per.row * 0.5 * c(1, .3)[p.p.idx]
# find adjustments -------------------------------------
adj.var.name <- c(0.5, 0.5) 
if( lab.type == "expanded" & lab.parallel[1] == 0 & 1 < ix )
  adj.var.name[1] <- 1.3 # new adjust to prevent overlapping
rotate.levels <- lab.parallel[1] == 0 | 
                (lab.parallel[1] == 0.5 & length(ix.set) == ix)
adj.levels   <- c(c(0.5, c(0.0,1)[b.t.idx])[1 + rotate.levels], 0.5) 
## cat("X: adj.levels", adj.levels, "    rotate.levels", rotate.levels)
##:put variable name and labels of levels of [[var ix]]##
#:121
#123:
##put variable name and labels of levels of [[var ix]]:##
# plot light boxes -------------------------------------
if(1 <= lab.boxes && ix <= length(grp.x))
   rect(x.1.2[1,],     col = lab.color[1], border = 0, xpd=NA,
        y + fac.side * y.size.box.h * (ix == 1 | lab.type != "compact"),
        x.1.2[2,], y - fac.side * y.size.box.h * (2 <= lab.boxes))
##:put variable name and labels of levels of [[var ix]]##
#:123
#124:
##put variable name and labels of levels of [[var ix]]:##
# plot separation line ---------------------------------
if(ix <= length(grp.x))
   segments(x.1.2[1,], y, x.1.2[2,], y, xpd=NA, col=lab.color[2],
            lwd = if( 0 == (lab.boxes %% 1) ) 1 else 10*(lab.boxes%%1))
##:put variable name and labels of levels of [[var ix]]##
#:124
#125:
##put variable name and labels of levels of [[var ix]]:##
# plot var names ---------------------------------------
if( 0 < length(var.name) && !is.na(var.name)  && 
    ( ix == 1 | lab.type != "compact") ){ 
   txt <- if( lab.type == "compact" ) 
             paste(grp.x.o, collapse = " * ") else grp.x.o[ix]
   text(txt, x = x.prior, y = y.pos.var.name, # col = 2+ix-1, 
        cex = lab.cex, adj = adj.var.name, xpd=NA) 
} 
##:put variable name and labels of levels of [[var ix]]##
#:125
#126:
##put variable name and labels of levels of [[var ix]]:##
# plot labels of levels --------------------------------
if( 0 < length(var.name) && !is.na(var.name) ){ # find labels of var name 
  if(is.factor(h <- data.mat[, var.name])) lev <- levels(h) 
  else                               lev <- sort(unique(h))
  idx <- seq(1, (h <- length(x.current)), by = max(1, ceiling(h/lab.n.max[2])))
  lev.h <- rep(lev, ceiling(h/length(lev))) ### lab.char.max
  lev.h <- substring(lev.h, 1, pmin(lab.n.max[1], nchar(lev.h)))
  text(lev.h[idx], x = x.current[idx], y = y.pos.levels, adj = adj.levels, 
       srt = 90 * rotate.levels, xpd=NA, cex = lab.cex) ## , col = "red")
  # points(x = x.current[idx], x = rep(y.pos.levels, length(idx)))
}   
##:put variable name and labels of levels of [[var ix]]##
#:126
  }
}
##:add x labels##
#:119
  
#129:
##add y labels:##
if(!is.null(YINTS) && y.lab.side %in% c(2,4)){
  # create matrix of intervals of separation levels 0, 1, ..
  INTS <- c(list(rbind(c(0, ymin, ymax, .5*(ymin+ymax)))), unique(YINTS))
  INTS <- matrix(unlist(lapply(INTS, t)), nrow=4)
  rownames(INTS) <- c("no", "mins", "maxs", "mids")
  iy.set <- rev(seq(along=grp.y))
  for(iy in iy.set) {         # loop over number of variables for labels
    
#130:
##put var name and levels on left or right margin of [[var iy]]:##
# get y positions --------------------------------------
var.name  <- grp.y[iy]                                   # var name of      no iy
y.prior   <- INTS["mids", INTS["no",]==iy-1, drop=FALSE] # pos of var name: no iy-1
y.current <- INTS["mids", INTS["no",]==iy,   drop=FALSE] # pos of labels:   no iy
y.1.2     <- INTS[2:3, INTS[1,]      ==iy-1, drop=FALSE] # separation lines of iy-1 
if(lab.type != "compact"){
  line.no <- lab.mar2 - 3 * iy + 1
} else {  line.no <- lab.mar2 - iy + 1 - 1 } #???
l.r.idx   <- 1 + (y.lab.side == 4)
p.p.idx   <- 1 + (0 == lab.parallel[2])
fac.side  <- c(-1, 1)[l.r.idx]
x         <- c(xmin, xmax)[l.r.idx] + fac.side * x.per.row * line.no
x.size.box.h   <- 1.1 * x.per.row
x.pos.var.name <- x + fac.side * x.per.row * 0.5 
x.pos.levels   <- x - fac.side * x.per.row * 0.5 * c(1, .3)[p.p.idx]
# adjustments -----------------------------------------
adj.var.name   <- c(0.5, 0.5) 
if( lab.type == "expanded" & lab.parallel[2] == 0 & 1 < iy )
  adj.var.name[1] <- 1.3 # new adjust to prevent overlapping
horizontal.levels <- lab.parallel[2] == 0 | 
                    (lab.parallel[2] == 0.5 & length(iy.set) == iy)
adj.levels   <- c(c(0.5, c(0.0,1)[l.r.idx])[1 + horizontal.levels], 0.5) 
##:put var name and levels on left or right margin of [[var iy]]##
#:130
#131:
##put var name and levels on left or right margin of [[var iy]]:##
# plot light boxes ------------------------------------
if( 1 <= lab.boxes && iy <= length(grp.y)) 
   rect(           x + fac.side * x.size.box.h * 
                             (lab.type != "compact" | iy == 1),
        y.1.2[1,], x - fac.side * x.size.box.h * (2 <= lab.boxes), 
        y.1.2[2,], col = lab.color[1], border = 0, xpd=NA)
# plot white separation line --------------------------
if(iy <= length(grp.y)) 
   segments(x, y.1.2[1,], x, y.1.2[2,], xpd=NA, col=lab.color[2],
            lwd = if( 0 == (lab.boxes %% 1) ) 1 else 10 * (lab.boxes %% 1))
# plot var names --------------------------------------
if( 0 < length(var.name) && !is.na(var.name) &&
    ( iy == 1 | lab.type != "compact") ){ 
   txt <- if( lab.type == "compact" ) 
             paste(grp.y.o, collapse = " * ") else grp.y.o[iy]
   text(txt, y = y.prior, x = x.pos.var.name, # col = 2+iy-1, 
        cex = lab.cex, adj = adj.var.name, xpd=NA, srt = 90) 
} 
# plot labels of levels -------------------------------              
if( 0 < length(var.name) && !is.na(var.name)){  # put labels of levels
  if(is.factor(h <- data.mat[, var.name])) lev <- levels(h) 
  else                               lev <- sort(unique(h))
  idx <- seq(1, (h <- length(y.current)), by = max(1, ceiling(h/lab.n.max[2])))
  lev.h <- rep(lev, ceiling(h/length(lev)))
  lev.h <- substring(lev.h, 1, pmin(lab.n.max[1], nchar(lev.h)))
  text(lev.h[idx], y = y.current[idx], x = x.pos.levels, adj = adj.levels, 
       srt = 90 - 90 * horizontal.levels, xpd=NA, cex = lab.cex) # col=2+iy-1, 
  # points(y = y.current[idx], x = rep(x.pos.levels, length(idx)))
}    
##:put var name and levels on left or right margin of [[var iy]]##
#:131
  }
}
##:add y labels##
#:129
  
#134:
##add cols and icons legends:##
y0 <- ymin - y.lab.mar1 - y.mar1 # labels bottom side
if(x.lab.side == 3) y0 <- ymin - y.mar1              # level top side 
x0 <- gxmin + 2 * x.per.row; dx <- x.per.row * .7; dy <- y.per.row * .7
if( lab.legend == "vertical" ){
  y0 <- ymax - y.per.row; x0 <- xmax + x.per.row
  if(y.lab.side == 4)     x0 <- x0 + x.lab.mar2 + 0.5 * dx 
}
##:add cols and icons legends##
#:134
#135:
##add cols and icons legends:##
if( lab.legend %in% c("cols", "vertical") ){  # pic legend upright to x 
  if( pic.legend.ok ){ # FALSE, if only 1 icon used          
    # icons legend in case of  "cols" or "vertical" --------------------
    text(x0 - x.per.row, y0 - 0.5 * y.per.row, adj = c(0,0), grp.icon.o, 
         cex = lab.cex, xpd = NA)
    y <- y0.label.line <- y0 - 0.5 * y.per.row  
    if(length(icons) <= legend.n.limit){ 
      for(i in seq(along=icons)){
        if( is.na(lev.pic[i]) ) next
        pic <- icons[[ i ]]; y  <- y  - y.per.row
        
#136:
##plot symbol for pic legend, not parallel case:##
if(is.matrix(pic)){ # no recoloring
  if(!is.null(grp.color) || !identical(1, colors))
    pic <- greys.to.col.pic(pic, "#555555", simple = simple)
  rasterImage(pic, x0, y, x0+dx, y+dy, interpolate = icon.draft, xpd = NA)
} else {
  if( is.function( unlist(pic) ) || is.character( unlist(pic) )){ # pic by function
    pic <- unlist(pic); fn <- pic
    dxy <- c(dx, dy)
     # call.icon.generator(fn, xy = cbind(x0, y), dxy = dxy, color = "darkgrey") #180312 :
     call.icon.generator(fn, xy = cbind(x0, y), dxy = dxy, color = "grey", pic.args = pic.args) 
  } else { 
    pic <- unlist(pic)
    points( 0.5*(x0 + (x0 + dx)), 0.5 * (y + (y + dy)), xpd =NA,
            cex = 1*lab.cex, pch = as.numeric(pic[1]), col = "black" )
  } 
}
##:plot symbol for pic legend, not parallel case##
#:136
        text(x0 + 1 * x.per.row, y, adj = c(0,0), lev.pic[i],
             cex = lab.cex, xpd = NA)
        if(icon.frame) rect(x0, y, x0 + dx, y + dy, xpd=NA)  
      }
    }
    if( lab.legend == "cols") x0 <- (gxmin + gxmax) / 2 + x.per.row
    if( lab.legend == "vertical") y0 <- y0 - (2 + length(icons)) * y.per.row  
  }
  
#137:
##add color legend, not parallel case:##
# color legend in case of  "cols" or "vertical" ------------------------------
if(0 < length(grp.color) && color.legend.ok ){                        # color legend 
  text(x0 - x.per.row, y0 - 0.5 * y.per.row, adj = c(0,0), grp.color.o, 
       cex = lab.cex, xpd = NA)
  y <- y0.label.line  <- y0 - 0.5 * y.per.row  
  if( length(colors) > legend.n.color ) ## CCL
    cat("Remark: some color legend labels skipped\n")    
  color.labels.idx.set <- seq(1, length(colors), by = legend.color.skip + 1)    # CCL
  for(i in seq(along=colors)) {
    y  <- y - y.per.row / (1 + legend.color.skip)
    rect(x0, y, x0 + dx, y + dy, col = colors[i], xpd=NA, border = NA)
    if( is.na(lev.color[i]) ) next
    if( i %in% color.labels.idx.set )                                           # CCL
      text(x0 + 1 * x.per.row, y, adj = c(0,0), lev.color[i], cex = lab.cex, xpd = NA)
    if(icon.frame) rect(x0, y, x0 + dx, y + dy, xpd=NA)  
  } 
}
##:add color legend, not parallel case##
#:137
}
##:add cols and icons legends##
#:135
#138:
##add cols and icons legends:##
if( lab.legend %in% c("rows", "skewed", "horizontal") ){ # legend parallel
  if( pic.legend.ok ){               # icons labels
    if(length(icons) <= legend.n.limit){ 
      text(x0 - x.per.row, y0 - 0.5 * y.per.row, grp.icon.o, cex = lab.cex, 
           adj = c(0, 0), xpd = NA )                             # variable name
      y0.label.line  <- y0 - (y.delta.rot + y.per.row) - 0.5 * y.per.row 
      res <- find.xlab.coor( lev.pic, y0.label.line )
      xr0 <- res[,"xr0"]; xr1 <- res[,"xr1"]
      yr0 <- res[,"yr0"]; yr1 <- res[,"yr1"]
      xlab <- res[,"xlab"]; ylab <- res[,"ylab"]
      for(i in seq(along=icons)) {
        if( is.na(lev.pic[i]) ) next
        pic <- icons[[ i ]]
        
#139:
##plot symbol for pic legend, parallel layout:##
if(is.matrix(pic)){
  pic <- greys.to.col.pic(pic, "#555555", simple = simple)
  rasterImage(pic, xr0[i], yr0[i], xr1[i], yr1[i], interpolate = icon.draft)
} else {
  if( is.function( unlist(pic) ) || is.character( unlist(pic) ) ){
    pic <- unlist(pic); fn <- pic
    dxy <- c(dx, dy)
     # call.icon.generator(fn, xy = cbind(xr0[i], yr0[i]), dxy=dxy, color = "darkgrey") #180312 :
     call.icon.generator(fn, xy = cbind(xr0[i], yr0[i]), dxy = dxy, color = "grey", 
                        pic.args = pic.args) 
  } else {
    pic <- unlist(pic)
    points( 0.5*(xr0[i] + xr1[i]), 0.5 * (yr0[i] + yr1[i]), cex = 1*lab.cex,
            pch = as.numeric(pic[1]), col = "black")
  }
}
##:plot symbol for pic legend, parallel layout##
#:139
      }
      if(icon.frame) rect(xr0, yr0,  xr1, yr1, xpd=NA) 
    }
    srt <- c(0, 45, 90)[ which( lab.legend == c("rows","skewed","horizontal")) ]
    text(xlab, ylab, adj = c(0,0), xpd = NA, lev.pic, cex = lab.cex, srt = srt )
    # increment of x0 and y0 in case of a color legend
    if( lab.legend == "rows")       y0 <- y0               - y.per.row * 2   
    if( lab.legend == "skewed")     y0 <- y0 - y.delta.rot - y.per.row * 2
    if( lab.legend == "horizontal") 
      x0 <- xr1[length(icons)] + (1 + nchar(grp.icon.o)/3) * x.per.row 
  }
  
#140:
##add color legend, parallel layout:##
# color legend in case of  "rows", "skewed" or "horizontal"
if(0 < length(grp.color) && color.legend.ok ){    # color labs
  text(x0 - x.per.row, y0 - 0.5 * y.per.row, grp.color.o, cex = lab.cex,
       adj = c(0, 0), xpd = NA )                        # variable name
  y0.label.line  <- y0 - (y.delta.rot + y.per.row) - 0.5 * y.per.row 
  idx <- seq(1, length(colors), by = legend.color.skip + 1) 
  if( lab.legend == "rows"){
    res <- find.xlab.coor( lev.color[idx], y0.label.line, skip = 0 )
    xr0 <- res[,"xr0"]; xr1 <- res[,"xr1"]; yr0 <- res[,"yr0"]; yr1 <- res[,"yr1"]
    xlab <- res[,"xlab"]; ylab <- res[,"ylab"]
    rect(xr0, yr0, xr1, yr1, col = colors[idx], xpd=NA, border = NA)
    srt <- c(0, 45, 90)[ which( lab.legend == c("rows", "skewed", "horizontal")) ]
    text(xlab, ylab, adj = c(0,0), xpd = NA, lev.color[idx], cex = lab.cex,
         srt = srt)
  } else {
    res <- find.xlab.coor( lev.color, y0.label.line, skip = legend.color.skip )
    xr0 <- res[,"xr0"]; xr1 <- res[,"xr1"]; yr0 <- res[,"yr0"]; yr1 <- res[,"yr1"]
    xlab <- res[,"xlab"]; ylab <- res[,"ylab"]
    rect(xr0, yr0, xr1, yr1, col = colors, xpd=NA, border = NA)
    srt <- c(0, 45, 90)[ which( lab.legend == c("rows", "skewed", "horizontal")) ]
    text(xlab[idx], ylab[idx], adj = c(0,0), xpd = NA, lev.color[idx], cex = lab.cex,
         srt = srt)
  }
  if(icon.frame) rect(xr0, yr0, xr1, yr1, xpd=NA) 
  if( length(colors) > legend.n.color ) 
    cat("Remark: some color legend labels skipped\n")    
} 
##:add color legend, parallel layout##
#:140
} 
##:add cols and icons legends##
#:138
#155:
##add cols and icons legends:##
if(verbose) text( usr[2], usr[3], arguments, adj = c(1,0), xpd=NA, cex = 0.7)
##:add cols and icons legends##
#:155
}
#150:
##reinstall graphics old parameter:##
newpar <- par(no.readonly = TRUE) 
par(oldpar)
##:reinstall graphics old parameter##
#:150
##:finish plot##
#:110
#151:
##construct output:##
jobs <- jobs[order(jobs[,"col"]),,drop = FALSE]
jobs <- jobs[order(jobs[,"row"]),,drop = FALSE]; rownames(jobs) <- NULL
invisible(list(jobs = jobs, data.mat = data.mat, newpar = newpar))
##:construct output##
#:151
##:body of [[iconplot]]##
#:8
}
#:2
##:define [[iconplot]]##
