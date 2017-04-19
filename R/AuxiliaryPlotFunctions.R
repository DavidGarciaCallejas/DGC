# Auxiliary plot functions and color parameters

#########################
#########################

#' color-blind palette
#'
#' @return Hex codes of a color-blind palette
#' @export
#'
cbPalette <- function(){
  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
}

#' Colors I usually use
#'
#' @return six common-used colors from a color-blind palette
#' @export
#'
palette.values <- function(){
  c("#E69F00", "#56B4E9", "#009E73","#0072B2", "#D55E00", "#CC79A7")
}

#########################
#########################

#' Draw multiple ggplot objects
#'
#' @param ... plot objects
#' @param plotlist list of plot objects
#' @param file 
#' @param cols number of columns
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @return draws a graphical object with grid.newpage and print
#' @export
#'
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





#' draw several plots with only one common legend
#'
#' @param ... ggplot2 objects
#' @param ncol number of columns
#' @param nrow number of rows
#' @param position position of the legend
#'
#' @return graphical output from function grid.draw
#' @export
#'
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

# this function (from http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot?lq=1)
# adjusts the x-axis on facet_wrap with uneven number of columns

#' Adjust x-axis on facet_wrap 
#'
#' this function (from http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot?lq=1)
#' adjusts the x-axis on facet_wrap with uneven number of columns
#'
#' @param x plot object
#' @param pos up or down
#' @param newpage 
#' @param vp 
#'
#' @return graphical output from function grid.draw
#' @export
#'
#'
facetAdjust <- function(x, pos = c("up", "down"), 
                        newpage = is.null(vp), vp = NULL)
{
  # part of print.ggplot
  ggplot2:::set_last_plot(x)
  if(newpage)
    grid.newpage()
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p)
  # finding dimensions
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  # number of panels in the plot
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  # missing panels
  n <- space - panels
  # checking whether modifications are needed
  if(panels != space){
    # indices of panels to fix
    idx <- (space - ncol - n + 1):(space - ncol)
    # copying x-axis of the last existing panel to the chosen panels 
    # in the row above
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      # if pos == down then shifting labels down to the same level as 
      # the x-axis of last panel
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  # again part of print.ggplot, plotting adjusted version
  if(is.null(vp)){
    grid.draw(gtable)
  }
  else{
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }
  invisible(p)
}



#' Theme for publication
#'
#' @param base_size font size
#' @param base_family font family
#'
#' @return apply the theme to a ggplot object
#' @export
#'
theme_Publication <- function(base_size=12, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            # panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.y = element_line(colour="black"),
            axis.line.x = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size= unit(0.5, "cm"),
            legend.spacing = unit(0.1, "cm"),
            # legend.title = element_text(face="italic"),
            plot.margin=grid::unit(c(0,0,0,0),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="white"),
            strip.text = element_text(),
            panel.border = element_rect(colour = "grey50", fill=NA, size=0.5)
    ))
  
}


#' Auxiliary fill scale for theme_Publication
#'
#' @param ... 
#'
#' @return a ggplot discrete scale
#' @export
#'
scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

#' Auxiliary color scale for theme_Publication
#'
#' @param ... 
#'
#' @return a ggplot discrete scale
#' @export
#'
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{ColorVectors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @author Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#' @references \url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @keywords hplot
#'
#' @examples
#' plot_crayons()
#'
#' @export
#' @importFrom grDevices rgb2hsv
#' @importFrom graphics par plot rect text
#'
plot_crayons <-
  function(method2order=c("hsv", "cluster"), cex=0.6, mar=rep(0.1, 4))
  {
    method2order <- match.arg(method2order)
    
    crayons <- ColorVectors("crayons")
    
    # get rgb
    colval <- col2rgb(crayons)
    
    if(method2order == "hsv") {
      # convert to hsv
      colval <- t(rgb2hsv(colval))
      
      # order the colors; first two lines are to get black/gray/silver/white first
      ord <- order(names(crayons)!="Black", names(crayons)!="Gray",
                   names(crayons)!="Silver", names(crayons)!="White",
                   colval[,1], colval[,2], colval[,3])
      
    } else {
      ord <- hclust(dist(t(colval)))$ord
    }
    
    oldmar <- par("mar")
    on.exit(par(mar=oldmar))
    
    par(mar=mar)
    x <- (1:7)-1
    y <- (1:19)-1
    x <- rep(x, each=19)
    y <- rep(y, 7)
    
    plot(0, 0, type="n", xlab="", ylab="", xaxs="i", yaxs="i",
         xlim=c(0, max(x)+1), ylim=c(max(y)+0.5, -0.5),
         xaxt="n", yaxt="n")
    
    dx <- 0.2
    dy <- 0.4
    rect(x+dx/4, y-dy, x+dx, y+dy, border="black",
         col=crayons[ord])
    
    text(x+dx*1.2, y, names(crayons)[ord], cex=cex, adj=c(0, 0.5))
  }

#' Vectors of colors for figures
#'
#' Creates different vectors of related colors that may be useful for figures.
#'
#' @param set Character string indicating a set of colors.
#'
#' @return Vector of character strings representing the chosen set of colors, in RGB.
#'
#' @author Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#' @seealso \code{\link{plot_crayons}}
#' @keywords utilities
#'
#' @examples
#' plot(0, 0, type="n", xlab="", ylab="", xlim=c(0, 9), ylim=c(7.5, 0), yaxs="i",
#'      xaxt="n", yaxt="n", mar=c(0.6, 5.1, 0.6, 0.6), xaxs="i")
#' axis(side=2, at=1:7, c("general", "bg", "bgpng", "CC", "f2", "sex", "main"), las=1)
#'
#' gen <- ColorVectors("general")
#' points(seq(along=gen), rep(1,length(gen)), pch=21, bg=gen, cex=4)
#' text(seq(along=gen), rep(c(0.55, 0.7), length(gen))[seq(along=gen)], names(gen))
#'
#' points(1, 2, pch=21, bg=ColorVectors("bg"), cex=4)
#' points(1, 3, pch=21, bg=ColorVectors("bgpng"), cex=4)
#'
#' CC <- ColorVectors("CC")
#' points(seq(along=CC), rep(4,length(CC)), pch=21, bg=CC, cex=4)
#' text(seq(along=CC), rep(3+c(0.55, 0.7), length(CC))[seq(along=CC)], names(CC))
#'
#' f2 <- ColorVectors("f2")
#' points(seq(along=f2), rep(5,length(f2)), pch=21, bg=f2, cex=4)
#' text(seq(along=f2), rep(4.7, length(f2)), names(f2))
#'
#' sex <- ColorVectors("sex")
#' points(seq(along=sex), rep(6,length(sex)), pch=21, bg=sex, cex=4)
#' text(seq(along=sex), rep(5.7, length(sex)), names(sex))
#'
#' points(1, 7, pch=21, bg=ColorVectors("main"), cex=4)
#'
#' @export
#'
ColorVectors <-
  function(set=c("general", "bg", "bgpng", "CC", "f2", "sex", "main", "crayons"))
  {
    general <- c('lightblue'  =rgb(102,203,254,maxColorValue=255),
                 'hotpink'    =rgb(254,  0,128,maxColorValue=255),
                 'pink'       =rgb(254,102,254,maxColorValue=255),
                 'green'      =rgb(102,254,102,maxColorValue=255),
                 'purple'     =rgb(128,  0,128,maxColorValue=255),
                 'lightpurple'=rgb(203,102,254,maxColorValue=255),
                 'yellow'     =rgb(254,203,102,maxColorValue=255),
                 'darkblue'   =rgb(  0,128,128,maxColorValue=255))
    
    bg <- rgb(24, 24, 24, maxColorValue=255)
    bgpng <- rgb(32, 32, 32, maxColorValue=255)
    
    # text
    text <- c('yellow'   =rgb(255, 255, 102, maxColorValue=255),
              'lightblue'=rgb(102, 204, 255, maxColorValue=255),
              'pink'    =rgb(255, 102, 255, maxColorValue=255))
    
    CC <- c("AJ"  =rgb(240,240,  0,maxColorValue=255),
            "B6"  =rgb(128,128,128,maxColorValue=255),
            "129" =rgb(240,128,128,maxColorValue=255),
            "NOD" =rgb( 16, 16,240,maxColorValue=255),
            "NZO" =rgb(  0,160,240,maxColorValue=255),
            "CAST"=rgb(  0,160,  0,maxColorValue=255),
            "PWK" =rgb(240,  0,  0,maxColorValue=255),
            "WSB" =rgb(144,  0,224,maxColorValue=255))
    
    f2 <- c(AA=as.character(CC[1]), AB=rgb(0, 200, 0, maxColorValue=255), BB=as.character(CC[5]))
    sex <- c(female=rgb(255,80,80, maxColorValue=255), male=as.character(CC[5]))
    
    main <- rgb(0, 64, 128, maxColorValue=255)
    
    crayons = c("Almond"="#efdecd",
                "Antique Brass"="#cd9575",
                "Apricot"="#fdd9b5",
                "Aquamarine"="#78dbe2",
                "Asparagus"="#87a96b",
                "Atomic Tangerine"="#ffa474",
                "Banana Mania"="#fae7b5",
                "Beaver"="#9f8170",
                "Bittersweet"="#fd7c6e",
                "Black"="#000000",
                "Blizzard Blue"="#ace5ee",
                "Blue"="#1f75fe",
                "Blue Bell"="#a2a2d0",
                "Blue Gray"="#6699cc",
                "Blue Green"="#0d98ba",
                "Blue Violet"="#7366bd",
                "Blush"="#de5d83",
                "Brick Red"="#cb4154",
                "Brown"="#b4674d",
                "Burnt Orange"="#ff7f49",
                "Burnt Sienna"="#ea7e5d",
                "Cadet Blue"="#b0b7c6",
                "Canary"="#ffff99",
                "Caribbean Green"="#00CC99",
                "Carnation Pink"="#ffaacc",
                "Cerise"="#dd4492",
                "Cerulean"="#1dacd6",
                "Chestnut"="#bc5d58",
                "Copper"="#dd9475",
                "Cornflower"="#9aceeb",
                "Cotton Candy"="#ffbcd9",
                "Dandelion"="#fddb6d",
                "Denim"="#2b6cc4",
                "Desert Sand"="#efcdb8",
                "Eggplant"="#6e5160",
                "Electric Lime"="#ceff1d",
                "Fern"="#71bc78",
                "Forest Green"="#6dae81",
                "Fuchsia"="#c364c5",
                "Fuzzy Wuzzy"="#cc6666",
                "Gold"="#e7c697",
                "Goldenrod"="#fcd975",
                "Granny Smith Apple"="#a8e4a0",
                "Gray"="#95918c",
                "Green"="#1cac78",
                "Green Blue"="#1164b4",
                "Green Yellow"="#f0e891",
                "Hot Magenta"="#ff1dce",
                "Inchworm"="#b2ec5d",
                "Indigo"="#5d76cb",
                "Jazzberry Jam"="#ca3767",
                "Jungle Green"="#3bb08f",
                "Laser Lemon"="#fefe22",
                "Lavender"="#fcb4d5",
                "Lemon Yellow"="#fff44f",
                "Macaroni and Cheese"="#ffbd88",
                "Magenta"="#f664af",
                "Magic Mint"="#aaf0d1",
                "Mahogany"="#cd4a4c",
                "Maize"="#edd19c",
                "Manatee"="#979aaa",
                "Mango Tango"="#ff8243",
                "Maroon"="#c8385a",
                "Mauvelous"="#ef98aa",
                "Melon"="#fdbcb4",
                "Midnight Blue"="#1a4876",
                "Mountain Meadow"="#30ba8f",
                "Mulberry"="#c54b8c",
                "Navy Blue"="#1974d2",
                "Neon Carrot"="#ffa343",
                "Olive Green"="#bab86c",
                "Orange"="#ff7538",
                "Orange Red"="#ff2b2b",
                "Orange Yellow"="#f8d568",
                "Orchid"="#e6a8d7",
                "Outer Space"="#414a4c",
                "Outrageous Orange"="#ff6e4a",
                "Pacific Blue"="#1ca9c9",
                "Peach"="#ffcfab",
                "Periwinkle"="#c5d0e6",
                "Piggy Pink"="#fddde6",
                "Pine Green"="#158078",
                "Pink Flamingo"="#fc74fd",
                "Pink Sherbert"="#f78fa7",
                "Plum"="#8e4585",
                "Purple Heart"="#7442c8",
                "Purple Mountain's Majesty"="#9d81ba",
                "Purple Pizzazz"="#fe4eda",
                "Radical Red"="#ff496c",
                "Raw Sienna"="#d68a59",
                "Raw Umber"="#714b23",
                "Razzle Dazzle Rose"="#ff48d0",
                "Razzmatazz"="#e3256b",
                "Red"="#ee204d",
                "Red Orange"="#ff5349",
                "Red Violet"="#c0448f",
                "Robin's Egg Blue"="#1fcecb",
                "Royal Purple"="#7851a9",
                "Salmon"="#ff9baa",
                "Scarlet"="#fc2847",
                "Screamin' Green"="#76ff7a",
                "Sea Green"="#93dfb8",
                "Sepia"="#a5694f",
                "Shadow"="#8a795d",
                "Shamrock"="#45cea2",
                "Shocking Pink"="#fb7efd",
                "Silver"="#cdc5c2",
                "Sky Blue"="#80daeb",
                "Spring Green"="#eceabe",
                "Sunglow"="#ffcf48",
                "Sunset Orange"="#fd5e53",
                "Tan"="#faa76c",
                "Teal Blue"="#18a7b5",
                "Thistle"="#ebc7df",
                "Tickle Me Pink"="#fc89ac",
                "Timberwolf"="#dbd7d2",
                "Tropical Rain Forest"="#17806d",
                "Tumbleweed"="#deaa88",
                "Turquoise Blue"="#77dde7",
                "Unmellow Yellow"="#ffff66",
                "Violet (Purple)"="#926eae",
                "Violet Blue"="#324ab2",
                "Violet Red"="#f75394",
                "Vivid Tangerine"="#ffa089",
                "Vivid Violet"="#8f509d",
                "White"="#FFFFFF",
                "Wild Blue Yonder"="#a2add0",
                "Wild Strawberry"="#ff43a4",
                "Wild Watermelon"="#fc6c85",
                "Wisteria"="#cda4de",
                "Yellow"="#fce883",
                "Yellow Green"="#c5e384",
                "Yellow Orange"="#ffae42")
    
    switch(match.arg(set),
           general=general,
           bg=bg,
           bgpng=bgpng,
           CC=CC,
           f2=f2,
           sex=sex,
           main=main,
           crayons=crayons)
  }
