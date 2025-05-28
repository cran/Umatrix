plotMatrix <- function(Matrix = NULL, BestMatches=NULL, Cls = NULL, ClsColors=NULL,
                       ColorStyle = "Umatrix", Toroid=TRUE, BmSize=2, DrawLegend=F,
                       FixedRatio=T, CutoutPol=NULL, Nrlevels = NULL, TransparentContours = T,
                       Imx = NULL, Clean=F, RemoveOcean=F, TransparentOcean = FALSE,
                       Title = NULL, BestMatchesLabels = NULL,
                       BestMatchesLabelStyle = c(), BestMatchesShape=19, MarkDuplicatedBestMatches = F, YellowCircle = F){

  if(is.null(ClsColors)) ClsColors=DefaultColorSequence()
  if(ColorStyle == "Umatrix") Colormap = c("#3C6DF0","#3C6DF0","#3C6DF0","#006602","#006A02","#006D01","#007101","#007501","#007901","#007C00","#008000","#068103","#118408","#0B8305","#17860A","#1D870D","#228810","#288A12","#2E8B15","#348D18","#398E1A","#3F8F1D","#45911F","#4A9222","#509325","#569527","#5C962A","#61982C","#67992F","#6D9A32","#729C34","#789D37","#7E9F39","#84A03C","#89A13F","#8FA341","#95A444","#9AA547","#A0A749","#A6A84C","#ACAA4E","#B1AB51","#B7AC54","#BDAE56","#C3AF59","#C8B15B","#CEB25E","#CBAF5C","#C8AC59","#C5A957","#C3A654","#C0A352","#BDA050","#BA9D4D","#B7994B","#B49648","#B29346","#AF9044","#AC8D41","#A98A3F","#A6873C","#A3843A","#A08138","#9E7E35","#9B7B33","#987830","#95752E","#92722B","#8F6E29","#8C6B27","#8A6824","#876522","#84621F","#815F1D","#7E5C1B","#7B5918","#795616","#765313","#714E0F","#6C480B","#674307","#6F4D15","#785822","#806230","#896D3E","#91774C","#998159","#A28C67","#AA9675","#B3A183","#BBAB90","#C3B59E","#CCC0AC","#D4CABA","#DDD5C7","#E5DFD5","#E7E1D8","#E9E4DB","#EBE6DE","#ECE8E1","#EEEAE4","#F0EDE7","#F2EFEA","#F4F1ED","#F6F4F0","#F8F6F3","#F9F8F6","#FBFAF9","#FDFDFC","#FFFFFF","#FFFFFF","#FEFEFE","#FEFEFE","#FEFEFE","#FDFDFD","#FDFDFD","#FDFDFD","#FCFCFC","#FCFCFC","#FCFCFC","#FBFBFB","#FBFBFB","#FBFBFB","#FAFAFA","#FAFAFA","#FAFAFA","#F9F9F9","#F9F9F9","#FFFFFF","#FFFFFF")
  else if(ColorStyle == "Pmatrix") Colormap = c("#FFFFFF","#FFFFF7","#FFFFEF","#FFFFE7","#FFFFDF","#FFFFD7","#FFFFCF","#FFFFC7","#FFFFBF","#FFFFB7","#FFFFAF","#FFFFA7","#FFFF9F","#FFFF97","#FFFF8F","#FFFF87","#FFFF80","#FFFF78","#FFFF70","#FFFF68","#FFFF60","#FFFF58","#FFFF50","#FFFF48","#FFFF40","#FFFF38","#FFFF30","#FFFF28","#FFFF20","#FFFF18","#FFFF10","#FFFF08","#FFFF00","#FFFA00","#FFF400","#FFEF00","#FFEA00","#FFE400","#FFDF00","#FFDA00","#FFD400","#FFCF00","#FFCA00","#FFC500","#FFBF00","#FFBA00","#FFB500","#FFAF00","#FFAA00","#FFA500","#FF9F00","#FF9A00","#FF9500","#FF8F00","#FF8A00","#FF8500","#FF8000","#FF7A00","#FF7500","#FF7000","#FF6A00","#FF6500","#FF6000","#FF5A00","#FF5500","#FF5000","#FF4A00","#FF4500","#FF4000","#FF3A00","#FF3500","#FF3000","#FF2B00","#FF2500","#FF2000","#FF1B00","#FF1500","#FF1000","#FF0B00","#FF0500","#FF0000","#FA0000","#F40000","#EF0000","#EA0000","#E40000","#DF0000","#DA0000","#D40000","#CF0000","#CA0000","#C50000","#BF0000","#BA0000","#B50000","#AF0000","#AA0000","#A50000","#9F0000","#9A0000","#950000","#8F0000","#8A0000","#850000","#800000","#7A0000","#750000","#700000","#6A0000","#650000","#600000","#5A0000","#550000","#500000","#4A0000","#450000","#400000","#3A0000","#350000","#300000","#2B0000","#250000","#200000","#1B0000","#150000","#100000","#0B0000","#050000")
  else if(ColorStyle == "Empty") Colormap = rep("white", 100)
  else stop("ColorStyle not found.")

  # In a previous version, BestMatches with size 2 were drawn much smaller.
  # This proportion is maintained here.
  BmSize = BmSize / 2

  ################
  #### bmuLabel Standard Style
  ################
  if(is.null(BestMatchesLabelStyle$color)) BestMatchesLabelStyle$color = "white"
  if(is.null(BestMatchesLabelStyle$fontface)) BestMatchesLabelStyle$fontface = "bold"
  if(is.null(BestMatchesLabelStyle$fill)) BestMatchesLabelStyle$fill = "red"
  if(is.null(BestMatchesLabelStyle$alpha)) BestMatchesLabelStyle$alpha = 0.5
  if(is.null(BestMatchesLabelStyle$angle)) BestMatchesLabelStyle$angle = 0
  if(is.null(BestMatchesLabelStyle$size)) BestMatchesLabelStyle$size = 4
  if(is.null(BestMatchesLabelStyle$nudge_x)) BestMatchesLabelStyle$nudge_x = 0
  if(is.null(BestMatchesLabelStyle$nudge_y)) BestMatchesLabelStyle$nudge_y = 2

  if(!Toroid){
    Imx = NULL
    RemoveOcean = FALSE
    TransparentOcean = FALSE
  }

  #########
  #########

  if(is.null(Matrix))
    stop("Matrix needs to be given")

  if(!is.matrix(Matrix)){
    stop("Matrix has to be of type matrix")
  }
  if(length(dim(Matrix))!=2)
    stop("Matrix has to be a of type matrix, not an array")

  if(!is.null(Cls)){
    if(length(ClsColors) < length(DefaultColorSequence()))
      ClsColors = c(ClsColors, DefaultColorSequence()[(length(ClsColors)+1):(length(DefaultColorSequence()))])
    if(max(Cls) > length(ClsColors))
      stop(paste("The amount of given Colors (ClsColors) is not enough for the Cls. The highest Cls Value is",
                 max(Cls),"while the number of ClsColors only reaches to",
                 length(ClsColors)))
  }

  # Normalization of Umatrix values
  # Milligan, Copper 1988 A Study of Standardization of Variables in Cluster Analysis,
  # robust Normalization Z_5: "Z_5 is bounded by 0.0 and 1.0 with at least one observed value at each of these end points"
  quants=quantile(as.vector(Matrix),c(0.01,0.5,0.99),na.rm = T)
  minU=quants[1]
  maxU=quants[3]
  # Ratio between minHeight/maxHeight=1/HeightScale

  Matrix=(Matrix-minU)/(maxU-minU)

  quants2=quantile(as.vector(Matrix),c(0.01,0.5,0.99),na.rm = T)
  minU2=quants2[1]
  maxU2=quants2[3]

  ### Estimate height from Umatrix
  # Ratio between minHeight/maxHeight=1/HeightScale
  if(is.null(Nrlevels)){
    Nrlevels=round(maxU2/max(minU2,0.05),0)
  }
  # The levels must be set before limiting the values to 0 and 1,
  # but after the value range has been shifted, so that the density in the boundary regions can be estimated
  indMax=which(Matrix>1,arr.ind=T)
  indMin=which(Matrix<0,arr.ind=T)

  if(length(indMax)>0)
    Matrix[indMax]=1
  if(length(indMin)>0)
    Matrix[indMin]=0

  BestMatches = CheckBestMatches(BestMatches, Cls)
  CheckUmatrix(Matrix, shiny=F)
  CheckImx(Imx, shiny=F)

  # force rownames. this keeps log of the "original position" of the bestmatch within the list.
  # that is necessary because of the way BestMatchesLabels is currently defined.
  if(!is.null(BestMatches)){
    if(is.null(rownames(BestMatches))) rownames(BestMatches) <- 1:nrow(BestMatches)
  }

  # link BestMatchesLabels with BestmatchKey
  if((!is.null(BestMatches)) & (!is.null(BestMatchesLabels))){
    BestMatchesLabels <- cbind(BestMatches[,1], BestMatchesLabels)
  }

  Nrlevels2=Nrlevels # color intervals, not contours!
  # get Toroid values for the Umatrix
  if(Toroid){
    tU <- ToroidUmatrix(Matrix, BestMatches, Cls)
    Matrix <- tU$Umatrix
    BestMatches <- tU$BestMatches
    Cls <- tU$Cls
  }

  # configure filter, so that every bestmatch stays in
  if(!is.null(BestMatches)){
    BestMatchesFilter = rep(T,nrow(BestMatches)) # every Bestmatch stays
  }

  # put Imx on Umatrix and bestmatches if given
  if(!is.null(Imx)){
    for(i in 1:nrow(Imx)){
      for(j in 1:ncol(Imx)){
        if(Imx[i,j] == 1){
          Matrix[i,j] = NA
          if(!is.null(BestMatches))
            BestMatchesFilter[(BestMatches[,2] == i) & (BestMatches[,3] == j)] = F
        }
      }
    }

    if(!is.null(BestMatches)) BestMatches = BestMatches[BestMatchesFilter,]
    if((!is.null(Cls)) & (!is.null(BestMatches))) Cls = Cls[BestMatchesFilter]
  }

  #### remove ocean around Umatrix
  if(RemoveOcean){
    oceanLine = !apply(Matrix, 1, function(x) any(x != -1))
    startLine = min(which(!oceanLine),na.rm=T)
    endLine = length(oceanLine) - min(which(rev(!oceanLine)),na.rm=T) + 1

    oceanCol = !apply(Matrix, 2, function(x) any(x != -1))
    startCol = min(which(!oceanCol),na.rm=T)
    endCol = length(oceanCol) - min(which(rev(!oceanCol)),na.rm=T) + 1

    if(!is.null(BestMatches)){
      BestMatches <- BestMatches - cbind(rep(0,nrow(BestMatches)),startLine-1,startCol-1)
    }
    Matrix <- Matrix[startLine:endLine,startCol:endCol]
  }

  if(!TransparentOcean) Matrix[which(is.na(Matrix))] = 0

  nrows = nrow(Matrix)
  ncols = ncol(Matrix)

  colorx = Colormap

  # define level intervals
  # 35 levels
  # upper limit a bit above 1 so that every point inside the Umatrix is below it
  levelBreaks <- seq(0,1.000001,length.out=(Nrlevels2+1))

  # change Umatrix into 35 fixed values
  unfixedMatrix <- Matrix
  for(i in 1:Nrlevels2){
    Matrix[ (Matrix >= levelBreaks[i]) & (Matrix <= levelBreaks[i+1]) ] = levelBreaks[i]
  }

  duplicatedBestMatches = matrix(,ncol=2, nrow=0)
  # find duplicated bestmatches
  if(!is.null(BestMatches)){
    dup = duplicated(BestMatches[,1], fromLast = T) | duplicated(BestMatches[,1])
    duplicatedBestMatches = BestMatches[dup,,drop=F]
  }

  # transparency of the contours
  if(TransparentContours) alpha = 0.5
  else alpha = 1

  ###############
  # draw the Umatrix with ggplot
  ###############
  #### convert Umatrix into a dataframe
  unnamedMatrix <- Matrix
  colnames(unnamedMatrix) <- NULL
  rownames(unnamedMatrix) <- NULL

  dfMatrix <- melt(unnamedMatrix)
  colnames(dfMatrix) <- c("y","x","z")

  dfMatrix2 = dfMatrix
  dfMatrix2[is.na(dfMatrix$z),3]=0

  MatrixPlot <- ggplot(dfMatrix, aes_string('x','y')) +   # data layer
    geom_tile(aes_string(fill = 'z'))+

    scale_fill_gradientn(colours=colorx,space='Lab', na.value="transparent")+
    stat_contour(aes_string(z='z'), data=dfMatrix2,bins=Nrlevels,size = 0.25,color='black',alpha=alpha,na.rm = F)+ # 80% of runtime is for contours
    ylab("Lines (y)")+xlab("Columns (x)")

  if(!DrawLegend) MatrixPlot <- MatrixPlot + theme(legend.position="none")

  if(FixedRatio)
    MatrixPlot <- MatrixPlot + coord_fixed(1,xlim=c(0.5,ncols+0.5),ylim=c(nrows+0.5,0.5), expand=0)
  else
    MatrixPlot <- MatrixPlot + coord_cartesian(xlim=c(0.5,ncols+0.5),ylim=c(nrows+0.5,0.5), expand=0)

  if(Clean){
    if(is.null(Title)){
      MatrixPlot <- MatrixPlot + theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.ticks.length = unit(0.01,"lines"),
        panel.grid = element_blank(),
        axis.title.x=element_blank(),
        plot.margin = unit( c(0.001,0.001,0.001,0.001) , "lines" ),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        panel.background = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank(),
        plot.background=element_rect(fill="white", colour=NA)

      ) + labs('x'=NULL, 'y'=NULL)

      MatrixPlot <- MatrixPlot +theme(panel.spacing = unit(0,"null"))
    }
    else{
      MatrixPlot <- MatrixPlot + theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.ticks.length = unit(0.01,"lines"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),

        panel.background = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank(),
        plot.background=element_rect(fill="white", colour=NA)

      ) + labs('x'=NULL, 'y'=NULL)
    }
  }

  # add the BestMatches
  if(!is.null(BestMatches)){
    if(is.null(Cls)) Cls <- rep(1,nrow(BestMatches))

    Cls = factor(Cls)

    uniqueClasses <- sort(na.last=T,unique(Cls))

    ClsColors = c("darkgreen", ClsColors)
    names(ClsColors) = 0:(length(ClsColors)-1)

    MatrixPlot = MatrixPlot+scale_color_manual(values=ClsColors, name="Clusters")

    if(!is.null(BestMatchesShape))
      if(length(BestMatchesShape)>1)
        MatrixPlot = MatrixPlot+scale_shape_manual(values=BestMatchesShape, name="Clusters")

    # extract bestmatches without a class
    d = data.frame(y = BestMatches[,2], x = BestMatches[,3], class = Cls)

    if(YellowCircle)
      MatrixPlot <- MatrixPlot + geom_point(data=d, col = "yellow", size = rel(BmSize)*1.4,
                                            shape=BestMatchesShape)

    # elements with and without class
    if(length(BestMatchesShape)==1){
      MatrixPlot <- MatrixPlot + geom_point(aes_string(y = 'y', x='x', col='factor(class)'), data=d , size = rel(BmSize),
                                            shape=BestMatchesShape)
    }
    else{
      MatrixPlot <- MatrixPlot + geom_point(aes_string(y = 'y', x='x', col='factor(class)', shape='factor(class)'), data=d ,
                                            stroke=BmSize)
    }

    # add labels
    if(!is.null(BestMatchesLabels)){
      bestmatchLabels = data.frame(y = BestMatches[,2], x = BestMatches[,3], 'label' = BestMatchesLabels[as.numeric(rownames(BestMatches)),2])
      MatrixPlot <- MatrixPlot + ggrepel::geom_label_repel(data=bestmatchLabels,
                                            aes_string(label="label"),
                                            color=BestMatchesLabelStyle$color,
                                            fontface=BestMatchesLabelStyle$fontface,
                                            fill=BestMatchesLabelStyle$fill ,
                                            alpha=BestMatchesLabelStyle$alpha ,
                                            angle=BestMatchesLabelStyle$angle ,
                                            size=BestMatchesLabelStyle$size ,
                                            nudge_x =BestMatchesLabelStyle$nudge_x,
                                            nudge_y =BestMatchesLabelStyle$nudge_y )
    }

    # mark duplicates
    if(nrow(duplicatedBestMatches)>0){
      if(MarkDuplicatedBestMatches){
        duplicatedBestMatchesMarker = data.frame(y = duplicatedBestMatches[,2]+1, x = duplicatedBestMatches[,3])
        MatrixPlot <- MatrixPlot + geom_point(data = duplicatedBestMatchesMarker,
                                              shape=24,
                                              color="red",
                                              fill="red",
                                              size=4)
      }
    }
  }

  return(MatrixPlot + ggtitle(Title) + scale_size_area())  # return the plot
}
