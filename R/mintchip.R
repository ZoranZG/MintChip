# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Genomic build hg19 gencode gene locations
#'
#' Gencode gene locations. Includes the exons, introns and UTRs
#'
#' Code used to generate dataset:
#' install_github(mskilab/gTrack)
#' library(gTrack)
#' gt19 = track.gencode(build = 'hg19')
#' dat = gt19@data
#' dt_19 = mclapply(1:length(dat[[1]]), function(x){
#'   g = dat[[1]][[x]]
#'   gr2dt(g[g$type == 'gene'])
#' }, mc.cores = 8)
#' hg19_gene_locations = rbindlist(dt_19)
#' usethis::use_data(hg19_gene_locations)
#'
#'
#' @format ## `hg19_gene_locations`
#' A data.table with 46,096 rows and 12 columns:
#' \describe{
#'   \item{seqnames}{Chromosome location}
#'   \item{start,end}{start and end of the gene from build hg19, includes UTRs}
#'   \item{strand}{Strand of the transcribed gene}
#'   \item{width}{Width of the transcribed gene}
#'   \item{gene_name}{Gene name}
#'   \item{transcript_name}{Transcript name}
#'   \item{transcript_id}{Gencode transcript ID}
#'   \item{type}{Artifact of the original dataset used to generate this file}
#'   \item{exon_number}{Artifact of the original dataset used to generate this file}
#'   \item{type.1}{Artifact of the original dataset used to generate this file}
#'   \item{tx.ord}{Artifact of the original dataset used to generate this file}
#' }
#' @source <http://mskilab.com/gTrack/hg19//gencode.composite.collapsed.rds>
"hg19_gene_locations"


#' Sample fragments for demo
#'
#' Sample interactions obtained from Xue et al.
#'
#' @format ## `frags_demo`
#' A data.table with 143 rows and 4 columns:
#' \describe{
#'   \item{start,end}{start and end of the sample interactions}
#'   \item{thickness}{example strand thicknesses for demo interactions}
#'   \item{annotation}{example annotations for demo interactions}
#' }
#' @source Xue et al.
"frags_demo"


#' Sample features for demo
#'
#' Sample features for demo, shares some start and end locations with interactions
#'
#' @format ## `features_demo`
#' A data.table with 4 rows and 2 columns:
#' \describe{
#'   \item{start,end}{start and end of the sample features}
#' }
#' @source Xue et al.
"frags_demo"


#' Sample fragments for demo
#'
#' Sample fragments obtained from Xue et al.
#'
#' @format ## `frags_demo`
#' A data.table with 143 rows and 4 columns:
#' \describe{
#'   \item{start,end}{start and end of the sample features}
#'   \item{thickness}{example strand thicknesses for demo}
#'   \item{annotation}{example annotations for demo}
#' }
#' @source Xue et al.
"frags_demo"


#' Genomic build hg19 gencode gene locations
#'
#' Gencode gene locations. Includes the exons, introns and UTRs
#'
#' Code used to generate dataset:
#' install_github(mskilab/gTrack)
#' library(gTrack)
#' gt19 = track.gencode(build = 'hg19')
#' dat = gt19@data
#' dt_19 = mclapply(1:length(dat[[1]]), function(x){
#'   g = dat[[1]][[x]]
#'   gr2dt(g[g$type == 'gene'])
#' }, mc.cores = 8)
#' hg19_gene_locations = rbindlist(dt_19)
#' usethis::use_data(hg19_gene_locations)
#'
#'
#' @format ## `hg19_gene_locations`
#' A data.table with 46,096 rows and 12 columns:
#' \describe{
#'   \item{seqnames}{Chromosome location}
#'   \item{start,end}{start and end of the gene from build hg19, includes UTRs}
#'   \item{strand}{Strand of the transcribed gene}
#'   \item{width}{Width of the transcribed gene}
#'   \item{gene_name}{Gene name}
#'   \item{transcript_name}{Transcript name}
#'   \item{transcript_id}{Gencode transcript ID}
#'   \item{type}{Artifact of the original dataset used to generate this file}
#'   \item{exon_number}{Artifact of the original dataset used to generate this file}
#'   \item{type.1}{Artifact of the original dataset used to generate this file}
#'   \item{tx.ord}{Artifact of the original dataset used to generate this file}
#' }
#' @source <http://mskilab.com/gTrack/hg19//gencode.composite.collapsed.rds>
"hg19_gene_locations"


#' @name mintchip
#' @title mintchip, the Genomic-Inteaction Visualizer
#' @description mintchip takes in a series of paired genomic interactions and features
#' and plots them within a given genomic window using ggplot2.
#' @param interactions a data.frame or data.table that contains start
#' (default column name - 'start' and end (default column name = 'end') coordinates of
#' genomic interactions as well as any annotations you want to color by
#' (default column name - 'annotation') and a numeric column that will
#' map to interaction thickness (default column name - "thickness")
#' @param features a data.frame or data.table that contains the
#' start (default column name - 'start') and end (default column name = 'end') coordinates of
#' genomic features that you would like to plot as well as the color you would like these
#' elements to be (default column name - 'color') and any text annotations that you
#' would like to add to the features (default column name - 'name')
#' @param gene_list a chacter vector of gencode gene names that you would like to plot
#' below the interactions, and above the genomic features
#' @param genome_build either "hg19" or "hg38" and indicates which genome build
#' to pull the gene annotations from
#' @param alpha a numeric value between 0 and 1 that specifies the transparency of the
#' genomic interactions. alpha scales linearly with the number of overlaps, e.g.
#' if alpha = 0.5 and two interactions intersect, their intersection will have the
#' same transparency as one interaction at alpha = 1.
#' alpha = 1 is no transparency
#' alpha = 0 is fully transparency
#' @param height_scale a numeric (0,Inf) that sets the amount by which the higher peaks will stand out from lower peaks
#' a high height_scale (e.g. 5) will yield highs that are much higher than the lows
#' a low height_scale (e.g. 0.1) will yield a plot where most of the peaks are similar sizes
#' @param thickness_scale a numeric (0,Inf) that sets the amount by which the thicker peaks will stand out from thinner peaks
#' a high thickness_scale (e.g. 5) will yield thick lines that are much thicker than the thinner lines
#' a low thickness_scale (e.g. 0.1) will yield a plot where most of the lines are similar thickness
#' @param base_thickness linearly scales all line widths, e.g. if set to 2, all lines will be twice as thick
#' if set to 0.5, all lines will be half as thick
#' @param xmin a numeric that indicates the leftmost genomic coordinate for plotting viewing.
#' If not specified, automatically generated from the interactions, features, and gene_list.
#' @param xmax a numeric that indicates the rightmost genomic coordinate for plotting viewing.
#' If not specified, automatically generated from the interactions, features, and gene_list.
#' @param color_palette The RColorBrewer palette to use for annotating the
#' genomic interations, default is "Dark2"
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom stats median
#' @return a ggplot object that visualizes the provided genomic interactions and features
#' @author Zoran Z. Gajic
#' @export
mintchip <- function(interactions = NULL,
                     features = NULL,
                     gene_list = NULL,
                     genome_build = 'hg38',
                     alpha = 1,
                     height_scale = 1,
                     thickness_scale = 1,
                     base_thickness = 1,
                     xmin = NULL,
                     xmax = NULL,
                     color_palette = "Dark2") {

  ## Name:
  cat("\n\n")
  cat("########################################################################\n\n")
  cat("                              Welcome To\n\n")
  cat("########################################################################\n")
  cat('
888b     d888  d8b           888      .d8888b.   888       d8b
8888b   d8888  Y8P           888     d88P  Y88b  888       Y8P
88888b.d88888                888     888    888  888
888Y88888P888  888  88888b.  888888  888         88888b.   888 88888b.
888 Y888P 888  888  888 "88b 888     888         888 "88b  888 888 "88b
888  Y8P  888  888  888  888 888     888    888  888  888  888 888  888
888   "   888  888  888  888 Y88b.   Y88b  d88P  888  888  888 888 d88P
888       888  888  888  888  "Y888   "Y8888P"   888  888  888 88888P"
                                                               888
                                                               888
                                                               888\n')
  cat('########################################################################\n')

  ## interactions should be non-null
  if(is.null(interactions)){
    stop(message('"interactions" cannot be null, "interactions" must be a data.frame or data.table.'))
  }


  ## Interactions should be a data.frame or data.table
  interaction_class = class(interactions)
  if(interaction_class[1] == 'data.table'){
    cat(message('"interactions" object identified as a data.table.\n'))
  } else if(interaction_class[1] == 'data.frame'){
    cat(message('"interactions" identified as a data.frame, internally converting to data.table.\n'))
    interactions = as.data.table(interactions)
  } else{ ## Interactions is not a data.frame or data.table
    stop(message('"interactions" cannot be null, interactions must be a data.frame or data.table.\n'))
  }


  ## Interactions needs to have the columns 'start' and 'end'
  if(!all(c('start', 'end') %in% colnames(interactions))){
    stop(message("interactions must have the column names: 'start' and 'end"))
  }

  ## start column must be numeric
  if(!inherits(interactions$start, 'numeric')){
    stop(message("the 'start' column in the interactions table must be of class numeric"))
  }
  ## end column must be numeric
  if(!inherits(interactions$end, 'numeric')){
    stop(message("the 'end' column in the interactions table must be of class numeric"))
  }


  ## checking to see if there is an annotations column in the interactions table
  annotation = F
  if('annotation' %in% colnames(interactions)){
    cat(message('annotations column identified in the interactions table.\n'))
    annotation = T
  }


  ## checking to see if there is a thickness column in the interactions table
  thickness = F
  if('thickness' %in% colnames(interactions)){
    cat(message('thickness column identified in interactions table.\n'))
    thickness = T
  }


  cat(message('checking to make sure height_scale is a numeric between [0,Inf)\n'))
  ## height_scale must be a 0 or positive numeric value
  if(!inherits(height_scale, 'numeric')){
    stop(message('height_scale must be a numberic value'))
  }
  if(height_scale < 0){
    stop(message('height_scale must be greater than or equal to 0'))
  }


  cat(message('checking to make sure thickness_scale is a numeric between [0,Inf)\n'))
  ## thickness_scale must be a 0 or positive numeric value
  if(!inherits(thickness_scale, 'numeric')){
    stop(message('thickness_scale must be a numberic value'))
  }
  if(thickness_scale < 0){
    stop(message('thickness_scale must be greater than or equal to 0'))
  }


  ## alpha must be a numeric between 0 and 1
  if(!inherits(alpha, 'numeric')){
    stop(message('alpha must be a numeric value'))
  }
  if(!(alpha >= 0 & alpha <= 1)){
    stop(message('alpha must have a value between [0,1]'))
  }

  ## Generating the data.table that we'll be using for plotting.
  ## It creates three points per interaction.
  ## a start point, an end point and a middle point that we'll use to fit a parabola
  interactions$thickness = interactions$thickness/median(interactions$thickness)
  dt_plot = rbindlist(lapply(1:nrow(interactions), function(x){
    ix = interactions[x]
    start = ix$start
    end = ix$end
    dt_para = data.table(
      x = c(start,(start+end)/2,end),
      y = c(0,(end-start)^height_scale,0),
      id = x,
      alpha = alpha
    )
    if(annotation){
      dt_para$annotation = ix$annotation
    } else{
      dt_para$annotation = 'Interactions'
    }
    if(thickness){
      dt_para$thickness = base_thickness*(ix$thickness^thickness_scale)
    }
    return(dt_para)
  }))

  ## This line normalizes the y axis of the plot to always be from 0-1
  dt_plot$y = dt_plot$y/max(dt_plot$y)


  ## Setting up for features
  if(!is.null(features)){

    ## features should be a data.frame or data.table
    feature_class = class(features)
    if(feature_class[1] == 'data.table'){
      cat(message('features object identified as a data.table.\n'))
    } else if(feature_class[1] == 'data.frame'){
      cat(message('features identified as a data.frame, internally converting to data.table.\n'))
      features = as.data.table(features)
    } else{ ## features is not a data.frame or data.table
      stop(message('features can only be null, a data.frame, or data.table.\n'))
    }


    ## features needs to have the columns 'start' and 'end'
    if(!all(c('start', 'end') %in% colnames(features))){
      stop(message("features must have the column names: 'start' and 'end"))
    }

    ## start column must be numeric
    if(!inherits(features$start, 'numeric')){
      stop(message("the 'start' column in the features table must be of class numeric"))
    }
    ## end column must be numeric
    if(!inherits(features$end, 'numeric')){
      stop(message("the 'end' column in the features table must be of class numeric"))
    }
    ## check to see if there is a 'name' column, if not create an empty name column
    if(!('name' %in% colnames(features))){
      features$name = paste0('Feature: ', 1:nrow(features))
    }
    ## check to see if there is a 'color' column, if not create an empty color column
    if(!('color' %in% colnames(features))){
      features$color = 'blue'
    }


    ## Creating the feature annotations
    ## This code generates the data table that we'll be using to plot the feature
    dt_poly = rbindlist(lapply(1:nrow(features), function(x){
      poly_genes(name  = features$name[x],
                 start = features$start[x],
                 end   = features$end[x],
                 fill  = 'grey',
                 color = features$color[x],
                 yset  = -0.05,
                 height = -0.15)
    }))


  } else{
    cat(message('No features object detected\n'))
  }

  ## Gene features
  if(!is.null(gene_list)){
    if(!(genome_build %in% c('hg19', 'hg38'))){
      stop(message('Invalid build, build must be one of "hg19" or "hg38"'))
    }
    ## Valid build
    if(genome_build == 'hg19'){
      ref = MintChip::hg19_gene_locations
    } else if (genome_build == 'hg38'){
      ref = MintChip::hg38_gene_locations
    }
    ## subsetting reference
    ## making sure gene_list is a character vector
    if(!inherits(gene_list,'character')){
      stop(message('gene_list can either be NULL or of class character'))
    }
    ref_sub = ref[ref$gene_name %in% gene_list]
    if(length(ref_sub) < 1){
      warning(message('no valid genes found, make sure gene_list contains gene_names found in:
              MintChip::hg19_gene_locations if using hg19 or
              MintChip::hg38_gene_locations if using hg38'))
    } else{
      dt_poly_genes = rbindlist(lapply(1:nrow(ref_sub), function(x){
        poly_genes(name  = ref_sub$gene_name[x],
                   start = ref_sub$start[x],
                   end   = ref_sub$end[x],
                   fill  = 'grey',
                   color = 'blue',
                   yset  = -0.25,
                   height = -0.35)
        }))

    }

  } else{
    dt_poly_genes = NULL
  }


  ## Defining min and max boundaries
  if(is.null(xmin)){
    xmin = min(dt_plot$x)
    if(!is.null(features)){
      xmin = min(xmin, min(dt_poly$x))
    }
    if(!is.null(gene_list)){
      xmin = min(xmin, min(dt_poly_genes$x))
    }
  }
  if(is.null(xmax)){
    xmax = max(dt_plot$x)
    if(!is.null(features)){
      xmax = max(xmax, max(dt_poly$x))
    }
    if(!is.null(gene_list)){
      xmax = max(xmax, max(dt_poly_genes$x))
    }
  }

  ## This is the plotting code:

  ## First we'll setup a color pallete for plotting
  ## the replication is in case we have over 8 colors
  ## in which case we'll have to repeat colors, this is a noted limitation
  pal = rep(brewer.pal(n=8,color_palette), 1000)
  ## Find the number of colors we'll need
  uni_fill = unique(dt_plot$annotation)
  ## create a data.table that maps features to colors
  fill_colors = data.table(uni_fill, color = pal[1:length(uni_fill)])
  ## making a vector for the legend colors
  legend_colors = fill_colors$color
  names(legend_colors) = uni_fill
  ## This is the base plot call, it sets the following:
  ## plot theme (theme_bw())
  ## the color scheme based on the legend_colors (scale_color_manual)
  ## a legend annotation (annotate) which puts colored text
  ## where the text is the category and the color maps to the plot color
  ## Sets the xlab to chr9 genomic position (feel free to change this)
  p = ggplot(dt_plot, aes(x = x, y = y)) +
    theme_classic() +
    scale_color_manual(values = legend_colors) +
    xlab('Genomic Position') + ylab('') +
    annotate(geom = 'text', x = xmin+0.05*(xmax-xmin),
             y = (1.1-c(1:length(legend_colors))/10),
             label = names(legend_colors),
             col = legend_colors) +
    theme(legend.position = "none") +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    scale_x_continuous(limits = c(xmin, xmax))
  ## This is the code the creates the parabolas for plotting
  ## note we do a for loop and iteratively add each parabola
  ## calculating rough number of elements for text output
  nci = nchar(nrow(interactions))
  for (interaction in dt_plot[!duplicated(dt_plot$id)]$id){
    curve = dt_plot[id == interaction]
    to_print = (interaction %% (10^(nci-2)) == 0) | (interaction == 1) | (interaction == nrow(interactions))
    if(to_print){
      cat(message(paste('Fitting interaction: ', interaction, ' / ', nrow(interactions), '\n')))
    }
    p = p +
      geom_line(data = curve,stat="smooth",method = "lm",
                color = fill_colors[uni_fill == curve$annotation[1]]$color,
                formula = y ~ poly(x, 2), span = 200,
                se = FALSE,lineend="round", alpha = curve$alpha[1], size=curve$thickness[1],
      )
  }
  ## This is the code that creates the gene boxes and names for plotting
  ## note we do a for loop and iteratively add each genebox and name
  if(!is.null(features)){
    for (feature in dt_poly[!duplicated(dt_poly$name)]$name){
      cat(message(paste('Plotting Feature: ', feature, '\n')))
      feature = dt_poly[name == feature]
      p = p + geom_polygon(data = feature,mapping=aes(x = x, y = y)) +
        annotate(geom="text", x=sum(feature$x)/4, y=-0.2, label=feature$name[1], col = feature$color[1], size = 2)
    }
  }
  ## Check to see if we have any gene annotations, if we do, add them here
  if(!is.null(dt_poly_genes)){
    if(is.null(features)){
      dt_poly_genes$y = dt_poly_genes$y+0.2
    }
    if(nrow(dt_poly_genes) > 0){
      for (gene in dt_poly_genes[!duplicated(dt_poly_genes$name)]$name){
        cat(message(paste('Plotting Gene: ', gene, '\n')))
        gene = dt_poly_genes[name == gene]
        p = p + geom_polygon(data = gene,mapping=aes(x = x, y = y)) +
          annotate(geom="text", x=sum(gene$x)/4, y=min(gene$y -0.05), label=gene$name[1], col = gene$color[1], size = 2)
      }
    }
  }


  return(p)

}

message = function(text){
  return(paste0('MintChip ', Sys.time(), ': ',text))
}

## converts the genes into boxes for plottingv
poly_genes = function(name, start, end, fill, color, height = -0.1, yset = 0){
  poly = data.table(x = c(start,end,end,start),
                    y = c(yset,yset,height,height),
                    fill = fill, color = color, name = name)
  return(poly)
}
