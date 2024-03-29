MintChip
=======

A tool for visualizing genomic interactions.


Installation
------------

1. Install R-3.5 or higher

2. Install devtools

```{r}
install.packages('devtools')
```
3. Install MintChip and dependent packages

```{r}
devtools::install_github('ZoranZG/MintChip')
```

MintChip parameters 
------------------

The main utility in MintChip comes from the `mintchip` function which visualizes 
genomic interactions from experiments such as HiChip. MintChip is designed for visualizing interactions on single chromosomes. The mintchip function takes several arguments:

#### interactions: 
a data.frame or data.table that contains start (default column name - 'start)' and end (default column name = 'end') coordinates of genomic interactions as well as any annotations you want to color by (default column name - 'annotation') and a numeric column that will map to interaction thickness (default column name - "thickness")

#### features:
a data.frame or data.table that contains the start (default column name - 'start') and end (default column name = 'end') coordinates of genomic features that you would like to plot as well as the color you would like these elements to be (default column name - 'color') and any text annotations that you would like to add to the features (default column name - 'name')

#### gene_list:
a character vector of gene names that you would like to plot, can pick between hg19 and hg38 using the `genome_build` parameter.

#### genome_build:
either "hg19" or "hg38" and indicates which genome build to pull the gene annotations from.

#### alpha:
a numeric value between 0 and 1 that specifies the transparency of the genomic interactions. alpha scales linearly with the number of overlaps, e.g. if alpha = 0.5 and two interactions intersect, their intersection will have the same transparency as one interaction at alpha = 1. alpha = 1 is no transparency. alpha = 0 is fully transparency.

#### curvature:
a numeric (-Inf,Inf) A numeric value giving the amount of curvature. Negative values produce left-hand curves, positive values produce right-hand curves, and zero produces a straight line. Taken from geom_curves documentation.

#### thickness_scale:
a numeric (0,Inf) that sets the amount by which the thicker peaks will stand out from thinner peaks. A high thickness_scale (e.g. 5) will yield thick lines that are much thicker than the thinner lines. A low thickness_scale (e.g. 0.1) will yield a plot where most of the lines are similar thickness.

#### base_thickness:
linearly scales all line widths, e.g. if set to 2, all lines will be twice as thick, if set to 0.5, all lines will be half as thick.

#### xmin:
a numeric that indicates the leftmost genomic coordinate for plotting viewing. If not specified, automatically generated from the interactions, features, and gene_list.

#### xmax:
a numeric that indicates the rightmost genomic coordinate for plotting viewing. If not specified, automatically generated from the interactions, features, and gene_list.

#### color_pallete:
The RColorBrewer palette to use for annotating the genomic interactions, default is "Dark2"


Running MintChip
------------------

Basic interaction visualization
```{r}
mintchip(interactions = MintChip::frags_demo)
```

<img src="images/Tutorial_Image_1.svg" alt="some text"  width="1000" height="250">


Visualizing interactions and user defined features
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo)
         
```
<img src="images/Tutorial_Image_2.svg" alt="some text"  width="1000" height="250">

Visualizing interactions and genes
note:
genome_build = "hg19" uses hg19 coordinates
genome_build = "hg38" uses hg38 coordinates
```{r}
mintchip(interactions = MintChip::frags_demo,
         gene_list = c('CD274', 'JAK2'))   
```

<img src="images/Tutorial_Image_3.svg" alt="some text"  width="1000" height="250">


Visualizing interactions, feature and genes
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'))
```
<img src="images/Tutorial_Image_4.svg" alt="some text"  width="1000" height="250">


Adding in an alpha channel
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5)   
```
<img src="images/Tutorial_Image_5.svg" alt="some text"  width="1000" height="250">

Changing peak curvature
Curvature of <0 makes curves above the line y = 0 line.
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25)
```
<img src="images/Tutorial_Image_6.svg" alt="some text"  width="1000" height="250">

Curvature of >0 makes curves below the line y = 0 line.
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = 0.25) 
```
<img src="images/Tutorial_Image_7.svg" alt="some text"  width="1000" height="250">

Increasing the magnitude of curvature will make more 'curved' interactions while decreasing the magnitude of curvature will make interactions flatter. curvature of 0 is a straight line. See geom_curve documentation for additional info.

```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -1) 
```
<img src="images/Tutorial_Image_6.svg" alt="some text"  width="1000" height="250">



Changing thickness
Thickness is defined by the 'thickness' column in the interactions data
base_thickness >1 increases size of all interactions uniformly
Basic interaction visualization
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 5)
```
<img src="images/Tutorial_Image_9.svg" alt="some text"  width="1000" height="250">


base_thickness <1 decreases size of all interactions uniformly
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 0.2)
```
<img src="images/Tutorial_Image_10.svg" alt="some text"  width="1000" height="250">

thickness_scale is similar to height scale except it works on thickness
e.g. thickness_scale > 1 increases thickness exponentially (e.g. thickness^2)
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 1,
         thickness_scale = 2)
```
<img src="images/Tutorial_Image_11.svg" alt="some text"  width="1000" height="250">

e.g. thickness_scale < 1 decreases thickness exponentially 
and makes thickness more uniform (e.g. thickness^0 = 1)
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 1,
         thickness_scale = 0)
```
<img src="images/Tutorial_Image_12.svg" alt="some text"  width="1000" height="250">


xmin and xmax can be used to change the plot view
zooming out
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 1,
         thickness_scale = 1,
         xmin = 4000000,
         xmax = 7000000)
```
<img src="images/Tutorial_Image_13.svg" alt="some text"  width="1000" height="250">

zooming in
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 1,
         thickness_scale = 1,
         xmin = 5100000,
         xmax = 5500000)
```
<img src="images/Tutorial_Image_14.svg" alt="some text"  width="1000" height="250">

You can also change the color palette
```{r}
mintchip(interactions = MintChip::frags_demo,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5,
         curvature = -0.25,
         base_thickness = 1,
         thickness_scale = 1,
         xmin = 5100000,
         xmax = 5500000,
         color_palette = 'Set1')
```
<img src="images/Tutorial_Image_15.svg" alt="some text"  width="1000" height="250">


You can also plot interactions on the bottom side of the plot by adding in a "strand"
column to you interactions tables. The strand column is a character column that can 
only have "+" and "-". "+" tells the program to plot an interaction on the top side 
and "-" tells the program to plot the interation on the bottom side. 
Be careful as this does stack with curvature, so a positive curvature will
invert the plot putting "+" on bottom and "-" on top.

```{r}
mint_flip = MintChip::frags_demo
mint_flip$strand = '-'
mint_flip$strand[c(128,26,138,69,27,48,2,102,101,13)] = "+"
mintchip(interactions = mint_flip,
         features = MintChip::features_demo,
         gene_list = c('CD274', 'JAK2'),
         alpha = 0.5)
```
<img src="images/Tutorial_Image_16.svg" alt="some text"  width="1000" height="250">


