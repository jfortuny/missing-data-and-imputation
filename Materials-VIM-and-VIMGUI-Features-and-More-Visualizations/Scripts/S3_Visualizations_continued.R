#########################################
###   VIM and VIMGUI Visualizations   ###
#########################################

# Load VIM library
library("VIM")

### Marginplot matrix 

# This function creates a scatterplot matrix, 
# with a panel function that is based on the
# previous marginplot example.

# So it is possible to get an overview about
# the distribution of multiple parameters at once.

# Select the variables of interest:
vars <- c("Air.Temp","Humidity",
          "Sea.Surface.Temp",
          "Air.Temp_imp","Humidity_imp",
          "Sea.Surface.Temp_imp")

# Create imputed tao data:
tao_kNN <- kNN(tao, k = 5)

# Draw the marginplot matrix of Air.Temp,
# Humidity, and Sea.Surface.Temp
marginmatrix(tao_kNN[,vars], 
             delimiter = "_imp", 
             alpha=0.6)

# Ones sees the different marginplots. 

# Interestingly, it turns out that the 
# variable Sea.Surface.Temp is also clearly
# separated in two clusters. Values, which 
# are imputed values in the variable Humidity 
# almost only appear in the first cluster, 
# which indicates low values of Sea.Surface.Temp. 

# Whereas imputed values in the variable 
# Air.Temp almost only belong to the right 
# cluster, which marks high values in 
# Sea.Surface.Temp. 

# The highlighted values of Air.Temp in the
# upper right (or lower left) panel reveal 
# that the imputation process wasn't as good
# as it was previously assumed when just
# examining the singular marginplot of same.

### Scatterplot Matrix with Imputed Missings

# Similar to the previous marginplot matrix
# method, this function also creates a 
# scatterplot matrix, but with a panel 
# function that is based on the previous
# scatterplot method. 

# It is also intended to get an overview
# about the distribution of multiple
# parameters at a single view. 

# In contrast to the marginplot matrix
# method, certain variables can be chosen
# to be used for the highlighting of
# imputed values. In the diagonal panels
# of the plot, density plots for each 
# variable are drawn. 

# Furthermore, these plots are split 
# according to the observed and the 
# imputed values of the variables which
# have been selected for highlighting.

# Select variables of interest, Air.Temp
# is used for highlighting. Being able
# to choose which variable is used for
# highlighting is very convenient.
vars <- c("Air.Temp","Humidity", 
          "Sea.Surface.Temp",
          "Air.Temp_imp",
          "Humidity_imp",
          "Sea.Surface.Temp_imp")

# Draw the scatterplot matrix, only
# Air.Temp is used for highlighting. It
# confirms conclusions from previous
# graphical methods.
scattmatrixMiss(tao_kNN[,vars], 
                # highlight can be a vector of variable names
                highlight="Air.Temp", 
                delimiter = "_imp", 
                # alpha is level of transparency
                alpha=0.6)

# The diagonal panels are density plots.
# Blue line is the density of the values
# of the particular variable of the panel,
# which are observed in the variables,
# that are selected to highlighting.

# The orange colored lines represent the
# density of the values, which are the
# imputed values in these variables.

# Parameter plotvars (below) is a vector
# of names that should be used for plotting.

# By combining highlight and plotvars, it 
# is possible to use certain variables just
# for highlighting and omit the separate 
# scatterplot of this variable (below).

vars <- c("Air.Temp","UWind",
          "VWind","Air.Temp_imp")

# Variable UWind and VWind are plotted and
# values variable Air.Temp are used for
# highlighting only....shows that plotted
# variables have no evident influence on
# imputed values in Air.Temp. Also, one
# can see the both variables are not
# correlated:
scattmatrixMiss(tao_kNN[,vars], 
                highlight="Air.Temp", 
                plotvars=c("UWind","VWind"), 
                delimiter = "_imp", 
                alpha=0.6)

### Parallel Coordinate Plot with Imputed Missings

# the parallel coordinate plot is also an ad-
# justment of the standard method to support 
# the highlighting of imputed values. The variables
# are represented by parallel axes. The data is 
# scaled and each observation is displayed as a
# continuous line, indicating the value it's 
# having in each of the variables. 

# Imputed values of certain variable can be
# highlighted. Thereby, the whole line is 
# emphasized to view the values, the particular 
# observation is having along the variables.

# Is useful to detect multivariate dependencies,
# or structures, because of which the imputed
# values have been missing, since it can show
# multiple parameters in one graphic.

# Impute chorizonDl data
chorizon_kNN <- kNN(chorizonDL[,c(15,101:110)], 
                    k=5)

# Shows that except in two outliers, the
# imputed values of Bi are having accumulation
# points throughout most of the variables
parcoordMiss(chorizon_kNN, 
             delimiter = "_imp" , 
             plotvars=2:11)

# Especially with the variables AI_XRF, P_XRF,
# Si_XRF and Ti_XRF, these variables are having
# an influence on the structure of the imputed
# values of Bi.

### Matrix Plot

# Helps to detect multivariate dependencies
# and patterns, but can also find outliers
# in the data set. Each cell of matrix is
# visualized by a rectangle. Observed values
# are colored according to a gray scale,
# whereas missing or imputed values are
# highlighted by a clearly distinguishable
# color.

# To determine the gray level of observed
# values, the variables are scaled to the 
# interval [0,1], small values are colored 
# in light gray and high values with dark 
# grey. 

# Supplementary, the data matrix can be
# sorted by the magnitude of a particular
# variable. The currently selected variable
# is then printed out in the R console.

# Can combine variables with missing (red,
# aren't any here) and variables with
# imputed values shown in orange. This is
# a matrix plot of the entire data set
# sleep which is imputed entirely. The
# data matrix is sorted on the variable
# Span.
# Create imputed sleep data:
sleep_kNN <- kNN(sleep, k = 5)
matrixplot(sleep_kNN, 
           delimiter="_imp", 
           sortby="Span")

# By looking at the plot, can see that 
# there is one clear outlier in variables
# BodyWgt, BrainWgt and Span. Which can be
# seen on the fact, that there is almost
# no color gradient in these variables, instead,
# there are many white and light gray
# colored rectangles and one black rectangle. 

# Since the graphic is sorted by the 
# values of the variable Span, it reveals, 
# that there are accumulation points of 
# imputed values in NonD and Dream in 
# a certain value interval of Span.

# Mosaic Plot with Imputed Missings

# The mosaic plot is a graphical repre-
# sentation of multi-way contingency
# tables, therefore it is mainly 
# intended for categorial variables. 

# The frequencies of the different cells
# are visualized by area-proportional 
# rectangles (tiles). 

# For constructing a mosaic plot, a 
# rectangle is first split vertically 
# at positions corresponding to the relative
# frequencies of the categories of a 
# corresponding variable. 

# Then the resulting smaller rectangles
# are again subdivided according to the 
# conditional probabilities of a second
# variable. This can be continued for 
# further variables accordingly. 

# Additionally, imputed values in certain
# variables are highlighted in order
# to explore their structure.

# Here is example of mosaic plot of Pred
# and Exp of data set sleep. Imputed values
# of NonD are highlighted. Shows that most
# of the values occur if the values of
# the variables are both in the category
# 1 or 5

# Also, many values in these categories
# are imputed values in the variable NonD.
# This also applies to values, which are
# in category 3 in the variable Pred and 5
# in Exp.

# The values, which are of category 1 in Pred
# and 4 in Exp are all imputed values in NonD.

# Category 1 is dominant in Exp, and same
# can be said about category 2 of Pred.
mosaicMiss(sleep_kNN, highlight=3, 
           plotvars=8:10, 
           delimiter="_imp", 
           miss.labels=FALSE)