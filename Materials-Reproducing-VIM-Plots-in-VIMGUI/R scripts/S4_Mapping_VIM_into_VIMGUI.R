#########################################
###   VIM and VIMGUI Visualizations   ###
#########################################

# Load VIM library
library("VIM")

# Get allstat up and running:
# Point to the file 'allstatGUI.R'
# and source() it in
source(file.choose())
# Then start it up
allstatGUI()

# Today we look at (with imputed values):

# Aggregation Plot
# Histogram and Barplot
# Spinogram and SpinePlot
# Boxplot and Parallel Boxplot
# Marginplot

# The default color-scheme for all graphical
# methods is blue for observed values, red for
# missing values and orange for imputed values.

# First we create them using the R command line.
# And then we recreate them using VIMGUI. In
# all cases we impute using kNN imputation.

?kNN

# Subsequently, the R-commands for the used 
# imputation methods are given:
sleep_kNN <- kNN(sleep, k = 5)
tao_kNN <- kNN(tao, k = 5)
chorizon_kNN  <- kNN(chorizonDL,
                     variable=c("Ca","As","Bi"), 
                     dist_var=c("Hg","K","La","Li"), 
                     k=5)

# Aggregate Missings and Imputed Missings

# We get overview of data set: number of missing
# values, number of missing that have been imputed

# See if there are certain combinations of
# variables with missing or imputed values.

# Data set sleep, we impute variables Sleep,
# Dream, and NonD
sleep_kNN_part <- kNN(sleep, 
                      variable=c("Sleep","Dream","NonD"), 
                      dist_var=colnames(sleep), k=5)

# Create the aggregate plot.
# Imputed values are orange.
?aggr
aggr(sleep_kNN_part, 
     delimiter="_imp", 
     numbers=TRUE, 
     prop=c(TRUE,FALSE))

# Variables span and gest contain missing
# values, but haven't been imputed yet.

### Left plot region ###
# Is a barplot with proportion of missing
# or imputed values in each variable.

# Variables NonD and Dream have highest
# proportion (and amount) of missing values.

### Right plot region ###
# Is an aggregation plot, showing all
# existing combinations of missing (red), 
# imputed (orange), and observed (blue).

# Also, the frequencies of different
# combinations are visualized by a small
# barplot and by the number of their
# occurrence on the right side.

# If values in the variable NonD are missing,
# then they are mostly also missing in the
# variable Dream, except for two times.

# Variable Sleep only has missing which
# are also missing in NonD.

### Customizing the Graphic ###
# Here is some code for customized
# aggregation plots with some of the
# adjustable parameters:

# Here only.miss=FALSE
aggr(sleep_kNN_part, delimiter="_imp", 
     sortVars=TRUE, numbers=TRUE, 
     prop=c(FALSE, FALSE), 
     only.miss=FALSE)

# Here only.miss=TRUE
aggr(sleep_kNN_part, delimiter="_imp", 
     sortVars=TRUE, numbers=TRUE, 
     prop=c(FALSE, FALSE), 
     only.miss=TRUE)

# combines the left and right side
aggr(sleep_kNN_part, delimiter="_imp", 
     combined=TRUE, sortVars=FALSE, 
     sortCombs=TRUE, numbers=TRUE, 
     prop=c(FALSE, FALSE), only.miss=TRUE)

### sortVars ###
# When sortVars is set to TRUE, the variables
# in the left barplot are sorted according
# to their number of missing and imputed values.

### sortCombs ###
# sortCombs is like sortVars but for the right
# side of the aggregation plot. If TRUE,
# combinations are sorted by the frequency
# of their occurrence

### only.miss ###
# If TRUE, barplot on right side is only
# drawn for combination including missing or
# imputed values. Is helpful if most obs
# are complete anyway

### combined ###
# Combines the two plot regions of the
# aggregation graphic.

##### Histogram and Barplot

# Both methods are adaptations of the familiar
# graphics. If more than one variable is
# supplied, the bins for the variable of 
# interest will be split according to the 
# imputed values in the additional variables. 

# Imputed values in the variable of interest are
# visualized with a barplot on the right side of 
# the plot, which is separated by a small gap.

# This bar is also split if more than one variable
# is supplied.

# A histogram is produced if the variable of 
# interest is of type numerical. Otherwise, 
# if it's of type categorial, a barplot is drawn.

# Both graphics are intended to visually analyze
# the distribution and the proportion of observed
# and imputed values of the variable of interest,
# to discover outliers and to detect possible bi- 
# or multivariate dependencies.

# select variables in tao
tao_vars <- c("Air.Temp",
              "Humidity",
              # these are imputed:
              "Air.Temp_imp", 
              "Humidity_imp")

# Histogram of the variable Air.Temp 
# of the data set tao, imputed values
# in Humidity are highlighted
histMiss(tao_kNN[,tao_vars], 
         delimiter="imp", 
         selection="any")

# Bars colored in orange represent imputed
# values (in Humidity) in the additional 
# variable of interest (Air.Temp). Values
# which have been imputed in Humidity have
# low values in the variable Air.Temp. This
# might indicate that missing values in
# Humidity are not MCAR.

### Barplot on far right
# On the right side of each plot a barplot
# is drawn, visualizing the imputed values
# in the variable of interest. The bar, which
# is colored dark blue, indicates imputed
# values in the variable of interest. 

# The dark orange colored bar represents
# values which are imputed values in the 
# variable of interest and also in the 
# additional variables.

# The right bar plot shows that the amount
# of imputed values in both variables, 
# Air.Temp and Humidity, is rather small.

# select variables in sleep
sleep_vars <- c("Exp","Dream",
                "NonD","Dream_imp",
                "NonD_imp")

# Barplot of the variable Exp of the
# data set sleep, imputed values in 
# Dream or NonD are highlighted
barMiss(sleep_kNN[,sleep_vars], 
        delimiter="_imp", 
        selection="any")

# Above figure shows there are no imputed
# values in any of the other variables,
# when the category of Exp is 3. If it's 5,
# the proportion of imputed values in the
# others increases to almost 50%. 

# Note also the category 1 is very
# dominant in this variable.

### Barplot on far right
# On the right side of each plot a barplot
# is drawn, visualizing the imputed values
# in the variable of interest. The bar, which
# is colored dark blue, indicates imputed
# values in the variable of interest. 

# The fact that no barplot is shown on far
# right is because there are no imputed
# values in the variable Exp at all. (But
# there are imputed values in Dream or NonD).

### Customizing the graphic

# Customized histogram plots:

# Set "Dream" as variable of interest,
# NonD and Sleep used for highlighting:
vars <- c("Dream","NonD","Sleep","Dream_imp",
          "NonD_imp","Sleep_imp")

# selection value allows choice to highlight
# imputed values in any or all of the additional 
# variables. Is like aggregation plot.

# selection = "any"..all values which are
# imputed in NonD or Sleep are highlighted.
histMiss(sleep_kNN[,vars], delimiter="imp", 
         selection="any")

# By changing it to "all" only the 4 values
# which are imputed in both variables are shown.

# Highlighting of imputed values in the additional
# variables in based on selection = any or all.
histMiss(sleep_kNN[,vars], delimiter="imp", 
         selection="all")

# By setting only.miss = FALSE, a barplot
# with two bars, instead of just one, is 
# printed on the right side of the plot. 
# Both are split in two parts again. 
# This barplot is not on the same scale 
# as the main plot anymore, hence an 
# additional y-axis is printed.
histMiss(sleep_kNN[,vars], delimiter="imp", 
         selection="all", only.miss=FALSE)

# First bar on right shows observed values in
# Dream. Orange part marks values in Dream which
# are imputed in the additional variables (NonD
# and Sleep).

# Second bar on right shows imputed values in
# variable of interest (Dream). Dark orange part
# describes values imputed in both Dream and in
# NonD and Sleep.

# Highlighting of imputed values in the additional
# variables in based on selection = any or all.

### Spinogram and spineplot with imputed missings

# Depends on the type of variable of interest, 
# if is numerical you get a spinogram, if is
# categorical a spineplot is created.

# Spinogram is an alternative to histogram.
# Instead of the vertical axis, the horizontal 
# axis is scaled according to relative frequencies
# of the categories/classes. 

# The vertical axis is rather scaled to a
# height of 1.

# The spineplot is the analogue to the barplot.
# If more than one variable is supplied, the 
# bins are split based on the values imputed
# in the additional variables. 

# The proportion of highlighted observations
# in each category/class is displayed on the 
# vertical axis.

# Height of each cell corresponds to the 
# proportion of highlighted observations
# so one can compare the proportions of 
# imputed values among the different categories/classes.

# Significant differences in these proportions
# indicate a missing at random situation.

# Air.Temp (numerical) variable of interest,
# Humidity is the other variable used
# for highlighting. Is a spinogram.
tao_vars <- c("Air.Temp","Humidity",
              "Air.Temp_imp","Humidity_imp")

# imputed values in the variable of interest
# are visualized with a spineplot on the right,
# which is split if is more than one variable.
spineMiss(tao_kNN[,tao_vars], 
          delimiter="imp", selection="any")

# Orange bars represent values, which are imputed
# in the additional variables and the values 
# they're having in the variable of interest.

# Instead of the height, in the spinogram the
# width of the bins represent the relative
# frequency of the categories/classes.

# Unlike a histogram, here the proportion 
# of imputed values can be compared among
# the different categories/classes.

# On right, dark blue bar indicates imputed
# values in the variable of interest. 
# Dark orange colored bar shows values
# imputed in the variable of interest and
# also in the additional variables.

# Exp is (categorical) variable of interest,
# Dream, NonD and Sleep are other variables
# used for highlighting. Is a spineplot.
sleep_vars <- c("Exp","Dream",
                "NonD","Sleep",
                "Dream_imp","NonD_imp",
                "Sleep_imp")

# Width of bins show relative frequencies.
# Can again see dominance of category 1.
spineMiss(sleep_kNN[,sleep_vars], 
          delimiter="_imp")

# Right side shows are no imputed values in Exp.

### Customizing the Spinogram and Spineplot

# Customizable features same as histogram example.

# Dream is variable of interest, NonD and
# Sleep are the 'other variables.'
vars <- c("Dream","NonD","Sleep","Dream_imp",
          "NonD_imp","Sleep_imp")

# Get a spinogram with only.miss = FALSE and
# selection = all
spineMiss(sleep_kNN[,vars], delimiter="imp", 
          selection="all", only.miss=FALSE)

### Boxplot with Imputed Missings

# The plot consists of three boxplots: A
# normal boxplot of the variable of interest;
# two boxplots grouped by observed and
# imputed values in the additional variables
# and the values of them in the variable of
# interest. Also, the frequencies of
# observed and imputed values are given
# for each boxplot.

# It gives a good overview about the
# distribution of the variable of interest. 
# Also, outliers can be identified easily.
# Also, by grouping between observed and 
# imputed values, one can examine whether
# the distributions differ.

# Dream is variable of interest:
vars <- c("Dream","NonD","Sleep",
          "Dream_imp","NonD_imp",
          "Sleep_imp")

# Boxplot of the variable Dream of
# the sleep data set, grouped by 
# imputed values in NonD or Sleep:
pbox(sleep_kNN[,vars], 
     delimiter="_imp", 
     selection="any")

### Left Boxplot ###
# A normal boxplot of the variable Dream.
# Three values of the variable Dream are
# obviously outliers.

### Right two Boxplots ###
# First boxplot has values observed in the
# additional variables NonD and Sleep
# and the values of them in Dream.

# The second one marks imputed values in
# NonD and Sleep, and the values they're
# having in the variables of interest.

# One sees that the three outliers of the
# Dream are observed values in the NonD and
# Sleep. This excludes the possibility
# that the outliers in Dreams have been
# causing the missing values in NonD
# and Sleep. Also the distribution of 
# observed and imputed values can be
# compared, which apparently differs.

### Frequencies ###
# At the bottom the frequencies of 
# observed and imputed values is
# printed. First line corresponds to 
# observed values in the variable of 
# interest and their distribution in
# the two groups and the second line
# to the imputed values.

# Dream has 62 values total, whereas
# 12 values of them are imputed. 
# 48 values are observed in NonD and
# Sleep. Total, there are 14 values
# which are imputed in NonD or Sleep
# and 12 of them are imputed in Dream.
# So 2 values are only imputed in
# NonD or Sleep.

### Customizing the Graphic ###

# same variable set
vars <- c("Dream","NonD","Sleep",
          "Dream_imp","NonD_imp",
          "Sleep_imp")

# the same variables are used, but
# the selection = all. Four values
# are imputed in NonD and Sleep.
# Two are imputed in all variables.
pbox(sleep_kNN[,vars], 
     delimiter="_imp", 
     selection="all")

# Also, the labels of the grouped 
# boxplots have changed.

# If numbers = FALSE, the output of
# the frequencies of observed and 
# imputed values is excluded.

### Parallel Boxplots ###
# Is similar to boxplot function.
# The only difference is that the
# additional variables are not
# combined.

# Instead, boxplots for each additional
# variable are created, while the values
# of the variable of interest are 
# grouped according to observed and
# imputed values of each particular
# variable.

# VARIABLES THAT DO NOT CONTAIN
# IMPUTED VALUES ARE EXCLUDED FROM
# THE PLOT. So the plot is useful
# to explore whether one continuous
# variable explains the distribution
# of missing values in any other
# variable.

vars <- c("Dream","NonD",
          "Sleep","Dream_imp",
          "NonD_imp","Sleep_imp")

# Note that changing from the normal
# boxplot to the parallel boxplot is
# just a matter of changing the
# selection parameter to "none".

# Parallel Boxplot of the variable Dream 
# of the sleep data set, imputed values in
# NonD and Sleep are highlighted separately
pbox(sleep_kNN[,vars], 
     delimiter="_imp", 
     selection="none")

# Left boxplot is same as previous.

# Boxplot for each additional variable:
# contains imputed values. They are 
# grouped according to the observed
# and imputed values in each particular
# variable....the values they have in
# the variable of interest are used to
# create each boxplot.

# In the graphic above, one can see
# the grouped boxplots for the variables 
# NonD and Sleep. The distribution of the
# imputed values hasn't changed significantly.

# In the bottom of the plot region, the 
# frequencies of observed and imputed values
# is printed again. The first line corresponds
# to observed values in the variable of interest
# and their distribution in the different groups 
# and the second line to the accordant imputed values.

# Reading the second line shows, that all imputed
# values of Dream are also imputed in the variable
# NonD. In contrast, there are only two values, 
# which are imputed in Sleep and Dream.

### Marginplot

# The marginplot is an enhancement to the normal 
# scatterplot, here imputed values are highlighted
# for each variable. In addition to the scatterplot, 
# boxplots for available and for imputed values, as 
# well as univariate scatterplots for the imputed values
# are given in the plot margins. 

# Furthermore the frequencies of imputed values are
# displayed, again for each variable.

vars <- c("Air.Temp","Humidity",
          "Air.Temp_imp",
          "Humidity_imp")

# In bivariate scatterplot, the values highlighted with
# orange color are values of Air.Temp, 
# which are imputed values in Humidity.

# The dark orange colored values mark values of
# Humidity which are imputed in Air.Temp. 

# The black are values imputed in both.
marginplot(tao_kNN[,vars], 
           delimiter="imp", 
           alpha=0.6)

# We see that the variable Air.Temp is separated 
# into two clusters. Further we see that imputed
# values of Humidity only occur in the first 
# cluster, and are values with low Air.Temp 
# and high Humidity and that they are imputed
# with a value between 84 and 91. 

# On the other hand, most of the imputed values
# of Air.Temp have been imputed with a value between
# 26 and 27 and mostly occur in the second cluster, 
# which are value with high Air.Temp.

# Univariate boxplots are grouped according
# to observed and imputed values in the other
# variable and the values they're having in
# current one.

# Frequencies
# In the bottom left corner the frequencies 
# of imputed values are shown and colored.

# Variable Air.Temp contains 81 imputed values
# and are 93 imputed values in the Humidity. 

# Also 3 values are imputed in both variables.

# Comparisons of Imputation Methods

# We use the marginplot to compare the outcome
# of different imputation methods. 

# In the first plot below the variables are
# imputed with the k-Nearest Neighbor method, 
# which is one of the built-in imputation 
# methods of the package VIM. 
library(e1071)

# In the second plot, the variables are 
# imputed with the mean method of package e1071.

tao_kNN <- kNN(tao, k = 5)
tao_mean <- as.data.frame(impute(tao, what = "mean"))
tao_mean <- cbind(tao_mean, tao_kNN[,9:11])

vars <- c("Air.Temp","Humidity",
          "Air.Temp_imp","Humidity_imp")

# This is as we did it before
marginplot(tao_kNN[,vars], 
           delimiter="imp",
           alpha=0.6, main="kNN")

# This one just imputes with the mean so all
# of the missings of each variable are imputed
# with the same value, the mean of that variable.

# Can see that the data distribution is completely
# destroyed.....especially in Air.Temp where all
# values are imputed in between the two clusters.
marginplot(tao_mean[,vars], 
           delimiter="imp",
           alpha=0.6, main="mean")
