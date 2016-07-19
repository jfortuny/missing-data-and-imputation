#########################################
###  Dealing with Missing Data in R   ###
#########################################

# Missing data are coded as NA and no difference
# is made between numeric and string data.

# NA causes no hiccups, can be treated as
# any other value:
v <- c(10, 20, NA, 30, 40)

# NA is one of the few non-numbers that can
# be inserted in vector v withou generating
# an error.

# Note that NA is a missing value to R, but
# "NA" is a string value to R.

anum <- c(10, 20, NA, 30, 40)
astr <- c("Austria", "Australia", NA, NA, "Germany", "NA")
anum
astr

# Cannot use comparison to find NAs
# This does not return FALSE as expected:
anum[1] == NA # is not a logical expression, returns NA

# To find missing values, can use function is.na()
is.na(anum)
is.na(astr)

# To find indexes of missing values, can use which()
# with is.na()
which(is.na(anum))
which(is.na(astr))

# To assign NA value to an element of a vector one
# could either use the assignment operator or the 
# function is.na():
x <- c(1, 4, 7, 10)
x
x[4] <- NA
x
is.na(x) <- 1 # assigns NA to index [1]
x

# NA and NULL are not equivalent
# NA represents an element that is "not available"
# NULL represents something that never existed and
# cannot exist at all.

# When a vector is used to create a factor
# by default, the missing value NA will
# be excluded from factor levels.
factor(anum)

# To create a factor that includes missing
# values from a numeric variable, use
# exclude=NULL. Also, the table() function
# will not create a factor level for NA which
# could be achieved by exclude=NULL.
factor(anum, exclude=NULL)
table(anum)
table(anum, exclude=NULL)

# Most of the summary functions in R can
# optionally exclude the missing values
# from calculations by using an argument
# that specifies how the missing value
# is to be treated

# na.rm is by default set to FALSE, meaning
# that missing values are not removed, so
# then the return value is also NA
x <- c(1, 4, NA, 10)
summary(x)
mean(x)
mean(x, na.rm=TRUE)
sd(x)
sd(x, na.rm=TRUE)

# Many of the modeling functions like lm(),
# glm() gam() have an optional argument that
# specifies how to handle missing values which
# is usually a function for preprocessing the
# input data

# na.fail() - issue an error if the object contains missing values
# na.omit() - exclude the missing values and return the rest of the object
# na.exclude() - same as na.omit() but will result in different behavior of
#                some functions (like napredict() and naresid())
# na.pass() - return also the missing values (the object remains unchanged)

## Most of the statistical modeling functions have
## an argument na.action (lm, glm, gam)
## na.action defaults to na.omit;
## can be na.exclude, na.fail, na.pass
options(error = expression(NULL))
df <- as.data.frame(matrix(c(1:7, NA), ncol = 2))
names(df) <- c("Y", "X")
df

# perform linear regression:
lm(Y~X, data=df)

lm(Y~X, data=df, na.action=na.omit)

lm(Y~X, data=df, na.action=na.fail)

### Other Special Values in R

# NaN denotes a value which is "not a number."
# Can check with function is.nan()
x <- c(1, 0, 10)
x/x # 0/0 is indeterminate
is.nan(x/x)

# Another special symbol is infinity Inf, can
# result from computations like 1/0.

# Can use is.finite() and is.infinite() to
# determine whether a number is finite or not.
1/x
is.finite(1/x)
-10/x
is.infinite(-10/x)

# Some calculations with Inf can be evaluated
# but others lead to "not a number" NaNs:
exp(-Inf)
0/Inf
Inf-Inf
Inf/Inf


