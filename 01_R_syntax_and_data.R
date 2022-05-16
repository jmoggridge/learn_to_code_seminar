##################################################
## Project: My first R script
##################################################

#' *assign an object to a name with <- *
##----------------------------------------------##
# name <- object
# use keyboard shortcut for <- : 'alt' and '-'

# store a value
my_value <- 5

# so that we can use it later
print(my_value * 10)


#' *where did `print` come from?*
##----------------------------------------------##
?print


#' *remove objects with rm()*
##----------------------------------------------##
rm(my_value)


#' *primitive data types*
##----------------------------------------------##
# we've seen these
dbl_val <- 1.2
chr_val <- 'text or string data'

# but not seen these
lgl_val <- TRUE
fct_val <- factor('category1')
date_val <- Sys.Date()
missing_val <- NA
nothing <- NULL


#' *Check out the environment panel list and grid views*
# there are other specific types but this is most of what we'll encounter

# clean up the workspace...
rm(list = ls())

#' *restart R* with *'cmd + shft + 0'*
#

#' *coerce data from one class to another*
##----------------------------------------------##
class(lgl_val)
val <- as.character(lgl_val)
class(val)


#' *we can do all the math you want*
##----------------------------------------------##
# + - * / ** // log exp sin cos tan  %/% %% ....


#' *...or boolean algebra*
##----------------------------------------------##
# | 'or'
# & 'and'
# ! 'not'
a <- T
b <- F

a | b
a & b
 !a

# 'xor': one or the other is true but not both
xor <- function(x, y){
  (x | y) & !(x & y)
}
xor(T, T)
xor(F, F)
xor(T, F)
xor(F, T)



#' **vectors**
##----------------------------------------------##
# any single value is already a vector...
print('some text')

# the [1] indicates the index of our value.
# our object named chr_val is a vector of length 1.
is.vector('some text')
length('some text')

# use is.* functions to test
is.vector('some text')
is.numeric('some text')


#' *which is.<class> function would return true for text input?*
# uncomment the next line (cmd + C), delete <class>,
# and hit tab to get autocompletion

# is.<class>('some text')


#' *combine ... into a vector with c()*
##----------------------------------------------##
my_vector <- c(1, 2, 3)
my_vector

#' *vectorized operations*
##----------------------------------------------##
#' most functions that operate on a single value will
#' also do the operation *for each* value in a vector
my_vector * 2

paste('number:', my_vector)


#' *subset with indices (generally discouraged)*
##----------------------------------------------##
another_vector <- letters[1:10]


#' *vectors can't have different types of data*
##----------------------------------------------##
my_vector <- c(1, 2, 3, '?')
print(my_vector)
class(my_vector)
as.numeric(my_vector)

#' *lists can have different types of data*
##----------------------------------------------##
my_list <- list(species = 'A', length = 1.5769, treatment = 'T')
print(my_list)

#' *but data.frames are better if data is a table*
##----------------------------------------------##
my_df <- data.frame(species = 'A',  length = 1.5769, treatment = 'T')
print(my_df)
































