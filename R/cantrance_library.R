############################################################
# This file is intended to contain all the functions 
# we write for CANTRANce, which will be compiled into 
# the cantrance R package. 
#
# Comments are in the style of the inlinedocs package
# instructions, in order to automate generation of 
# R help files
#
# Functions are organized by sections denoted using lines of
# @@@
# 
# Functions themselves are separated by lines of 
# #### and the function names are preceded by double pounds
# (e.g. ## myfunction) for search purposes
#
# JKB 2/4/2013
############################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# These functions are used across areas of CANTRANce

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - DATA MANIPULATION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## merge_preserve_sort
############################################################

merge_preserve_sort = function# Merges two data frames, preserving the original sort of the 1st data frame specified

##description<< This seems silly, but the base merge function 
## in R cannot consistently preserve the sort order, even with 
## sort=TRUE (see top R help tickets when you Google this 
## for more detail). Uses merge() with all.x=TRUE

(x,
    ### Data frame in order wished to be preserved. All rows will be kept
 y,
    ### Data frame to merge in. Non-merged rows will be deleted
 mergevars=NA,
    ### If merge variables are common to both datasets, vector of column names
 byx=NA,
    ### If merge variables are not named the same in both datasets, variable to merge on in x
 byy=NA
    ### If merge variables are not named the same in both datasets, variable to merge on in y
 ) {

    # Create a temporary id variable 
    x$tempid = 1:nrow(x)

    # Merge in y
    if (!is.na(mergevars)) { 
        x = merge(x,y,by=mergevars,all.x=TRUE)
    } else x = merge(x,y,by.x=byx,by.y=byy,all.x=TRUE)

    # Return to original sort
    x = x[order(x$tempid),]
    x$tempid=NULL
    return(x)

    ### Merged data frame with same number of rows and sort as x
}

############################################################
## pop_variable_present
############################################################

pop_variable_present = function# Tests for the presence of a variable in a CANTRAnce population

##description<< Determines whether the population data are contained in a data frame or in a list of data frames, and then tests for the presence of the variable in question

(variable_vector,
    ### String vector of one or more variable names
 pop,
    ### Data frame or list of data frames
logical=TRUE
    ### Return a logical for presence, or, the column number
    ### (if pop is a data frame) or list element (if pop 
    ### is a list). 
) 
{

    if (is.data.frame(pop)) {
        variable_names <- colnames(pop)
    } else {
        variable_names <- do.call("c",lapply(pop,colnames))
    }
    
    variable_exists = variable_vector%in%variable_names

    if (logical) return(variable_exists)
    else if (sum(variable_exists)==length(variable_vector))
        return(sapply(variable_vector,grep,lapply(pop,colnames)))
    else return('Error in pop_variable_present: cannot return index(es) because variable(s) not present')

### Logical vector of same length as input variable
}

############################################################
## head.list
############################################################

head.list = function# Display the first six rows of each data frame in a list
(x
    ### A list of data frames
) {
    lapply(x, function(y) head(y))

### A list of data frames
}

############################################################
## head2dim
############################################################

head2dim = function# Display the first six rows and first six columns of a two-dimensional data frame or matrix
(x
    ### A two-dimensional data frame or matrix
) {
    x[1:6, 1:6]

### A two-dimensional data frame or matrix
}



############################################################
## str2df
############################################################

str2df = function# Convert string input into a data frame
##description<< Converts string input into a data frame
(str, 
    ### A string
 ...
) {
    if (str!="") {
        df = read.csv(con <- textConnection(str),
                      stringsAsFactors=FALSE,
                      ...)
        close(con)
    } else df = NA
    return(df)
### A data frame
}


############################################################
## str2vec
############################################################

str2vec = function# Convert string input into a vector 
##description<< Converts string input into a vector 
(str, 
    ### A string
 vectype='character',
    ### Type of vector to create
 ...
) {
    vec = scan(con <- textConnection(str),
               what=vectype,
               sep=',',
               ...)
    close(con)
    return(vec)
### A vector
}


############################################################
## compile_list
############################################################

compile_list = function# Compile string inputs into a list
##description<< Compile up to 5 string inputs into a list
(str1='', 
    ### First string, optionally empty
 str2='',
    ### Second string, optionally empty
 str3='',
    ### Third string, optionally empty
 str4='',
    ### Fourth string, optionally empty
 str5=''
    ### Fifth string, optionally empty
) {
    l = list(str2df(str1),
             str2df(str2),
             str2df(str3),
             str2df(str4),
             str2df(str5))
    if (sum(!is.na(l))==0) l = NULL else l = l[!is.na(l)]
    return(l)
### A list
}


############################################################
## random_rows 
############################################################

random_rows = function# Randomly samples rows from a data frame

(dset,
    ### Data frame from which to sample
 n,
    ### Number of rows to sample
 rplc=FALSE
    ### Should sampling be with replacement?
) {
    return(dset[sample(nrow(dset), n, rplc), , drop=FALSE])
}


############################################################
## makeCI 
############################################################

makeCI = function# Compile mean estimate, lower, and upper confidence bounds into a single vector with appropriate formatting

(estimate,
    ### Numeric vector with mean estimate
 lower, 
    ### Numeric vector for lower bound
 upper, 
    ### Numeric vector for upper bound
 fmat
    ### Format for numbers in output, in the form accepted
    ### by sprintf()
) {
    v = paste0(sprintf(fmat, estimate),
                ' (',
                sprintf(fmat, lower),
                '-',
                sprintf(fmat, upper),
                ')')
    return(v)
### A vector
}


############################################################
## expand_dist_factor 
############################################################

expand_dist_factor = function#Expand a distribution of factor covariates into a full data frame

##description<< Expand a data frame with joint distribution
## of factor covariates into a data frame with the desired number
## of rows, with covariates distributed according to the 
## proportions specified in the original data frame

(expand_df, 
    ### Data frame of covariates to be expanded
 prop,
    ### Vector of proportions corresponding to each row
    ### in expand_df
 rows=NA
    ### Number of rows in output data frame if specific 
    ### number is desired. If NA, FIXME.
) {

    if (is.na(rows)) {
        # Determine smallest population possible to preserve
        # desired proportions
        m=1
        while(sum(round(prop*m)!=prop*m)>0) {
            m=m*10
        }
        # Cap at 10000
        if (m>10000) {
            m = 10000
            warning('In expand_dist_factor, capped m at 10000')
        }
        expand_num = prop*m
    } else expand_num = prop*rows

    # Repeat each row of covariates the number of times
    # designated by "prop"
    dset = untable(df=expand_df,
                   num=expand_num)

    # If only one covariate in the distribution, turn the
    # resultant vector into a matrix with the right column
    # name
    if (ncol(expand_df) == 1) {
        dset = data.frame(dset)
        names(dset) = names(expand_df)
    }

    return(dset)

### A data frame
}


############################################################
## expand_dist_continuous 
############################################################

expand_dist_continuous = function#Expand a distribution of continuous covariates into a full data frame

##description<< Expand a data frame with continuous
## distribution of factor covariates into a data frame with
## the desired number of rows, with covariates distributed 
## according to a truncated normal with the mean, SD, 
# mininum, and maximum specified in the original data frame

(name, 
    ### Name of covariate(s) to be expanded
 varmean,
    ### Mean value for covariate distribution
 varsd,
    ### Standard deviation for covariate distribution 
 varmin,
    ### Minimum value for covariate distribution
 varmax,
    ### Maximum value for covariate distribution
 rows
    ### Number of rows in output data frame
) {

    # Expand truncated normal based on desired 
    # specifications
    dset = data.frame(round(rtnorm(rows,
                                   mean=varmean,
                                   sd=varsd,
                                   lower=varmin,
                                   upper=varmax)))
    names(dset) = name

    return(dset)

### A data frame
}


############################################################
## dist_range_unif 
############################################################

dist_range_unif = function# Expand a range according to a uniform distribution

(n,
    ### Number of values to return from a uniform 
    ### distribution defined by rng
 rng, 
    ### String containing a range of values, separated by
    ### splitchar, to be expanded into n individual values
 splitchar='-',
    ### String character separating two values in rng
 to.round.int=FALSE
    ### If results will be rounded to the nearest integer,
    ### set to TRUE. This will expand the allowable range 
    ### for simulation so that integers in the range are
    ### evenly distributed after rounding 
) {

    rng.min=as.numeric(unlist(strsplit(rng, splitchar))[1])
    rng.max=as.numeric(unlist(strsplit(rng, splitchar))[2])
    if (to.round.int) {
        return(runif(n, rng.min-0.49999, rng.max+0.49999))
    } else {
        return(runif(n, rng.min, rng.max))
    }

### A vector of length n
}


############################################################
## expand_rangevar
############################################################

expand_rangevar = function# Take in a data frame in which one column is a range variable, then expand it to integers according to a uniform distribution

(dset,
    ### Data frame containing range variable 
 rangevar, 
    ### Name of range variable in dset
 newvar,
    ### Name of new variable to be created with expanded values
 splitchar='-'
    ### String character separating values in rangevar
) {
    names(dset)[names(dset)==rangevar] = 'rangevar'
    dset = ddply(dset,
                  .(rangevar),
                  function(x) {
                      x[, newvar] = round(dist_range_unif(nrow(x),
                          unique(as.character(x$rangevar)),
                          splitchar,
                          to.round.int=TRUE))
                      return(x)
                  })
    names(dset)[names(dset)=='rangevar'] = rangevar
    return(dset)
}


############################################################
## create_pop_list
############################################################
create_pop_list = function# Expand proportions of characteristics into a full population 

##description<< Expand a list of data frames with 
## proportions of population characteristics into a list
## of data frames with appropriately sized "populations"

(char_list,
    ### List of data frames containing proportions of
    ### population characteristics. Each data frame must
    ### contain a variable named "prop"
 n,
    ### Number of rows to which to expand each data frame
 ref_yr,
    ### Reference year for time of sample. Used to calculate
    ### birth year
 age_groups=NA,
    ### Data frame with a single column containing desired
    ### age groups, with ranges designated as "agemin-agemax"
 group_var=NA,
    ### Name of group variable, if desired
 group_val=NA
    ### Value for grouping variable
) {
  
  # Expand each data frame separately, then return as list
  pop = lapply(char_list,
               function(x) {
                   # Expand proportions to desired size
                   if ('mean'%in%names(x)) {
                       # Continuous covariate
                       if ('prop'%in%names(x)) 
                           warning('In create_pop_list(), data frame in char_list contains both prop and mean variables. Mean was used to create the expanded dataset.')
                       d = expand_dist_continuous(name=x$varname,
                                                  varmean=x$mean,
                                                  varsd=x$sd,
                                                  varmin=x$min,
                                                  varmax=x$max,
                                                  rows=n)
                   } else if ('prop'%in%names(x)) {
                       # Categorical covariate
                       if ('agegroup'%in%names(x)) {
                           d = expand_dist_factor(expand_df=subset(x,
                                                      select=-c(prop)),
                                                  prop=x$prop,
                                                  rows=n)
                           
                           # Sample from uniform distribution
                           # to assign age in specified range
                           d = expand_rangevar(dset=d,
                                               rangevar='agegroup',
                                               newvar='age',
                                               splitchar='-')
                       } else {
                           d = expand_dist_factor(expand_df=subset(x,
                                                      select=-c(prop)),
                                                  prop=x$prop,
                                                  rows=NA)
                       }
                   } else {
                       # Incorrect format
                       stop('In create_pop_list(), data frames in char_list must contain variables named either prop or mean.')
                   }

                   if ('age'%in%names(d)) {
                       # Assign birth year
                       d = transform(d, birth_year = study_year - age)
                   }
                   
                   return(d)
               })

    if (!is.na(group_var)) pop[[1]][, group_var] <- group_val
    return(pop)
  
### A list of data frames
}


############################################################
## group_ages  
############################################################

group_ages = function# Group ages within a data frame to aggregate them into an agegroup variable

##description<< Expects groups to have adjacent integer bounds, 
##e.g. 30-34, 35-39, 40-44. Non-integer ages will be rounded
##before matching to groups

(ages, 
    ### Vector of ages to be grouped 
 groups
    ### Vector of desired age groups, with ranges designated 
    ### as "agemin-agemax"
) {

    # Round ages
    ages <- round(ages)

    # Initialize vector for storing results
    agegroups <- ages

    # Loop through groups and assign group name to ages that
    # fall within that group
    for (i in 1:length(groups)) {
        g <- groups[i]
        agemin <- as.numeric(unlist(strsplit(g, '-'))[1])
        agemax <- as.numeric(unlist(strsplit(g, '-'))[2])
        agegroups[ages>=agemin & ages<=agemax] <- g
    }

    if (sum(grepl('-', agegroups))!=length(agegroups))
        warning('In group_ages(), groups do not encompass all ages.')

    return(agegroups)

### A data frame
}


############################################################
## aggregate_by_indicator
############################################################

aggregate_by_indicator = function#Aggregate a matrix by levels of an indicator matrix

##description<< Summarize matrix columns by levels of an 
## indicator variable that is also specified in a matrix,
## because its levels are different for different columns

(data,
    ### Matrix of values to be summarized
 indicator,
    ### Matrix of indicator values. Can be character strings
 fun,
    ### Summary function, e.g. "sum", "mean"
 data_name='x',
    ### What do the data represent?
 column_name = 'Column',
    ### What do the columns of the data represent?
    ### Set to 'REMOVE' to remove this column before returning
    ### results
 indicator_name = 'Indicator',
    ### What is the indicator?
 reshape_indicator=FALSE  
    ### Set to TRUE to reshape the data such that 
    ### the indicator values are wide
 ){
    summ = lapply(1:ncol(data),
                  function(x) {
                      return = aggregate(data[, x], 
                                         by=list(indicator[, x]),
                                         FUN=eval(parse(text=fun)))
                      return = data.frame(return,
                                          Column=rep(x,
                                                     nrow(return)))
                  })
    summ = do.call('rbind', summ)
    if (reshape_indicator) {
        summ = melt(summ, 
                    id.vars=c('Group.1', 'Column'), measure.vars='x')
        summ = cast(summ, Column~Group.1)
        summ = rename(summ, c(Column=column_name))
        summ = cbind(summ)
            # Otherwise it cannot be coerced to a matrix like
            # most data frames...
    } else {
        summ = rename(summ, c(Group.1=indicator_name,
                              Column=column_name,
                              x=data_name))
    }
    if (column_name=='REMOVE') summ[,'REMOVE'] = NULL
    return(summ)
### A data frame of the summary statistic, reported by indicator 
### level and by column #
}


############################################################
## aggregate_by_indicator_and_year
############################################################

aggregate_by_indicator_and_year = function# Aggregate a matrix by levels of an indicator matrix separately by year

##description<< Summarize matrix columns by levels of an
## indicator variable that is also specified in a matrix,
## because its levels are different for different columns,
## but do so BY YEAR. This assumes that the values in the
## data matrix are life-years, and that the user wants to
## separate out the summary by year

(data,
    ### Matrix of values to be summarized
 column_name = "Column",
    ### What do the columns of the data represent?
 indicator,
    ### Matrix of indicator values. Can be character strings
 indicator_name = "Indicator",
    ### What is the indicator?
 fun,
    ### Summary function, e.g. "sum", "mean"
 timehorizon
    ### What is the time horizon? Only years between time=0
    ### and this number will be summarized; years beyond
    ### will be discarded
 ){
    # Make a matrix of year indicators according to the time
    # horizon
    yearindicator = t(replicate(nrow(data), 1:timehorizon))

    summ = lapply(1:ncol(data),
                  function(x, 
                           tmax=timehorizon,
                           yrind=yearindicator) {
                      # Expand each person's life-years
                      # into an indicator of alive or
                      # dead by year, up to the max
                      # timehorizon. The indicator will
                      # contain a fraction if the person
                      # lived a fraction of that year.
                      expand.ly = replicate(tmax, data[, x])
                      expand.ly = ifelse(expand.ly>yrind,
                                         1,
                                         ifelse(yrind-expand.ly<1,
                                                expand.ly-(yrind-1),
                                                0))
                      return = aggregate(expand.ly,
                                         by=list(indicator[, x]),
                                         FUN=eval(parse(text=fun)))
                      return = data.frame(return,
                                          Column=rep(x,
                                                     nrow(return)))
                  })
    summ = do.call("rbind", summ)
    summ = rename(summ, c(Group.1=indicator_name,
                          Column=column_name))
    return(summ)

### A data frame where years are columns and values are the
### summary statistic, by indicator and column # (in the
### original data)
}


############################################################
## name_strata
############################################################

name_strata = function# Make covariate combination indicators

##description<< Determine the covariate combinations in the
## population and the data. This function specifies them in
## the way that will match the results of survfit

(data=pop[, covars], 
    ### A data frame or matrix of covariates to determine 
    ###combinations of
cov=covars
    ### A vector of covariate names
) {
    # Turn factors in data into strings
    data = as.data.frame(data, stringsAsFactors=FALSE) 
    if (ncol(data)==1) colnames(data) = cov
    cdata = apply(data,
                  2,
                  function(x) { 
                      if (is.factor(x)) {
                          return(levels(x)[unclass(x)]) 
                      } else return(x) 
                  }) 

    # Now create a stratum name for each person that is in
    # the pattern of the survfit naming scheme
    test = apply(cdata,
                 1,
                 FUN=function(x) { 
                    paste(paste0(paste0(names(x),
                                        "="),
                                 as.character(x)),
                          collapse=", ")
                 })

### A vector of character indicators for the covariate
### combinations, styled to match those generated by survfit
}            


############################################################
## data_info
############################################################

data_info = function# Collects some basic information about the user data

##description<< Tailored to examples (e) and (d) and needs
## to be generalized

(data, 
    ### Data frame
covars=NULL, 
    ### Names of continuous variables in the data frame.
    ### Default used to be ttr_covars
tabvars=NULL,
    ### Names of categorical variables 
rx.order=unique(survival_specs$rx)
    ### Order of comparison (treatment) groups
) {
    
    if (is.null(tabvars)) {
        # First compute person-time by treatment group
        pt = pyears(Surv(time, status)~rx, data=data)
        pt = with(pt, rbind(n, event, pyears, event/pyears))
        rownames(pt) = c("N", "Events", "Person-Time", "Incidence")
        pt = round(pt[, rx.order], 3)
    } else {
        # Statistics more useful for model (d):
        if (length(tabvars)<3) {
            pt = data.frame(rx_notest=c(0,0,1,1),
                            rx_test=c(0,1,0,1),
                            nocases=c(sum(data$rx_notest==0&
                                          data$rx_test==0),
                                      sum(data$rx_notest==0&
                                          data$rx_test==1),
                                      sum(data$rx_notest==1&
                                          data$rx_test==0),
                                      sum(data$rx_notest==1&
                                          data$rx_test==1)))
        } else if (length(tabvars)==3) {
            pt = data.frame(ftable(data[, tabvars])) 
            pt = pt[order(pt[, tabvars[1]]), ]        
        }
    }
   
    # Now return summary statistics by covariate 
    if (!is.null(covars)) {
        simple.stats = function(covar) {
            if (is.numeric(covar) & length(unique(covar))>5)
                return(rbind(summary(covar)["Mean"])) else
                    return(rbind(table(covar)))
        }
        s = lapply(covars[!covars%in%"rx"],
                   function(x, d=data) {
                       r = simple.stats(d[, x])
                       if (length(r)>1) {
                           colnames(r) = paste(x,
                                               colnames(r),
                                               sep="_")
                       } else if (length(r)==1) {
                           colnames(r) = paste(x,
                                               "mean",
                                               sep="_")
                       }
                       return(t(r))
                   })
        stats = do.call("rbind", s)
        colnames(stats) = "N/Mean"
    } else {
        stats=NULL
    }
    return(list(pt=pt, stats=stats))
### List of: 1) person-time or contingency table 2) N/mean
}


############################################################
## convert_to_years
############################################################

convert_to_years = function# Convert to years from other units
##description<< Converts from years, months, days, or hours
(x, 
    ### A numeric vector
 from="days"
    ### One of "years", "months", "days", or "hours"
) {
    switch(from,
           days=x/365,
           months=x/12,
           hours=x/(365*24),
           years=x)
### Numeric vector
}


############################################################
## comparevals
############################################################

comparevals = function# Returns the min or max of values in comparable matrices

##description<< Compares matrix elements and returns either
## the min or max values
(mat1, 
    ### An n by p matrix
mat2, 
    ### An n by p matrix
fun="min",
    ### Values to return: one of "min" or "max"
prefix="as"
    ### Prefix for column names in output matrix
) {
    # Convert to matrices if necessary
    if (is.data.frame(mat1)) mat1 = as.matrix(mat1)
    if (is.data.frame(mat2)) mat2 = as.matrix(mat2)

    # Check dimensions
    if (sum(dim(mat1)!=dim(mat2))>0)
        stop("In comparevals(), matrices not of same dimensions\n")

    # Generate matrix with either min or max
    mat=switch(fun,
               min=ifelse(mat1<mat2, mat1, mat2),
               max=ifelse(mat1>mat2, mat1, mat2))
    colnames(mat) = paste0(prefix, 1:ncol(mat))
    return(mat)

### A matrix of either the min or max values of the two
### input matrices
}


############################################################
## combinepops
############################################################

combinepops = function# Combine two data frames or matrices with rbind

##description<< Combine data frames/matrices and create an
## indicator variable to distinguish the data. The indicator
## can be returned separately if keeping the data as a
## matrix.

(list, 
    ### A list of data frames to be combined
 varname, 
    ### Name for the new indicator variable
 values=NULL, 
    ### Vector of values for the indicator, in the same
    ### order as the df's
 df=TRUE
    ### Return a data frame with the indicator appended? If 
    ### FALSE, a list of the data as a matrix and the
    ### indicator as a vector is returned.
 ) {

    # If values for the indicator are not specified,
    # just use the list index as the indicator
    if (is.null(values)) values = 1:length(list)

    # Stop if there aren't the same number of list 
    # elements and values
    if (!is.null(values) & length(list)!=length(values))
        stop("ERROR in combinepops")
    
    # Format 
    if (df) {
        # Create the new variable in each data frame
        newlist =  lapply(1:length(list),
                   FUN=function(x) {
                       newlist = data.frame(rep(values[x],
                                                     nrow(list[[x]])),
                                                 list[[x]])
                       if (is.null(colnames(list[[x]]))) {
                           colnames(list[[x]]) <- paste0("sim",
                                                         1:ncol(list[[x]]))
                       }
                       colnames(newlist) = c(varname,
                                             colnames(list[[x]]))
                       return(newlist)
            })

        # Return a data frame with the indicator appended
        return(do.call(rbind, newlist))
    } else {
        # Return a list of the data as a matrix and the
        # indicator as a vector
        data = do.call(rbind, list)
        indicator = lapply(1:length(values),
                           function(x) {
                               rep(values[x], nrow(list[[x]])) 
                           })
        indicator = do.call(c, indicator)
        return(list(d=data, i=indicator))
    }
### Either a data frame with the indicator as a new
### variable, or a list of 1) the data as a matrix and 2)
### the indicator as a vector
}


############################################################
## calc_simsumm_by_groups
############################################################

calc_simsumm_by_groups = function# Summarize data by groups

##description<< Simpler than calc_summ_by_groups, this function takes in a matrix/df where all columns except a group indicator store results of different simulations. Returns summaries by group across sims.

(data,
    ### Matrix/df of data with observations as rows, and 
    ### columns containing covariate data
 summ_group=NULL
    ### String containing the column name with groups by
    ### which to summarize. If NULL, returns the summary of 
    ### whole data.
 ){
    
    if (is.null(summ_group)) {
        data = transform(data, summ_group=1)
        summ_group='summ_group'
    }

    # Summarize the data
    datsum = ddply(data, c(summ_group), numcolwise(mean))

    # Take out the indicator
    indcol = which(colnames(datsum)==summ_group)
    matsum = as.matrix(datsum[,-indcol])

    # Calculate the mean, median, and 2.5% and 97.5%
    # quantiles across simulations
    summ = cbind(datsum[,indcol],
                 "Mean"=apply(matsum,
                              1,
                              FUN=mean,
                              na.rm=TRUE),
                 t(apply(matsum,
                         1,
                         FUN=quantile,
                         probs=c(0.025, 0.5, 0.975),
                         na.rm=TRUE)))
    colnames(summ)[1] = summ_group
    
    return(summ)

### A table summarizing mean, 2.5%, and 97.5% quantiles of
### summ_varname across simulations, grouped by summ_group
}
############################################################
## calc_summ_by_groups
############################################################

calc_summ_by_groups = function# Summarize data by groups

(data,
    ### Matrix/df of data with observations as rows, and 
    ### columns containing covariate data
 bootrows,
    ### Matrix/df of row indicators that can be applied to
    ### the data to recover different bootstraps of the
    ### data. Each column is a different bootstrap of the
    ### data.
 extra_data=NULL,
 extra_data_as_is=NULL,
 extra_data_name=NULL,
    ### Name of the data described in extra_data
 extra_data_bootrows=NULL,
 summ_group,
    ### String containing the column name with groups by
    ### which to summarize
 summ_varname
    ### String containing the column name for the variable
    ### to summarize
 ){
    
    # Helper function for one simulation
    summ_onesim = function(simdata, summ_group, summ_varname, x) {
        names(simdata)[names(simdata)==summ_group] <- 'group'
        names(simdata)[names(simdata)==summ_varname] <- 'varname'
        summ <- ddply(simdata,
                      .(group),
                      summarize,
                      mean=mean(varname))
        if (x==1)
            tabstr <<- data.frame(Group=summ$group)
        return(summ$mean)
    } # End summ_onesim()

    # Calculate the total HR for each individual in each
    # simulation
    nsims = ncol(bootrows)
    if (is.null(nsims)) nsims = ncol(bootrows[[1]])

    onesim_summs = sapply(1:nsims,
        function(x) {
            # Recreate the bootstrapped data
            simdata = recreate_simdata(dset=data,
                                       rows=bootrows,
                                       sim_num=x)
            simdata = add_extra_data(dset=simdata,
                                     edata=extra_data,
                                     edata_as_is=extra_data_as_is,
                                     edata_name=extra_data_name,
                                     edata_bootrows=extra_data_bootrows,
                                     sim_num=x)
               
            # Summarize the mean value based on covariates
            onesim_summ = summ_onesim(simdata,
                                      summ_group,
                                      summ_varname,
                                      x)
            return(onesim_summ)
        })
    
    # Calculate the mean, median, and 2.5% and 97.5%
    # quantiles across simulations
    summ = cbind(tabstr,
                 "Mean"=apply(onesim_summs,
                              1,
                              FUN=mean,
                              na.rm=TRUE),
                 t(apply(onesim_summs,
                         1,
                         FUN=quantile,
                         probs=c(0.025, 0.5, 0.975),
                         na.rm=TRUE)))
    names(summ)[names(summ)=='Group'] = summ_group
    
    return(summ)

### A table summarizing mean, 2.5%, and 97.5% quantiles of
### summ_varname across simulations, grouped by summ_group
}

############################################################
# rbind_list
############################################################

rbind_list = function# rbinds a list of lists and row indicators

##description<< Takes in a list (length i) OR a list (length i) 
##of lists (length j) and returns a data frame or a list of size j with the i elements rbinded. 
##Does the same for corresponding row indicators, such that
##the row indicators correctly correspond to the newly rbinded
##list elements. 
##Assumes, of course, that each of the j elements are data frames or 
##matrices who have the same columns across i elements
(lst,
    ### List OR list of lists
 rows=FALSE,
    ### Does lst contain row indicators?
 nesting=FALSE
    ### TRUE if lst is a list of lists, FALSE if it is one list
) {

    # Dimensions
    i = length(lst)

    if (nesting & i>1) {
        j = length(lst[[1]])

        # Initialize objects to return
        lst.all = vector(mode='list', length=j)

        # Rbind
        for (this_j in 1:j) {

            if (!rows) {
                # Extract this_j and rbind it to lst.all
                lst.all[[this_j]] = 
                    do.call(rbind, lapply(lst, `[[`, this_j))
            } else {
                # For rows, update the row indicators 
                # appropriately
                lst.all[[this_j]] = lst[[1]][[this_j]]
                for (this_i in 2:i) {
                    current_rowcount = nrow(lst.all[[this_j]])
                    lst.all[[this_j]] = 
                        rbind(lst.all[[this_j]], 
                              lst[[this_i]][[this_j]]+
                              current_rowcount)
                }
            }
        }
    } else {
        if (!rows) {
            lst.all = do.call(rbind, lst)
        } else {
            lst.all = lst[[1]]
            for (this_i in 2:i) {
                current_rowcount = nrow(lst.all)
                lst.all = rbind(lst.all,
                                 lst[[this_i]] + 
                                 current_rowcount)
            }
        }
    }

    return(lst.all)
}


############################################################
## combine_tables
############################################################

combine_tables = function# Combine tables into one using superrows

##description<< Combine separate tables with the same number
##of columns into one using superrow indicators

(table_list,
    ### List of data frames that have the same number
    ### of columns and column names
 table_IDs=NULL,
    ### Optional vector of string IDs that will become
    ### the superrow indicators for each table. Must 
    ### have the same length as table_list.
    ### If NULL, the names of the list elements will be used
 row_names=NULL
    ### If some of the tables have meaningful row names,
    ### enter a vector of IDs for the row names, one per
    ### table. Entries should be '' if a table doesn't
    ### have meaningful row names. The other entries MUST
    ### ALL BE THE SAME, AS THIS WILL BECOME A COLUMN HEAD

) {
    if (is.null(table_IDs)) table_IDs <- names(table_list)
    if (is.null(table_IDs)) table_IDs <- 1:length(table_list)
    if (!is.null(row_names)) {
        row_name <- unique(row_names[row_names!=''])
        if (length(row_name)!=1) stop('Error in combine_tables: can only have one non-empty row name')
    } 

    table_list <- lapply(1:length(table_list),
                         function(x) {
                            tab = data.frame(table_list[[x]])
        
                            # Add row IDs if necessary
                            if (!is.null(row_names)) {
                                tab = cbind(Row=rownames(tab),
                                            tab)
                                if (row_names[x]=='')
                                    tab$Row = rep('', nrow(tab))
                            }

                            # Add superrows
                            tab = data.frame(Table=rep('', nrow(tab)), 
                                             tab,
                                             stringsAsFactors=FALSE)
                            tab$Table[1] = table_IDs[x]
                            return(tab)
                         })

    # Bind tables together
    supertable <- do.call('rbind', table_list)

    # Scrub row and column names
    supertable <- rename(supertable, c(Table=''))
    # Special case for length 1
    if (length(table_list)==1) supertable[,1] <- NULL
    if (!is.null(row_names)) supertable <- rename(supertable,
                                                  c(Row=row_name))
    colnames(supertable) <- gsub('\\.', '', colnames(supertable))
    rownames(supertable) <- NULL

    return(supertable)
    ### Data frame of combined tables
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - BOOTSTRAPPING
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## bootstrap_rows
############################################################

# Updated 4/23/12

# Returns a dataset with nsim row indicators appended, for
# constructing bootstrapped datasets

bootstrap_rows = function# Bootstraps datasets and returns the row numbers (not names) for each sample, instead of the actual datasets

##description<< Takes a dataset, resamples the specified
## number of times with replacement, and returns the dataset
## along with the resampled row numbers.

(data, 
    ### A data frame from which to bootstrap
n_row=nrow(data),
    ### Number of rows to sample for each simulation
n_sim, 
    ### Number of simulations for which to bootstrap
prefix="sim",
    ### Prefix for the names of each bootstrapped dataset
return_data=TRUE
    ### If false, only the row indicators will be returned;
    ### default is to also return the data
) {

    # Sample row numbers nsim times for constructing nsim
    # datasets
    rows = replicate(nsim,
                     sample.int(nrow(data),
                                n_row,
                                replace=TRUE))
    colnames(rows) = paste0(prefix, 1:nsim)

    if (return_data)
        toreturn = data.frame(data, rows) else
            toreturn=rows
    return(toreturn)

### The input data with additional columns indicating nsim
### bootstrapped datasets, where the variable indicated by
### prefix contains row numbers for constructing the
### bootstrapped datasets
}


############################################################
## bootstrap_rows_list
############################################################

bootstrap_rows_list = function# Bootstrap rows from a list of data frames

(popdata,
    ### List of data frames from which rows are to be 
    ### bootstrapped
 nrows,
    ### Number of rows to bootstrap
 nsims,
    ### Number of simulations for which to bootstrap
 pfx
    ### Prefix for the names of each bootstrapped dataset
) {

    lapply(popdata,
         function(x) {
             x = bootstrap_rows(data=x,
                                n_row=nrows,
                                n_sim=nsims,
                                prefix=pfx,
                                return_data=FALSE)
             })
  
### A list of of data frames in which each data frame has
### nsims columns of bootstrapped rows
}


############################################################
## bootstrap_pop
############################################################

bootstrap_pop = function# Boostrap a population from a data set

##description<< Creates a population by bootstrapping from
## the data. For covars_specified, requires a covar_table
## data frame with columns "var1", "var2"..."varN", "prop",
## where prop indicates the proportion of observations to be
## returned for each combination of var1...varN.

(data=userdat, 
    ### Data frame
size=pop_size, 
    ### Size of boostrapped population
how=create_pop_method, 
    ### One of "simple_boostrap" (resamples rows randomly)
    ### or "covars_specified," which resamples rows
    ### according to the proportions indicated in 
    ### covar_table
covar_table=NULL
    ### Data frame with columns var1...varN, and prop,
    ### where prop indicates the proportion of observations 
    ### to be returned for the row's combination of variable
    ### values.
) {
    if (how=="simple_bootstrap") {
        if (size!=nrow(data)) {
            return(data[sample(rownames(data),
                               replace=TRUE,
                               size=pop_size),])
        } else {
            return(data)
        }
    }        
    if (how=="weighted_bootstrap") {
        # Confirm that covar_table is specified
        if (is.null(covar_table))
            stop("In bootstrap_pop(), weighted bootstrap option chosen but covar_table not specified")
        
        # Identify variables of interest
        byvars = colnames(subset(covar_table, select=-c(prop)))

        # Match desired number of observations to data,
        # by covariate
        data = merge(data, covar_table, all.x=TRUE)
        data = transform(data, size=prop*pop_size)

        # Use ddply to split the population into the groups
        # defined by the covar combinations in covar_table.
        # Then use the size specified in covar_table to
        # sample from the data
        data = ddply(data,
                     byvars,
                     function(x) {
                         thissize = unique(x$size)
                         theserows = sample(rownames(x),
                                            thissize,
                                            replace=TRUE)
                         return(x[theserows, ])
                     })
        data = subset(data, select=-c(prop, size))
        return(data)
    }
### A data frame of size pop_size
}


############################################################
## bootstrap_var
############################################################

bootstrap_var = function# Bootstrap a single variable from a population

##description<< Given a data frame or a list of data frames with 
## a specified variable,bootstrap from that variable according to rows

(dset,
    ### Data frame or matrix with column for variable of interest
    ### OR list of data frames of which one has variable of interest
 variable,
    ### Column name for variable of interest
 rows
    ### Matrix/df of row indicators that can be applied to
    ### recover different bootstraps of the data.
    ### Each column is a different bootstrap of the
    ### data
    ### OR a list of such indicators that correspond to the 
    ### list dset
) {
    if (is.data.frame(dset)) {
        sapply(1:ncol(rows),
               function(x) {
                   dset[rows[, x], variable]
               })
    } else {
        this_element <- grep(variable, dset)
        bootstrap_var(dset[[this_element]],
                           variable,
                           rows[[this_element]])
    }
    
### A matrix/df of the same dimensions as dset, containing 
### values for the variable bootstrapped from dset
}

############################################################
## add_agegroups
############################################################
add_agegroups = function# Add agegroup variable onto an existing dataset according to groups specified in a list of dataframes

(dset,
    ### Data frame or list of data frames containing 
    ### population data where rows are individuals and one
    ### column is named 'age'
 c_lst
    ### List of data frames. One data frame may contain a 
    ### column named 'agegroup' containing the desired 
    ### groupings. Otherwise, dset will be returned 
    ### unchanged
) {
    # Check whether 'agegroup' column exists in c_lst
    if (sum(grepl('agegroup', c_lst))>0) {
        # Extract desired groupings
        agegroups <- unique(c_lst[[grep('agegroup',
                                        c_lst)]]$agegroup)
        if (!is.data.frame(dset)) {
            # If dset is a list
            age_df_num <- grep('age', dset)
            dset[[age_df_num]]$agegroup <-
                group_ages(ages=dset[[age_df_num]]$age,
                           groups=agegroups)
        } else {
            # If dset is a data frame
            dset$agegroup <- group_ages(ages=dset$age,
                                        groups=agegroups)
        }
    }
    return(dset)

### A data frame or list
}


############################################################
## build_simpop
############################################################

build_simpop = function# Build a simulated population by bootstrapping from a list

(datalist, 
    ### List of data frames containing the data to be 
    ### bootstrapped then combined into a single data frame
 rowlist,
    ### List of data frames containing row numbers for 
    ### bootstrapping the data in datalist
 sim
    ### Simulation number. Indicates which column of rowlist
    ### to use
 ) {
    do.call('cbind',
            lapply(1:length(datalist),
                   function(i) {
                       datalist[[i]][rowlist[[i]][, sim], , drop=FALSE]
                   }))
}


############################################################
## recreate_simdata  
############################################################

recreate_simdata = function# Recreate a simulated dataset from a full dataset and a matrix of rows for bootstrapping

(dset,
    ### Matrix, data frame, or list of data frames with 
    ### observations as rows and columns containing 
    ### covariate data
 rows,
    ### Matrix, data frame, or list of data frames with row 
    ### indicators that can be applied to dset to recover 
    ### different bootstraps of the data. Each column is a 
    ### different bootstrap (sim) of the data.
 sim_num
    ### Number of simulation (column) to recreate
) {
    if (!is.data.frame(dset)) {
        bootdata = build_simpop(datalist=dset,
                                rowlist=rows,
                                sim=sim_num)
    } else {
        bootdata = dset[rows[, sim_num], ]
    }
    return(bootdata)

### A data frame
}


############################################################
## add_extra_data  
############################################################
add_extra_data = function# Add extra columns to a data frame within a simulation environment
(dset,
    ### Data frame with data to be added to
 edata=NULL,
    ### Matrix/df to be added to dset
 edata_as_is=NULL,
    ### Logical. If TRUE, columns from edata will be added 
    ### as columns to dset without modification. If FALSE,
    ### edata will be bootstrapped according to rows 
    ### specified in bootrows
 edata_name=NULL,
    ### Column name for edata once it's added to dset
 edata_bootrows=NULL,
    ### Matrix/df wiht row indicators that can be applied to
    ### edata to recover different bootstraps of edata. Each
    ### column is a different bootstrap (sim) of the data.
 sim_num=NULL
    ### Number of simulation (column) to recreate
) {
    if (!is.null(edata)) {
        if (edata_as_is) {
            if (!is.null(edata_name)) {
                dset[, edata_name] = edata[, sim_num]
            } else {
                stop("In add_extra_data(), edata_name must be specified when edata is given and edata_as_is is TRUE.")
            }
        } else {
            if (!is.null(edata_bootrows)) {
                orignames = names(dset)
                dset = cbind(dset,
                             edata[edata_bootrows[, sim_num], ])
                names(dset) = c(orignames, names(edata))
            } else {
                stop("In add_extra_data(), edata_bootrows must be specified when extra data is given and edata_as_is is FALSE.")
            }
        }
    }
    return(dset)
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - DETERMINE SURVIVAL PARAMETERS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## get_lambda
############################################################

get_lambda = function#Takes in parameter info for an exponential and returns the rate(s)
(param,
    ### String specifying the type of parameter to be used
    ### in the construction of the exponential curve. May 
    ### be "rate", "median", "mean", or "ksurv".
 values,
    ### Vector of values corresponding to the designated
    ### parameter.
 k=NA
    ### If "param" is "ksurv", give time for k-time survival
) {
    return(switch(param,
                  median = log(2)/values,
                  mean = 1/values,
                  ksurv = -log(values)/k,
                  rate = values))
### Vector of rates
}


############################################################
## convert_HR
############################################################

convert_HR = function#Convert list of covariate-specific survival statistics into HRs in relation to baseline survival curve
(lst=NULL,
    ### List of which each element is a data frame
    ### containing covariate combinations and the
    ### corresponding survival statistics or HRs
 surv_param,
    ### String specifying the type of survival parameter
    ### provided in lst. May be "rate", "median", "mean",
    ### or "ksurv".
 surv_k=NA,
    ### If "surv_param" is "ksurv", give time for k-time survival
 baseline_rate
    ### Rate against which to compare to determine hazard ratio
) {
    
    if (!is.null(lst)) {
        return(lapply(lst,
                      function(x) {
                          if ('stat'%in%names(x)) {
                              x$rate = get_lambda(param=surv_param,
                                                   values=x$stat,
                                                   k=surv_k)
                              x = transform(x, HR=rate/baseline_rate)
                              x = subset(x, select=-c(stat, rate))
                          } else x
                      }))
    } else return(lst)

### List of which each element is a data frame containing
### covariate combinations and the corresponding HR
}


############################################################
## calc_HR
############################################################

calc_HR = function#Calculate total HR based on covariate HRs

##description<< Matches covariate HRs to each person's
## covariates, and multiples to calculate the total HR. The
## input data structures are a bit particular. Expects
## multiple simulations.

(data,
    ### Matrix/df of data with observations as rows, and 
    ### columns containing covariate data
 bootrows,
    ### Matrix/df of row indicators that can be applied to
    ### the data to recover different bootstraps of the
    ### data. Each column is a different bootstrap of the
    ### data.
 extra_data=NULL,
 extra_data_as_is=NULL,
 extra_data_name=NULL,
    ### Name of the data described in extra_data
    ### Usually, age will be in 'data'. In special
    ### cases it may be contained in 'extra_data' instead, 
    ### in which case set extra_data_name to 'age'. This
    ### is very important for when covar_list contains 
    ### agegroups.
 extra_data_bootrows=NULL,
 covar_list,
    ### List of which each element is a data frame
    ### containing covariate combinations and the 
    ### corresponding HR
 age_in_data_not_extra_data=TRUE
 ) {
    # If there are no covariates to modify the baseline,
    # then just return a matrix of 1's
    if (is.null(covar_list)) {
        if (!is.data.frame(data)) bootrows = bootrows[[1]]
        total_HR = matrix(1,
                          nrow=nrow(bootrows),
                          ncol=ncol(bootrows))
    } else {
        # Helper function for one simulation
        calc_HR_onesim = function(simdata, covar_list) {
            HRs = sapply(covar_list,
                function(l) {
                    # Preserve row order
                    simdata$rowid = 1:nrow(simdata)

                    # Check whether the covariate is a
                    # factor or continuous based on the
                    # number of groups
                    if (nrow(l)==1) {
                        factor = FALSE
                    } else if (nrow(l)==2) {
                        factor = ifelse(!'rx'%in%names(l), 
                                        TRUE, 
                                        ifelse(ncol(l)==2,
                                               # In this case, 
                                               # HR/stat only varies 
                                               # by rx, not by rx AND 
                                               # another covariate
                                               TRUE,
                                               FALSE))
                    } else {
                        factor = TRUE
                    }
                    
                    # Combine hazard ratio based on factor
                    # status
                    if (factor) { # Factor variable
                        # Merge on HRs
                        m = merge(simdata,
                                  l,
                                  by=names(l)[!grepl("HR",
                                                     names(l))],
                                  all.x=TRUE)
                    } else { # Continuous variable
                        # Extract variable name
                        cvar = names(l)[!names(l)%in%c("rx", "HR")]
                        if (length(cvar)!=1)
                            warning("In calc_HR(), incorrect columns in covariate list")

                        # Merge on HRs (not yet unique to
                        # values
                        if (ncol(l)==2) { # Variable and HR
                            if (nrow(l)!=1) {
                                warning("In calc_HR(), wrong number of rows in covariate list")
                            } else {
                                m = data.frame(simdata,
                                               HR=l$HR,
                                               HRref=l[, cvar])
                            }
                        } else if (ncol(l)==3) { # Variable, rx, and HR
                                m = merge(simdata,
                                          l[, !names(l)%in%cvar],
                                          by="rx",
                                          all.x=TRUE)
                        } else { # Incorrectly specified
                            warning("In calc_HR(), incorrect number of columns in covariate list")
                        }
                        
                        # Exponentiate to calculate HRs
                        # corresponding to each covariate
                        # value
                        m$HR = m$HR^(m[, cvar]-m$HRref)
                    }
                    
                    # Re-sort and return
                    m = m[order(m$rowid), ]
                    return(m$HR)
                })
        } # End calc_HR_onesim()

        # Group ages if necessary
        age_in_data_not_extra_data = 
            ifelse(is.null(extra_data_name), 
                   TRUE, 
                   ifelse(extra_data_name=='age', 
                          FALSE, 
                          TRUE))
                             
        if (age_in_data_not_extra_data) {
            data = add_agegroups(dset=data,
                                 c_lst=covar_list)
        } else {
            if (!extra_data_as_is) 
            extra_data = add_agegroups(dset=extra_data, 
                                       c_lst=covar_list)
        }
        
        # Calculate the total HR for each individual in each
        # simulation
        nsims = ncol(bootrows)
        if (is.null(nsims)) nsims = ncol(bootrows[[1]])

        total_HR = sapply(1:nsims,
           function(x) {
               # Recreate the bootstrapped data
               simdata = recreate_simdata(dset=data,
                                          rows=bootrows,
                                          sim_num=x)
               simdata = add_extra_data(dset=simdata,
                                        edata=extra_data,
                                        edata_as_is=extra_data_as_is,
                                        edata_name=extra_data_name,
                                        edata_bootrows=extra_data_bootrows,
                                        sim_num=x)

               # Group ages if necessary
               if (!age_in_data_not_extra_data) {
                   if (extra_data_as_is) 
                   simdata = add_agegroups(dset=simdata,
                                           c_lst=covar_list)
                }

               # Get the HRs based on covariates
               HRs = calc_HR_onesim(simdata, covar_list)

               # Multiply HRs to get total HR
               total_HR = apply(HRs, 1, prod)
               return(total_HR)
           })
    }
    return(total_HR)

### A matrix of total HRs for each observation under each
### simulation. Thus, has the same dimensions as the input
### matrix "bootrows"
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - SURVIVAL SIMULATION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

combine_rates = function

##description<< Takes in crude cause-specific mortality probability,
## a life table for other-cause mortality, and ???
## and finds the net cause-specific mortality rate under an
## exponential distribution for cause-specific survival times
## to match the observed target mortality rate

(target_prob,
### target crude k-year mortality probability
k,
### number of years of observation
boot_pop,
### data frame containing boostrapped sample with age and
### age at other-cause death
min_rate,
### minimum net mortality rate
max_rate,
### maximum net mortality rate
num_points = 10
### number of grid points to search over
){
    n_pop = dim(boot_pop)[1]

    grid_rates = seq(min_rate,max_rate,length.out=num_points)
    diff = rep(0,num_points)
    sim_probs = rep(0,num_points)

    for(i in 1:num_points){
        specific_death = rexp(n_pop,rate=grid_rates[i]) + boot_pop$age
        died_of_disease = (specific_death < boot_pop$ageOC) & (specific_death - boot_pop$age < k)
        x = sum(died_of_disease)
        sim_probs[i] = x/n_pop
        diff[i] = sim_probs[i]-target_prob
    }

    ind = which.min(abs(diff))
    sprintf("Net rate: %f, Sim prob: %f, Diff in Prob: %f", grid_rates[ind], sim_probs[ind], diff[ind])
}

############################################################
## sim_KM
############################################################

sim_KM = function# Returns random deviates from a survival curve

##description<< Takes in event times and corresponding
## survival probs (as from a KM curve) and returns n random
## deviates

(survival, 
    ### Vector of survival probabilities, as from a KM curve
time, 
    ### Vector of event times corresponding to the survival
    ### probabilities
smalltimes, 
    ### Value to be returned if survival is higher/time is
    ### smaller than any observed. Suggestions: o,
    ### min(times), NA
bigtimes, 
    ### Value to be returned if survival is lower/time is
    ### higher than any observed. Suggestions: Inf,
    ### max(times), NA
nsims,
    ### Number of deviates to return
mindraw=0, 
    ### Minimum draw allowed on the survival scale
maxdraw=1, 
    ### Maximum draw allowed on the survival scale
draws=NULL
    ### If draws on the survival scale have already been
    ### made, specify as a vector here
) {

    # Take n_sim random draws between mindraw and maxdraw
    if (is.null(draws))
        draws = runif(nsims, min=mindraw, max=maxdraw)
    
    # Now get the corresponding times 
    ##note<< Uses a linear approximation to interpolate between observed events
    ttrs = approx(x=survival,
                  y=time,
                  method="linear",
                  xout=draws,
                  yleft=bigtimes,
                  yright=smalltimes)[["y"]]
    return(ttrs)
### Times to event representative of the input curve
}


#############################################################
### sim_exp_conditional
#############################################################
#
#sim_exp_conditional = function# Returns random deviates from an exponential distribution for a population with varying rates
#
###description<< Rates may be conditional on some covariate
### like treatment, which may be different across sims for a
### given person
#
#(pop, 
#    ### Data frame/matrix with multiple sims of the
#    ### covariate, stored in columns indicated by "prefix."
#    ### Rows are individuals in a population.
#prefix="sim", 
#    ### Prefix (a string) that identifies multiple columns
#    ### of sims
#rx=NULL, 
#    ### Name of ovariate across which rates may vary. The
#    ### prefix columns should contain sims of values of this
#    ### covariate
#stat="median", 
#    ### One of "rate", "mean", "median", or "ksurv" that
#    ### identifies how the rate parameter is specified
#k=NULL, 
#    ### If "ksurv" is chosen for "stat", the number of time
#    ### units (i.e. k=10 means 10-year survival when
#    ### time_units is years)
#params, 
#    ### Vector of rate parameters, one for each value of the
#    ### rx covariate
#n_sim=nsim, 
#    ### Number of deviates to return per person
#newprefix=NULL
#    ### Prefix to use to identify the columsn of returned
#    ### deviates
#) {
#
#    # Get lambda, and label it with the treatment names
#    lambda = switch(stat,
#                    median = log(2)/params,
#                    mean = 1/params,
#                    ksurv = -log(params)/k,
#                    rate = params)
#
#    # Function to generate n_sim deviates for each rate
#    get_deviates = function(rate, n=n_sim) {
#        return(rexp(n, rate=rate))
#    }
#
#    # If there is only one parameter, generate deviates
#    # using just that one value. Otherwise, generate
#    # different deviates by rx group
#    if (length(params)==1) {
#        times = matrix(get_deviates(lambda,
#                                    nrow(pop)*n_sim),
#                       nrow=nrow(pop))
#    } else {
#        # Attach names to parameters
#        if (!is.null(rx)) names(lambda) = rx else
#            stop("In sim_exp_conditional(), need list of rx names corresponding to parameters")
#
#        # Template matrix for results
#        times = matrix(NA, nrow=nrow(pop), ncol=n_sim)
#
#        # Counts of people by rx
#        popvals = pop[, grep(prefix, colnames(pop))]
#        rxtab = table(c(as.matrix(popvals)))
#
#        # Generate the appropriate number of deviates for
#        # each rate, and store in the right spots in "times"
#        store = lapply(names(rxtab),
#                       FUN=function(x) {
#                           vals = matrix(get_deviates(lambda[x],
#                                                      n=n_sim*nrow(pop)),
#                                       nrow=nrow(pop)) 
#                           times <<- ifelse(popvals==x,
#                                            vals,
#                                            times)
#                         })
#    }
#    if (!is.null(newprefix))
#        colnames(times) = paste0(newprefix, 1:n_sim)
#    return(times)
#### Matrix of random deviates, with rows corresponding to
#### rows of the input population
#}


############################################################
## sim_piecewiseexp_HRs
############################################################

sim_piecewiseexp_HRs = function# Returns random deviates from a regular or piecewise exponential distribution for a population with varying rates, specified by HRs

##description<< Rates are specified as baseline hazard(s)
## and hazard ratios
(baseline_rates, 
    ### Data frame of rates specifying the baseline hazards
    ### (column name "rate") and changepoints (column name
    ### "times"). Nrow = 1 if using a regular exponential
HRs,
    ### Matrix/df of HRs to be applied 
prefix=NA
    ### Prefix to apply to the sim number for column names
) {
    # Unique HRs and their counts
    unique_HRs = unique(as.vector(HRs))
    table_HRs = sapply(unique_HRs, function(x) sum(HRs==x))
    
    # Simulate and insert into appropriate cells of the
    # results matrix "sims_matrix"
    sims_matrix = matrix(NA, nrow=nrow(HRs), ncol=ncol(HRs))
    sims = lapply(1:length(unique_HRs), 
                  function(x) {
                      sims = rpexp(table_HRs[x],
                                   rate=baseline_rates$rate*unique_HRs[x],
                                   t=baseline_rates$times)
                      sims_matrix[HRs==unique_HRs[x]] <<- sims
                  })
    if (sum(is.na(sims))!=0) stop("In sim_piecewise_exp, a problem!")

     # Note, this actually works! Try it with a simple example:
     if (1==0) {
         test = cbind(c(1,2,3,4),c(4,3,2,1))
         replace_test_1 = c(8,0)
         test[test==1] = replace_test_1
     }

    if (!is.na(prefix))
        colnames(sims_matrix) = paste0(prefix,
                                       1:ncol(sims_matrix))
    return(sims_matrix)

### Matrix of random deviates of the same dimension as the
### HRs matrix
}


############################################################
## sim_piecewiseexp_HRs_restrictedrange
############################################################

sim_piecewiseexp_HRs_restrictedrange = function# Returns random deviates from a regular or piecewise exponential distribution for a population with varying rates, specified by HRs. Values can be simulated over a restricted range rather over [0,1]

##description<< Rates are specified as baseline hazard(s)
## and hazard ratios
(baseline_rates, 
    ### Data frame of rates specifying the baseline hazards
    ### (column name "rate") and changepoints (column name
    ### "times"). Nrow = 1 if using a regular exponential
HRs,
    ### Matrix/df of HRs to be applied 
lower_time_bounds=NULL,
    ### Matrix/df of lower time bounds to be applied
upper_time_bounds=NULL,
    ### Matrix/df of upper time bounds to be applied
time_max,
    ### Maximum time allowed. The rate of the terminal interval
    ### is assumed to be constant until time_max
prefix=NA
    ### Prefix to apply to the sim number for column names
) {
    # Test data; delete this after development
    if (1==0) {
        HRs = matrix(c(1,1,2,2,3,1,4,1),ncol=2)
        lower_time_bounds=matrix(c(49,54,59,65,62,70,76,85),
                                 ncol=2)
    }
    
    # Unique HRs and their counts
    unique_HRs = unique(as.vector(HRs))
    table_HRs = sapply(unique_HRs, function(x) sum(HRs==x))

    # Calculate length of time pieces and indicate when
    # the piece changes (this is a duplication of info. 
    # It's just a formatting choice)
    baseline_rates = transform(baseline_rates,
                               next_time = c(times[-1], Inf),
                               delta_t = c(times[-1], time_max)-times)
    
    # Helper matrix to indicate relevant time periods
    # Dimensions: (# cutpoints) x (# intervals), 
    # where there # cutpoints = # intervals + 1
    time_helper = matrix(0,
                         nrow=(nrow(baseline_rates)+1),
                         ncol=nrow(baseline_rates))
    time_helper[lower.tri(time_helper)] = 1

    # Matrix to store simulation results
    sims_matrix = matrix(NA, nrow=nrow(HRs), ncol=ncol(HRs))

    # Simulate, and insert results into appropriate cells 
    # of "sims_matrix". 
    # There are distinct simulations for each HR. I think
    # that can be simplified, as showed by Bender. To Do.
    sims = lapply(1:length(unique_HRs), 
        function(x) {
            # Apply HR
            HR = unique_HRs[x]
            these_rates = transform(baseline_rates,
                                    rate=rate*HR)

            # Calculate cumulative hazard for the cutpoints
            cumHaz = time_helper %*% 
                (these_rates$rate*these_rates$delta_t)
            these_rates = transform(these_rates,
                                    cumHaz = cumHaz[-length(cumHaz)],
                                    cumHaz_next = cumHaz[-1])

            # Pull out cumulative hazard corresponding to 
            # time_max
            maxHaz = these_rates$cumHaz_next[nrow(these_rates)]
                      
            # Prepare matrices
            upper_surv_bounds = lower_surv_bounds = upper_haz_bounds =
                lower_haz_bounds = draws =
                    matrix(NA, ncol=ncol(HRs), nrow=nrow(HRs))
            
            # For each piece of the piecewise exponential,
            # identify which lower time bounds fall within
            # the piece, and then translate the lower bound 
            # to its corresponding upper survival bound, via
            # the cumulative hazard. 
            if (!is.null(lower_time_bounds)) {
                for (i in 1:nrow(these_rates)) {
                    # Identify the relevant entries by time
                    # period and HR
                    these_entries = 
                        lower_time_bounds >= these_rates$times[i] &
                        lower_time_bounds < these_rates$next_time[i] &
                        HRs==HR

                    # Fill them with survival bound
                    # corresponding to their time
                    lower_haz_bounds[these_entries] = 
                        approx(x=c(these_rates$times, time_max),
                               y=c(these_rates$cumHaz, maxHaz),
                               xout=lower_time_bounds[these_entries])$y
                    upper_surv_bounds[these_entries] = 
                        exp(-lower_haz_bounds[these_entries])
                }
            } else upper_surv_bounds[HRs==HR] = 1

            # Do the same for the upper time bound
            if (!is.null(upper_time_bounds)) {
                stop("Have not coded restriction on upper time bounds, but it shouldn't take long if needed")
            } else lower_surv_bounds[HRs==HR] = 0

            # Randomly draw from the uniform distribution 
            # using the restrictions
            draws[HRs==HR] = runif(n=table_HRs[x],
                                   min=lower_surv_bounds[HRs==HR],
                                   max=upper_surv_bounds[HRs==HR])
            draws_cumHaz = -log(draws)

            # For each piece of the piecewise exponential,
            # identify which draws fall within the piece 
            # using the cumulative hazard, and calculate the 
            # corresponding times for them
            for (i in 1:nrow(these_rates)) {
                # Identify the relevant entries by
                # cumulative hazard and HR
                these_entries = draws_cumHaz >= these_rates$cumHaz[i] &
                    draws_cumHaz < these_rates$cumHaz_next[i] &
                    HRs==HR

                # Fill them with the time corresponding to 
                # their draw. This formula can be derived 
                # from Bender and agrees with Walke
                sims_matrix[these_entries] <<- these_rates$times[i] - 
                    (log(draws[these_entries]) + these_rates$cumHaz[i]) / 
                    these_rates$rate[i]
            }

            # Max time
            sims_matrix[draws_cumHaz>=maxHaz] <<- time_max
        })
    
    if (sum(is.na(sims_matrix))!=0)
        stop("In sim_piecewiseexp_HRs_restrictedrange, a problem!")

     # Note, this actually works! Try it with a simple example:
     if (1==0) {
         test = cbind(c(1,2,3,4),c(4,3,2,1))
         replace_test_1 = c(8,0)
         test[test==1] <- replace_test_1
     }

    if (!is.na(prefix))
        colnames(sims_matrix) = paste0(prefix,
                                       1:ncol(sims_matrix))
    return(sims_matrix)

### Matrix of random deviates of the same dimension as the
### HRs matrix
}


############################################################
## sim_same_qexp
############################################################

sim_same_qexp = function# Simulate new time to event using quantile from old time to event

(oldtime,
    ### Vector of time to event using old estimation method
 oldrate,
    ### Vector of rates for exponential distribution used to
    ### estimate oldtime
 newrate,
    ### Vector of rates for exponential distribution to be
    ### used to estimate new times to event
 prefix
    ### Prefix for column names in results vector/matrix
) {

    # Estimate new time to event using exponential
    # distribution quantile from old time to event
    newtime = qexp(p=pexp(oldtime, rate=oldrate),
                    rate=newrate)
    
    # Name columns with prefix
    colnames(newtime) = paste0(prefix, 1:ncol(newtime))
    
    # Correct nuances due to rounding
    newtime[abs(oldtime-newtime)<0.001] = 
        oldtime[abs(oldtime-newtime)<0.001]
    
    # Return
    return(newtime)
    
### Vector of time to event using same quantile as old time
### to event
}

############################################################
## sim_same_qdistr
############################################################

sim_same_qdistr = function# Simulate new value using quantile from a prior simulation from a distribution with different parameters

##description<< Same concept as sim_same_qexp with options for other
##distributions. In practice, input data structure is less flexible
##(I think)

(distr,
 priorsim,
    ### Vector of values simluated using prior distribution
 priorparam,
    ### Vector or data frame of parameters for prior sim
 newparam,
    ### Vector or data frame of parameters for new sim
 checkvalues=NULL,   
    ### If new sims should all be larger or smaller than
    ### prior sims, specify "larger" or "smaller" 
    ### respectively. A warning will be issued if this
    ### condition is not met
 prefix="sim"
    ### Prefix for column names in results vector/matrix
) {

    switch(distr,

           truncnorm = {

                require(msm)

                newsim = qtnorm(
                                p=ptnorm(priorsim, 
                                         mean=priorparam[["mean"]],
                                         sd=priorparam[["sd"]],
                                         lower=priorparam[["lower"]],
                                         upper=priorparam[["upper"]])
                ,
                                mean=newparam[["mean"]],
                                sd=newparam[["sd"]],
                                lower=newparam[["lower"]],
                                upper=newparam[["upper"]])

           },

           lognorm = {

               newsim = qlnorm(p=plnorm(priorsim,
                                        meanlog=priorparam[["mu"]],
                                        sdlog=priorparam[["sigma"]]),
                               meanlog=newparam[["mu"]],
                               sdlog=newparam[["sigma"]])
            },

           exponential = {

                newsim = qexp(p=pexp(priorsim, 
                                     rate=priorparam[["lambda"]]), 
                              rate=newparam[["lambda"]])
           })
    
    # Name columns with prefix
    if (is.matrix(priorsim)) {
        colnames(newsim) = paste0(prefix, 1:ncol(newsim))
    }
    
    # Correct nuances due to rounding
    correct = abs(priorsim-newsim)<0.001
    if (sum(correct)>0) newsim[correct] = priorsim[correct] 

    # Check values
    if (!is.null(checkvalues)) {
        diff = newsim-priorsim
        switch(checkvalues,
               larger={
                   warning="FALSE"%in%(diff>0)
               },
               smaller={
                   warning="FALSE"%in%(diff<0)
               })
        if (warning) warning("New values are not all ", checkvalues, " than prior values")
    }
    
    # Return
    return(newsim)
    
### Vector of simulated values using same quantile as a prior simulation
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - OTHER-CAUSE DEATH
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## calc_ac_lifespan
############################################################

calc_ac_lifespan = function# Generates an age at other-cause death from US lifetables

##description<< Given a year of birth and current age, draws 
## a random age at other-cause death from US cohort
## lifetables. 

(age, 
    ### Current age
birth_year, 
    ### Year of birth
male, 
    ### Sex: male=1 for male, male=0 for female
n_sim=100, 
    ### Number of random draws to return
time=FALSE,
    ### Return age at other-cause death, or time from
    ### current age to other-cause death?
haz=1,
    ### Should the US lifetables be modified by a hazard
    ### ratio before drawing from them? If so, the cohort
    ### survival will be raised to this number. 
lifetable=life_table
    ### Life table dataframe with columns Survival,
    ### BirthCohort, Age, and Male
) {

    # Calculate the right subset of rows based on sex and
    # birth cohort. Assumes that life table is sorted:
    # male (descending), birth year (asc.), age (asc.)

    min_birth_year = min(lifetable$BirthCohort)
    max_birth_year = max(lifetable$BirthCohort)
    min_age = min(lifetable$Age)
    max_age = max(lifetable$Age)
    start = (birth_year - min_birth_year) * (max_age - min_age + 1) + 1
    end = start + (max_age - min_age)
    if(male == 0){
    	subset_rows = (start:end) + dim(lifetable)[1]/2
    } else {
        subset_rows = start:end
    }

    # Subset data to specified birth cohort
    surv = lifetable[subset_rows, c("Age", "Survival")]
    
    # Apply hazard modifier
    if (haz!=1)
        surv$Survival = surv$Survival^haz

    # Get the max cumulative survival for specified age 
    maxu = surv[surv$Age == age, "Survival"]
    
    # Take random draws between 0 and the survival
    # probability. Because this is cumulative survival, 0
    # survival corresponds to the upper bound for age at
    # death. Next,linearly interpolate between age and
    # survival estimates and determine the age that
    # corresponds to the draw. Use 120 as the maximum age
    # possible (survival=0)
    death = with(surv,
                  sim_KM(survival=Survival,
                         time=Age,
                         smalltimes=age,
                         bigtimes=120,
                         nsims=n_sim,
                         mindraw=0,
                         maxdraw=maxu))
    
    # Convert the age into a time from entry age until
    # death?
    if (time==TRUE) death = death-rep(age, n_sim)

    # Check for errors
    if (sum(is.na(death))!=0 | sum(death==0)!=0)
        stop("In calc_ac_lifespan(), death values equal NA or O")
    return(death)

### A vector of length n_sim containing ages at other-cause
### death (or times to other-cause death, if time=TRUE)
}


############################################################
# calc_ac_lifespan_pop
############################################################

calc_ac_lifespan_pop = function# Use the individual calc_ac_lifespan function to estimate lifespans of a population

##description<< Given a population with birth years and
## ages, returns random draws of their ages at other-cause
## death

(popdata, 
    ### Data frame where individuals are rows, with columns 
    ### birth_year, age, and male
bootrows,
    ### Matrix/df of row indicators that can be applied to
    ### the data to recover different bootstraps of the
    ### data. Each column is a different bootstrap of the
    ### data
results_as_matrix=FALSE,
    ### Convert the results from a data frame to a matrix?
survHR=1
    ### Hazard ratio for survival as a modification of the
    ### life table in use
) {
    # Load life table data
    if (!"life_table"%in%ls(.GlobalEnv)) data(life_table)

    # Determine number of simulations
    nsim = ncol(bootrows)
    if (is.null(nsim)) nsim = ncol(bootrows[[1]])

    # Estimate for each simulation separately
    newpop = sapply(1:nsim,
        function(y) {
            # Construct the dataset
            if (!is.data.frame(popdata)) {
                thisboot = subset(build_simpop(datalist=popdata,
                                               rowlist=bootrows,
                                               sim=y),
                                  select=c(age, birth_year, male))
            } else {
                thisboot = popdata[bootrows[, y], 
                                   c("age",
                                     "birth_year",
                                     "male")]
            }
            thisboot$tempid = 1:nrow(thisboot)
            
            # Simulate ages at other-cause death
            ocs = ddply(thisboot,
                        .(age, birth_year, male),
                        function(x) {
                            # Extract age, birth year, and 
                            # male specifications
                            age = min(x$age)
                            birth_year = min(x$birth_year)
                            male = min(x$male)

                            # Calculate all-cause lifespan
                            if (is.data.frame(x))
                                nrow=nrow(x) else
                                    nrow=1
                            ageOCs = calc_ac_lifespan(age=age,
                                birth_year=birth_year,
                                male=male,
                                n_sim=nrow,
                                haz=survHR,
                                lifetable=life_table)
                                ageOCs = data.frame(x,
                                    ageOC=matrix(ageOCs,
                                                 nrow=nrow,
                                                 ncol=1))
                            return(ageOCs)
                        })
            
            # Return to original sort order, and return 
            # ageOC
            ocs = ocs[order(ocs$tempid), ]
            return(ocs$ageOC)
        })
    
    # Return results as a matrix?
    if (results_as_matrix) {
        colnames(newpop) = 1:nsim
        newpop = as.matrix(newpop)
    }

    return(newpop)

### A data frame or matrix with nsim columns of randomly
### drawn ages at other-cause death for individuals (rows)
}


############################################################
# calc_ac_lifespan_convert
############################################################

# Just converts between age at OC death and time to OC
# death. Choose between from="age" and from="time". Expects
# that rows are people in ocmat; age is a vector of their
# ages

calc_ac_lifespan_convert = function# Convert between age at other-cause death and time to other-cause death

##description<< Based on age at start of study, convert
## between the age time scale and study time scale

(ocmat,
    ### Matrix of times. Rows are individuals
age,
    ### Vector or matrix of ages at study start
from
    ### Either "age" or "time"
) {
    if (!from%in%c("age", "time"))
        stop("In calc_ac_lifespan_convert(), from must be set to either age or time")
    
   ocmat=switch(from,
                "age"=ocmat-age,
                "time"=ocmat+age)
   return(ocmat)
### Matrix of same dimensions as ocmat
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - SUMMARIZING RESULTS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
# cancer_death
############################################################

cancer_death = function# Determine time from study start to cancer death by adding times to intermediate outcomes

##description<< Adds times from study segments to calculate
## time from start of study to death from cancer

(data, 
    ### Matrix of times to event, encompassing all
    ### intermediate endpoints between study start and
    ### cancer death, with columns named with prefixes
    ### according to segment
prefixes_in,
    ### Vector of prefixes that identify event segments in
    ### input data
prefix_out="ttcd",
    ### Prefix for columns in output matrix
units="years", 
    ### Time units: one of "years", "months", "days", or
    ### "hours"
agevar="age", 
    ### Name of the age variable OR a matrix of ages
time=FALSE
    ### If age at death is preferable to time from study
    ### start to death, choose time=FALSE
) {
    # Add the times corresponding to each segment
    for (i in 1:length(prefixes_in)) {
        if (i==1) {
            time_to_death=data[, grep(prefixes_in[i], colnames(data))]
        } else {
            time_to_death=time_to_death +
                data[, grep(prefixes_in[i], colnames(data))]
        }
    }
    
    # Rename columns based on user-defined preference
    colnames(time_to_death) = paste0(prefix_out,
                                      1:ncol(time_to_death)) 

    # Convert to years
    time_to_death = convert_to_years(time_to_death, from=units)
    
    # Return, or calculate age at death
    if (time==TRUE) {
        return(time_to_death)
    } else {
        # Calculate age at death and return
        if (is.null(dim(agevar))) {
            # If age if constant across a row
            age_at_death = time_to_death + data[,agevar]
        } else {
            # If it's not
            age_at_death = time_to_death + agevar
        }
        return(age_at_death)
    }
### Matrix of times to death or ages at death
}


############################################################
## summarize_ecdf
############################################################

summarize_ecdf = structure(function#Summarize multiple empirical cdfs by covariate groups

##description<< Compute the empirical cdf of a distribution
## for multiple simulations and return the distribution
## of medians and a summary of the cdfs at each of the
## specified "xvalues"
    
(data, 
    ### A matrix where columns are different simulations of
    ### the data distribution and rows are individuals
xvalues=seq(0,10,0.5), 
    ### A vector of values for which to return the cdf
    ### probabilities
covar, 
    ### Vector of covariate values for each individual 
    ### by which cdf summaries should be computed
covar.order=NULL
    ### Vector of the covariate values in their preferred
    ### order
) {
   
    # Get the covariate values; use alphanumeric sort to 
    # order them if an order is not specified
    char.covar = as.character(covar)
    if (is.null(covar.order))
        covar.order = sort(unique(char.covar))

    # If there is only one simulation run, turn the data
    # into a matrix
    if (is.null(dim(data))) data = as.matrix(data)

    # Function that summarizes one run by the covariate
    # groups
    summarize_one = function(thisrun,
                             covs=char.covar,
                             xvals=xvalues,
                             covar.values=covar.order) {
        # Get the empirical cdf    
        cdfs = tapply(thisrun, INDEX=covs, FUN=ecdf)
        
        # Evaluate the empirical cdf at the intervals
        # specified by xvals, for each covariate group    
        values = sapply(covar.values,
                        FUN=function(x,
                                     c=cdfs,
                                     d=thisrun,
                                     cv=covs,
                                     xax=xvals) {
                            this.data = d[cv==x]
                            S_t = 1 - cdfs[[x]](this.data)
                            S_ts = approx(x=this.data,
                                           y=S_t,
                                           method="linear",
                                           xout=xax,
                                           yleft=1,
                                           yright=NA,
                                           f=1)[["y"]]
                            S_ts = matrix(S_ts,
                                          nrow=1,
                                          dimnames=list(names(cdfs)[x],
                                                        NULL))
                            return(S_ts)
                        },
                        USE.NAMES=TRUE)
        
        # Reformat the results into one vector. Could have
        # also done this by turning values into a data frame
        # and using "stack"
        values.long = c(values)
        names(values.long) =  c(mapply(rep,
                                       colnames(values),
                                       nrow(values)))
        return(values.long)    
    } # end summarize_one

    # Apply summarize_one to each covariate group and
    # compile results. Each column is a simulation, and rows
    # are points on the empirical cdf. Rownames indicate
    # which covariate group the empirical cdf refers to, so
    # there are as many rows as there are groups times
    # xvalues
    summaries = sapply(1:ncol(data), 
                       FUN=function(x, c=covar, d=data) {
                           tmp = summarize_one(d[, x], covs=c)
                       },
                       USE.NAMES=TRUE )

    # Now get the median and upper/lower bounds for each
    # xvalue. Using na.rm=TRUE because...well, how else to
    # summarize? Also return the mean.
    summ2 = cbind("Mean"=apply(summaries, 1, FUN=mean, na.rm=TRUE),
                  t(apply(summaries,
                          1,
                          FUN=quantile,
                          probs=c(0.025, 0.5, 0.975),
                          na.rm=TRUE)))
   
    # Now calculate the medians
    meds = sapply(covar.order,
                  FUN=function(x) {
                      d=as.matrix(data[covar==x,])
                      apply(d, 2, median)
                  })    
    # Now calculate the means
    means = sapply(covar.order,
                   FUN=function(x) {
                       d=as.matrix(data[covar==x,])
                       apply(d, 2, mean)
                   }) 

    # Format and return
    summ2 = cbind(x=rep(xvalues, length.out=nrow(summ2)), summ2)
    return(list(med=meds, mean=means, summ=summ2))

### A list of 1) Median survivals for each simulation (rows)
### by covariate group (columns), 2) Same as #1 except
### means, and 3) Median, lower, and upper bound across 
### simulations of survival probabilities at each timepoint
### specified in xvalues, by covariate group (indicated by rownames)
}, ex=function(){
    # Example code here! Just testing out the inlinedocs format 
    # for examples
})


############################################################
## summarize_censored
############################################################

# This function is similar but it summarizes multiple
# Kaplan-Meiers. Very slow right now--not sure if we can
# speed up the computation of thousands of KMs?

summarize_censored = function# Summarize multiple Kaplan-Meiers by covariate groups

##description<< Compute the net KM curve for a distribution
## with competing risk over multiple simulations, and return
## the distribution of medians and a summary of the K-M
## curves at each of the specified "xvalues"

(cancer, 
    ### Matrix of times to main endpoint. Rows are
    ## individuals
other, 
    ### Matrix of times to competing endpoint. Rows are
    ### individuals
xvalues=seq(0,10,0.5), 
    ### Vector of times to evaluate K-M survival
    ### probabilities 
covar, 
    ### Vector of covariate values for each individual by
    ### which K-Ms should be computed
covar.order=NULL
    ### Vector of the unique covariate values in their
    ### preferred order
) {

    # Covariate values
    char.covar = as.character(covar)
    if (is.null(covar.order))
        covar.order = sort(unique(char.covar))
    
    # If there is only one simulation run, turn the data
    # into a matrix
    if (is.null(dim(cancer))) {
        cancer = as.matrix(cancer)
        other = as.matrix(other)
    }

    # Prepare to save the median survival    
    meds = matrix(NA,
                  nrow=ncol(cancer),
                  ncol=length(covar.order),
                  dimnames=list(NULL, covar.order))     
        
    # Function that summarizes one run by the covariate
    # groups
    summarize_one_censored = function(thiscanc,
                                      thisother,
                                      covs=char.covar,
                                      covar.values=covar.order,
                                      xvals=xvalues,
                                      n) {
        # Establish the times and censoring status
        ctime = apply(cbind(thiscanc, thisother), 1, min)
        status = thiscanc<thisother    # TRUE if cancer death
        cdata = data.frame(time=ctime, status=status)

        # Evaluate the Kaplan Meier at the intervals
        # specified by xvals, for each covariate group
        values = sapply(covar.values,
                        FUN=function(x,
                                     c=cdata,
                                     cv=covs,
                                     xax=xvals) {
                            this.data = c[cv==x, ]

                            # Fit Kaplan-Meier curves to
                            # each of the covariate strata
                            km = survfit(Surv(time, status)~1,
                                         data=this.data)
                            kms = summary(km)
                            
                            # If there is more than one
                            # failure... 
                            if (sum(this.data$status)>1) {
                                # Try to store median survival
                                approx(y=kms$time,
                                       x=kms$surv,
                                       method="linear",
                                       xout=0.5,
                                       yright=NA)[["y"]] ->> meds[n, x]
                                
                                # Now get the estimates for
                                # the xvalues points
                                # specified. Using "rule=2"
                                # in the approx function so
                                # that when times are greater
                                # than the time of the last
                                # event, the survival will
                                # be that of the last event
                                # (consistent with the step
                                # function of KM)
                                S_ts = approx(x=kms$time,
                                               y=kms$surv,
                                               method="linear",
                                               xout=xax,
                                               yleft=1,
                                               rule=1:2,
                                               f=1)[["y"]]
                                S_ts = matrix(S_ts,
                                              nrow=1,
                                              dimnames=list(covar.values[x],
                                                            NULL))
                            } else {
                                warning(paste("\nIn summarize_censored(), sim", n, "has one or fewer failures so we cannot compute a KM curve"))
                                S_ts = matrix(NA,
                                              nrow=1,
                                              ncol=length(xax),
                                              dimnames=list(covar.values[x],
                                                            NULL))
                            }
                            return(S_ts)
                        },
                        USE.NAMES=TRUE)
                
        # Reformat the results into one vector. Could have
        # also done this by turning values into a data frame
        # and using "stack"
        values.long = c(values)
        names(values.long) =  c(mapply(rep,
                                       colnames(values),
                                       nrow(values)))
        return(values.long)
    } # end summarize_one_censored

    # Apply summarize_one_censored to each covariate group
    # and compile results 
    summaries = sapply(1:ncol(cancer),
                       FUN=function(x,
                                    c=char.covar,
                                    canc=cancer,
                                    oth=other) { 
                           tmp = summarize_one_censored(thiscanc=canc[, x],
                                                        thisother=oth[, x],
                                                        covs=c,
                                                        n=x)
                       },
                       USE.NAMES=TRUE)

    # Now get the median and upper/lower bounds for each
    # xvalue
    summ2 = cbind("Mean"=apply(summaries, 1, FUN=mean, na.rm=TRUE),
                  t(apply(summaries,
                          1,
                          FUN=quantile,
                          probs=c(0.025, 0.5, 0.975),
                          na.rm=TRUE)))
    
    # Format and return
    summ2 = cbind(x=rep(xvalues,
                        length.out=nrow(summ2)),
                  summ2)
    return(list(med=meds, summ=summ2))

### A list of 1) Net median survivals for each simulation
### (rows) by covariate group (columns) and 2) Median,
### lower, and upper bound across simulations of net
### survival probabilities at each timepoint specified in
### xvalues, by covariate group (indicated by rownames)
} 


############################################################
# summarize_cmprsk
############################################################

# Cumulative incidence - incoporating OC death into number at risk

summarize_cmprsk = function#Summarize multiple crude survival curves by covariate groups

##description<< Compute the crude KM curve for a
## distribution with competing risk over multiple
## simulations, and return the distribution of medians and
## a summary of the crude survival at each of the specified
## "xvalues"

(cancer, 
    ### Matrix of times to main endpoint. Rows are
    ### individuals
other, 
    ### Matrix of times to competing endpoint. Rows are
    ### individuals
xvalues=seq(0,10,0.5), 
    ### Vector of times to evaluate K-M survival
    ### probabilities 
covar=NULL,
    ### Vector of covariate values for each individual by
    ### which crude survivals should be computed
covar.order=NULL
    ### Vector of the unique covariate values in their
    ### preferred order
) {
    # Get the covariate values; use alphanumeric sort to 
    # order them if an order is not specified
    char.covar = as.character(covar)
    if (is.null(covar.order))
        covar.order = sort(unique(char.covar))

    # If there is only one simulation run, turn the data
    # into a matrix
    if (is.null(dim(cancer))) {
        cancer = as.matrix(cancer)
        other = as.matrix(other)
    }

    # Prepare to save the median survival    
    meds = matrix(NA,
                  nrow=ncol(cancer),
                  ncol=length(covar.order),
                  dimnames=list(NULL, covar.order))     
    
    # Function to evaluate one run
    summarize_one_cmprsk = function(c,
                                    o,
                                    r=char.covar,
                                    n=NULL,
                                    xvals=xvalues) {
        
        # Establish the time to event and which event
        # (1=cancer death, 2=other-cause death.  0 would be
        # censored.)
        time = apply(cbind(c, o), 1, min)
        status = ifelse(c<o, 1, 2) # status=1 indicates cancer death
        
        # Check for # of records
        if (length(c)>100000)
            warning("cuminc in cmp_cdf() may not run with >100,000 records")
        # Check for no competing events
        these_events = unique(status)
        if (length(these_events)==1) {
            whichcause = switch(these_events,"1"="other-cause","2"="cancer")
            stop("No ", whichcause, " events to calculate competing risk")
        }
        # Estimate the cumulative incidence function
        crude = cuminc(time, status, char.covar)
        
        # Evaluate it at the specified time points and just
        # keep the estimates where cancer is the endpoint
        # (ends with a "1" - remove that after it's not
        # needed). Using approx because "timepoints" gives
        # the step function, not linear, approximation (they
        # appear to be exactly the same though). Also being
        # careful with order so as to match the output of
        # the other summarize functions
        covar.values = names(crude)[grep("1$", names(crude))]
        use.to.get.order = order(covar.order)
        names(use.to.get.order) = 1:length(covar.values)
        covar.values =
            sort(covar.values)[as.numeric(names(sort(use.to.get.order)))]
        
        values = sapply(covar.values,
                        FUN=function(x,
                                     c=crude,
                                     xax=xvalues) {
                            unique_times = unique(c[[x]]$time)
                            unique_cuminc = timepoints(c,
                                                unique_times)$est[x, ]
                            vals = approx(x=unique_times,
                                          y=unique_cuminc,
                                          xout=xax,
                                          rule=1:2,
                                          method="linear",
                                          ties="ordered")[["y"]]
                            # plot(timepoints(c, xax)$est[x, ], vals) ;
                            # abline(a=0,b=1)
            
                            # Also record the median
                            if (!is.null(n)) {
                                if (sum(unique_cuminc!=0)>0) {
                                    approx(x=unique_cuminc,
                                           y=unique_times,
                                           method="linear",
                                           xout=0.5,
                                           yright=NA)[["y"]] ->>
                                        meds[n, gsub(" 1$", "", x)]
                                } else {
                                    warning(paste("\nIn summarize_cmprsk(), sim", n, "has one or fewer failures so a median cannot be computed"))
                                    meds[n, gsub(" 1$", "", x)] = NA
                                }
                            }
                                    
                            # Return
                            return(vals)    
                        },
                        USE.NAMES=TRUE)
        colnames(values) = gsub(" 1$", "", colnames(values))    

        # Now format it for return
        survival = stack(as.data.frame(values))
        survival$values = 1-survival$values    
        survival = matrix(survival$values,
                          ncol=1,
                          dimnames=list(survival$ind, "survival"))
        return(survival)
    } # End summarize_one_cmprsk()

    # Get it for all runs
    survs = sapply(1:ncol(cancer),
                   FUN=function(x) {
                       c=cancer[, x]
                       o=other[, x]
                       summarize_one_cmprsk(c, o, n=x)
                   })

    # This is so dumb, but the one way I can think of now to
    # get the right row names
    rownames(survs) = rownames(summarize_one_cmprsk(cancer[, 1],
                                                    other[, 1]))

    # Now get the median and upper/lower bounds for each
    # xvalue
    summ2 = apply(survs,
                  1,
                  FUN=quantile,
                  probs=c(0.025, 0.5, 0.975))
    summ2 = rbind(apply(survs, 1, FUN=mean),
                  summ2)
    rownames(summ2)[1] = "Mean"

    # Format and return
    summ2 = cbind(x=rep(xvalues, length.out=ncol(summ2)),
                  t(summ2))
    return(list(med=meds, summ=summ2))

### A list of 1) Crude median survivals for each simulation
### (rows) by covariate group (columns) and 2) Median,
### lower, and upper bound across simulations of crude
### survival probabilities at each timepoint specified in
### xvalues, by covariate group (indicated by rownames)
}


############################################################            
## calc_pys
############################################################    

# Person years saved
calc_pys = function# Summarize person-years saved across simulations

##description<< Calculate expected life-years for each
## person and compare total life years across intervention
## groups(s) to determine the distribution of life-years
## saved (or lost) by the intervention(s) 

(cancer, 
    ### Matrix of times to main endpoint. Rows are
    ### individuals
other, 
    ### Matrix of times to competing endpoint. Rows are
    ### individuals
covar, 
    ### Vector indicating intervention/control groups
covar.order=NULL,
obs=control_group,
    ### Character string indicating which group is the
    ### control
calc.time, 
    ### At what time point after study start should the
    ### person-years-saved be calculated?
suppress_negative=TRUE,
    ### Should negative person-years-saved be suppressed
    ### (i.e. don't allow person-years lost?)
colname=""
    ### Column name for output matrix
) {

    # If only one simulation run, make matrices
    if (is.null(dim(cancer))) {
        cancer = as.matrix(cancer)
        other = as.matrix(other)
    }

    # Calculate person-years-lived in each run 
    pyl = sapply(1:ncol(cancer),
                 FUN=function(x,
                              c=cancer,
                              o=other,
                              index=covar,
                              t=calc.time) {
                     # Sum the years lived by the index variable
                     death = ifelse(o[, x]<c[, x],
                                    o[, x],
                                    c[, x])
                     pyl = ifelse(death<t, death, t)
                     pyl.bycov = aggregate(pyl,
                                           by=list(covar),
                                           FUN=sum)
                     pyl.bycov.mat = cbind(pyl.bycov$x)
                     rownames(pyl.bycov.mat) = pyl.bycov$Group.1
                     if (x==1) rownames(pyl.bycov.mat) ->> mat.names
                     return(pyl.bycov.mat)
                 })    
    pyl = t(pyl)
    colnames(pyl) = mat.names

    # Summarize them compared to the control group
    treatments = which(colnames(pyl)!=obs)
    pys = sapply(colnames(pyl)[treatments], 
                 FUN=function(x, p=pyl, o=obs) {
                     pys = pyl[, x]-pyl[, o]
                     if (suppress_negative) pys = pys[pys>0]
                     if (length(pys)==0) pys = 0
                     return(c(PYS=mean(pys),
                              quantile(pys, probs=c(0.025,
                                                    0.975))))
                 })
    rownames(pys) = c("Mean",
                      "Lower Bound",
                      "Upper Bound")
    if (colname!='') colnames(pys) = colname
    if (!is.null(covar.order))
        pys = pys[, covar.order[-which(covar.order==obs)]]
    return(round(pys, 3))

### A matrix of mean, lower, and upper-bound person-years
### saved by treatment group (columns)
}


############################################################
## summarize_sims
############################################################

# Function to summarize the mean and 95% intervals across
# sims

summarize_sims = function# Summarize results across simulations

##description<< Return the mean and 2.5% and 97.5% quantiles
## across simulations

(data,
    ### Matrix of simulations. Rows are simulations and 
    ### columns are groups for which distinct summaries
    ### will be returned
 ID,
    ### ID for the data that will be used as a prefix
    ### in the column names of the results
 digits
    ### number of digits to round the results
) {

    mean = apply(data, 2, FUN=mean, na.rm=TRUE)
    ci = apply(data,
               2,
               FUN=quantile,
               probs=c(.025,.975),
               na.rm=TRUE)
    res = round(cbind(t(t(mean)), t(ci)), digits)
    colnames(res) = paste(c("Mean", "Lower", "Upper"),
                          ID)
    return(res)

### A matrix of the mean, 2.5%-ile, and 97.5%-ile for each 
### column of data
}


############################################################
## summarize_surv_types
############################################################

# Function to summarize the mean and 95% intervals for mean, 
# median, and time_max survival for a given type of survival

summarize_surv_types = function# Summarize mean and 95% confidence intervals for mean, median, and time_max survival for a given type of survival

##description<< Return the mean and 2.5% and 97.5% quantiles
## for mean, median, and time_max survival

(lst,
    ### List of matrices med, mean, and summ to be
    ### summarized. Within each matrix, rows are simulations
    ### and columns are groups for which distinct summaries
    ### will be returned
 kvals,
    ### time for which to return k-time survival
 n_arms=2
    ### number of study arms. Leave as NULL to 
    ### have the function compute it on based on
    ### data in lst
) {
    if (is.null(n_arms)) {
        # Determine number of study arms
        study_arms = c()
        for (i in 1:length(lst)) {
            if (names(lst)[i]%in%c("mean", "med")) {
                study_arms = c(study_arms, colnames(lst[[i]]))
            } else study_arms = c(study_arms, 
                                  unique(rownames(lst[[i]])))
        }
        n_arms = length(unique(study_arms))
    }

    # Function to evaluate one type of summary statistic
    summarize_one_surv_types = function(dset, summ.type, ktimes) {
        if (!is.null(dim(dset))) {
            if (summ.type%in%c("mean", "med")) {
                # Calculating mean or median survival
                label = switch(summ.type,
                               mean="Mean Survival",
                               med="Median Survival")
                mean = apply(dset,
                             2,
                             FUN=mean,
                             na.rm=TRUE)
                ci = apply(dset,
                           2,
                           FUN=quantile,
                           probs=c(.025,.975),
                           na.rm=TRUE)
                stats = round(cbind(mean=t(t(mean)),
                                    t(ci)),
                              2)
                colnames(stats) = c(label, "Lower", "Upper")
            } else {
                stats = lapply(ktimes,
                    function(k) {
                        label = paste0(k, '-Year Survival')
                        z = round(dset[dset[, "x"]==k,
                                       c("Mean", "2.5%", "97.5%")],
                                  4)
                        colnames(z) = c(label, "Lower", "Upper")
                        return(z)
                })
                stats = do.call("cbind", stats)
            }
        } else {
            # If dset is NULL, desired metric wasn't computed
            # (probably due to censoring), so return empty
            # table
            label = switch(summ.type,
                           mean="Mean Survival",
                           med="Median Survival",
                           summ="k-Year Survival")
            stats = matrix(NA,
                           nrow=n_arms,
                           ncol=3,
                           dimnames=list(NULL,
                                         c(label,
                                           "Lower",
                                           "Upper")))
        }
        return(stats)
    } #End summarize_one_surv_types()

    # Summarize the results of mean, median, or k-time
    # survival probability over the multiple simulations
    stats = lapply(c("mean", "med", "summ"),
                   function(i) {
                       # extract summary statistics for
                       # desired metric
                       return(summarize_one_surv_types(lst[[i]],
                                                       i,
                                                       kvals))
                   })
    return(do.call("cbind", stats))
### A matrix of the mean, 2.5%-ile, and 97.5%-ile for mean,
### median, and k-time survival
}


############################################################
## get_surv_summ 
############################################################

get_surv_summ = function# Get summary survival statistics for a variety of survival metrics

(lst,
    ### List, of which each element corresponds to a type
    ### of survival (e.g. all-cause, net cause-specific,
    ### etc.) and is a list of three data frames, one each
    ### for median survival ("med"), mean survival ("mean"),
    ### and k-time survival ("summ")
 times,
    ### Numeric vector representing the years for which
    ### k-year survival should be reported
 n_arms=2
    ### Number of study arms. Set to NULL to have this function
    ### compute it based on the data in lst
) {

    if (is.null(n_arms)) {
        # Determine number of study arms
        study_arms = c()
        for (i in 1:length(lst)) {
            if (names(lst)[i]%in%c("mean", "med")) {
                study_arms = c(study_arms, colnames(lst[[i]]))
            } else study_arms = c(study_arms, 
                                  unique(rownames(lst[[i]])))
        }
        n_arms = length(unique(study_arms))
    }

    dset = do.call('rbind',
                    lapply(names(lst),
                           function(x) {
                               return(summarize_surv_types(
                                         lst=lst[[x]],
                                         kvals=times,
                                         n_arms=n_arms))
                           }))

    return(data.frame(Measure=rep(names(lst), each=n_arms),
                      Group=rownames(dset),
                      dset,
                      row.names=NULL,
                      stringsAsFactors=FALSE,
                      check.names=FALSE))

### A data frame with summary survival statistics
}



############################################################
## calc_surv_stat
############################################################

calc_surv_stat = function#Calculate survival statistic based on simulated data

##description<< Matches person-years lived and event status
## to each individual's covariates, then calculates the
## corresponding survival statistic. Expects multiple
## simulations and returns mean, median, 2.5%, and 97.5%
## quantiles across simulations.

(data,
    ### Matrix/df of data with observations as rows, and 
    ### columns containing covariate data
 bootrows,
    ### Matrix/df of row indicators that can be applied to
    ### the data to recover different bootstraps of the
    ### data. Each column is a different bootstrap of the
    ### data
 extra_data=NULL,
 extra_data_as_is=NULL,
 extra_data_name=NULL,
    ### Name of the data described in extra_data
 extra_data_bootrows=NULL,
 covar_list,
    ### List of which each element is a data frame
    ### containing covariate combinations and the 
    ### corresponding HR
  ptime,
    ### Matrix/df of same dimensions as bootrows. Contains
    ### data on the person-time at risk of the event of
    ### interest
 status,
    ### Matrix/df of same dimensions as bootrows. Contains
    ### binary data on event status
 statistic,
    ### String specifying the type of survival statistic to
    ### be reported
 k=NA,
    ### If "statistic" is "ksurv", give time for k-time
    ### survival
 t_units=NA,
    ### If "statistic" is "ksurv", give time units for
    ### k-time survival
 stage_shift=FALSE,
    ### If stage needs to be shifted to scr_stage among 
    ### screen-detected individuals. Very specific to
    ### the screening model for lack of better options
    ### right now.
 rx_name='Treatment'
    ### Name for "rx" variable in output table.
 ){

    # Helper function for one simulation
    calc_stats_onesim = function(simdata,
                                 covar_list,
                                 x,
                                 statistic,
                                 k=NA) {

        # Calculate incidence rate among all individuals
        general = sum(simdata$st)/sum(simdata$pt)
    
        # Calculate incidence rate by intervention group
        by.rx.template = data.frame(Group.1=c(0,1))
        by.rx = merge(by.rx.template, 
                      aggregate(simdata[, c("pt", "st")],
                          list(simdata[, "rx"]),
                          sum),
                      all=TRUE)
        by.rx = transform(by.rx, rate=st/pt)
    
        # Calculate incidence rate based on covariates for
        # HRs
        if (!is.null(covar_list)) {
            by.cov = lapply(covar_list,
                function(l) {
                    # Find factor variables
                    if (nrow(l)==1) {
                        factor = FALSE
                    } else if (nrow(l)==2) {
                        factor = ifelse(!'rx'%in%names(l), TRUE, FALSE)
                    } else {
                        factor = TRUE
                    }

                    if (factor) {
                        vals = data.frame(subset(l, select=-c(HR)),
                                          Order=1:nrow(l))
                        if (ncol(l)==2) {
                            cov = names(l)[!grepl("HR", names(l))]
                            if (cov=='rx') return(NULL)
                            r = aggregate(simdata[, c("pt", "st")],
                                          list(cov=simdata[, cov]),
                                          sum)
                            names(r)[1] = cov
                            r$rx = 'All'
                        } else {
                            r = aggregate(simdata[,
                                            c("pt", "st")],
                                          simdata[,
                                            names(l)[!grepl("HR",
                                                            names(l))]],
                                          sum)
                        }
                        r = merge(vals, r, all=TRUE)
                        r = r[order(r$Order), 
                              names(r)[!grepl('Order', names(r))]]
                        r = transform(r, stat=st/pt)
                        r = subset(r, select=-c(pt, st))
                        covar = names(r)[!grepl('stat', names(r)) &
                                         !grepl('rx', names(r))]
                        names(r)[names(r)==covar] = "Value"
                        r = cbind(Covariate = covar, r)
                        for (var in c("Covariate",
                                      "Value",
                                      'rx'))
                            r[, var] = as.character(r[, var])
                        return(r)
                    } else return(NULL)
                })
            by.cov = do.call("rbind", by.cov)
        } else by.cov=NULL
    
        # Combine rates into a comprehensive dataframe
        if (!stage_shift) {
            stats = rbind(data.frame(Covariate="All",
                                     Value="All",
                                     rx="All",
                                     stat=general),
                          data.frame(Covariate="All",
                                     Value="All",
                                     rx=as.character(by.rx$Group.1),
                                     stat=by.rx$rate),
                          by.cov)
        } else {
            stats = rbind(data.frame(Covariate='All',
                                     Value='All',
                                     rx=as.character(by.rx$Group.1),
                                     stat=by.rx$rate),
                          by.cov)
        }
        stats = rename(stats, c(rx=rx_name))

        # Convert rate into desired survival statistic
        stats$stat = switch(statistic,
                            median = log(2)/stats$stat,
                            mean = 1/stats$stat,
                            ksurv = exp(-1*k*stats$stat),
                            rate = stats$stat)

        # Save table structure for later use
        if (x==1)
            tabstr <<- subset(stats, 
                              select=c('Covariate', 'Value', rx_name))
   
        # Return result
        return(stats$stat)
    } # End calc_stats_onesim()
 
    # Group ages if necessary
    data = add_agegroups(dset=data,
                         c_lst=covar_list)

    # For each simulation, recreate the bootstrapped data,
    # then calculate the rate by covariate group
    nsims = ncol(bootrows)
    if (is.null(nsims)) nsims = ncol(bootrows[[1]])

    all_stats = sapply(1:nsims,
        function(x) {

            # Recreate bootstrapped data
            simdata = recreate_simdata(dset=data,
                                       rows=bootrows,
                                       sim_num=x)
            simdata = add_extra_data(dset=simdata,
                                     edata=extra_data,
                                     edata_as_is=extra_data_as_is,
                                     edata_name=extra_data_name,
                                     edata_bootrows=extra_data_bootrows,
                                     sim_num=x)

            # Shift stage if applicable
            if (stage_shift) {
                simdata$stage[simdata$rx=='Screened'] = 
                    as.character(simdata$scr_stage[simdata$rx=='Screened'])
            }

            # Incorporate person-time and survival status
            simdata$pt = ptime[, x]
            simdata$st = status[, x]

            # Calculate statistic for a single simulation
            stats = calc_stats_onesim(simdata,
                                      covar_list,
                                      x,
                                      statistic,
                                      k)
            # debug: if (length(stats)!=9) browser()
            return(stats)
        })
    
    # Calculate the mean, median, and 2.5% and 97.5%
    # quantiles across simulations
    survlab = switch(statistic,
                     median = "Median Survival",
                     mean = "Mean Survival",
                     ksurv = "K-Time Survival",
                     rate = "Rate")

    stats_summ = cbind(tabstr,
                       "mean"=apply(all_stats,
                                    1,
                                    FUN=mean,
                                    na.rm=TRUE),
                       t(apply(all_stats,
                               1,
                               FUN=quantile,
                               probs=c(0.025, 0.5, 0.975),
                               na.rm=TRUE)))
    names(stats_summ)[names(stats_summ)=="mean"] = survlab
    return(stats_summ)

### A data frame summarizing the desired survival statistic
### for all individuals, individuals grouped by treatment
### status,and individuals grouped by covariates as
### designated in "covar_list". 
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - COSTS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## calc_costs_QALYs
############################################################

calc_costs_QALYs = function# Calculate the cost of treatment in a population

##description<< Takes in additional cost of treatment per
## year and computes total cost of treatment based on
## life-years on treatment. Discounts if desired.

(lifeyears,
    ### Matrix of life-years lived after treatment. Rows are
    ### individuals and columns are different simulations
 timetorecur,
    ### Matrix of times to disease recurrence (net). Rows
    ### are individuals and columns are different simulations
 treatment,
    ### Matrix of treatment indicators, for each person in
    ### each sim
 cancerdeath,
    ### Matrix of indicators whether the death was from
    ### cancer (1) or other (0)
 costutil,
    ### Data frame where treatment indicators (as strings)
    ### are the first column, (relative) costs are the
    ### second column, and utilities are the third, with
    ### column names "Treatment", "Cost", "Utility". If
    ### life-years are preferred to QALYs, use utilities of
    ### 1.0.
 costoftest=NULL,
    ### If a test was administered to each individual, input
    ### its cost. It will be multiplied by the number of
    ### individuals and added to the total cost,
    ### undiscounted since it is assumed to be administered
    ### at time 0
 discount=0,
    ### Optional discount rate expressed as a PROPORTION not
    ### PERCENT, e.g. input 3% discount rate as 0.03. Use
    ### discount=0 for no discounting
 timehorizon=NULL
    ### If discounting, must specify the time frame of the
    ### lifeyears; which should be stored in the variable
    ### time_max_inyears
) {
    # Calculate time spent before recurrence
    timeprerecur = ifelse(lifeyears>timetorecur,
                          timetorecur,
                          lifeyears)
    if (discount==0) {
        # Sum life-years by treatment group
        lifeyrsum = aggregate_by_indicator(data=lifeyears,
                                           indicator=treatment,
                                           fun='sum',
                                           column_name='Sim',
                                           indicator_name='Treatment')
        
        # Sum pre-recurrence life-years by treatment group
        # People who don't recur before OC death spend their
        # whole life in pre-recurrence
        prerecursum = aggregate_by_indicator(data=timeprerecur,
                                             indicator=treatment,
                                             fun='sum',
                                             column_name='Sim',
                                             indicator_name='Treatment')
     
        # Add count of pre-recurrence to the lifeyrsum data
        # frame
        lifeyrsum = transform(lifeyrsum,
                              PreRecur=prerecursum$x)
        lifeyrsum = rename(lifeyrsum, c(x="LifeYears"))
        lifeyrsum = transform(lifeyrsum, 
                              PostRecur = LifeYears-PreRecur)
    } else {
        # To discount, we have to separate out each year
        # after the study starts since each year has a
        # different discount rate
        lifeyrsum = aggregate_by_indicator_and_year(lifeyears,
                                                    "Sim",
                                                    treatment,
                                                    "Treatment",
                                                    "sum",
                                                    timehorizon)
    
        prerecursum = aggregate_by_indicator_and_year(timeprerecur,
                                                      "Sim",
                                                      treatment,
                                                      "Treatment",
                                                      "sum",
                                                      timehorizon)

        # Make a matrix of year indicators
        yearindicator = t(replicate(nrow(lifeyrsum),
                                    1:timehorizon))

        # Calculate the discount rate (as a matrix of same
        # dim's as lifeyrsum)
        discount_rate = 1/(1+discount)^(yearindicator-1)

        # Discounted life years
        lifeyrdisc = lifeyrsum[, grep("V", colnames(lifeyrsum))]*
                        discount_rate
        prerecurdis = prerecursum[, grep("V", colnames(prerecursum))]*
                        discount_rate
        lifeyrsum = data.frame(Treatment=lifeyrsum$Treatment,
                               Sim=lifeyrsum$Sim,
                               LifeYears=rowSums(lifeyrdisc),
                               PreRecur=rowSums(prerecurdis))
        lifeyrsum = transform(lifeyrsum,
                              PostRecur=LifeYears-PreRecur)
    }

    # Add counts of treated and untreated to lifeyrsum
    Ntreat = aggregate_by_indicator(data=matrix(1,
                                           ncol=ncol(treatment),
                                           nrow=nrow(treatment)),
                                    indicator=treatment,
                                    fun='sum',
                                    column_name='Sim',
                                    indicator_name='Treatment'
                                    )
    lifeyrsum$N = Ntreat$x

    # Calculate costs and QALYs by treatment per simulation.
    # Merge on costs and utilities
    m = merge(lifeyrsum, costutil, by="Treatment", all.x=TRUE)

    # Calculate total costs and QALYs by treatment, not yet
    # including the one-time costs that are not by
    # treatment. This is slightly different depending on
    # whether we are going through recurrence or not. 
    if (sum(m$PreRecur!=0)==0) {
        # We are not going through recurrence, because there
        # are no pre-recurrence years recorded
        return = transform(m,
                           TotCost = InitialCost*N+
                                        AnnualRecurCost*(PostRecur-N),
                           QALY = InitialUtil*N +
                                        AnnualRecurUtil*(PostRecur-N))
    } else {
        return = transform(m,
                           TotCost = InitialCost*N+
                                        AnnualCost*(PreRecur-N)+ 
                                            AnnualRecurCost*PostRecur,
                           QALY = InitialUtil*N +
                                        AnnualUtil*(PreRecur-N)+
                                            AnnualRecurUtil*PostRecur)
    }
    return = subset(return, select=c(Sim, LifeYears, TotCost, QALY))

    # Sum over treatment to get total cost and QALY per sim
    return = ddply(.data=return,
                   .variables="Sim",
                   .fun=colSums)

    # The Sim indicator was summed, so fix that
    return$Sim = rownames(return)

    # One-time future costs:
    # Recurrence
    # Set timetorecur to greater than timehorizon if
    # recurrence occurs after death (since timetorecur is
    # NET). First, set to zero if we're not going through
    # recurrence
    if (sum(m$PreRecur!=0)==0) {
        costrecur = data.frame(RecurCost=rep(0, nrow(return)))
    } else {
        timetorecur_if_alive = ifelse(timetorecur>lifeyears,
                                      timehorizon+1,
                                      timetorecur)
        costrecur = calc_one_future_cost_indicator(timetorecur_if_alive,
                                                   zero_is_zero_cost=FALSE,
                                                   treatment, 
                                                   timehorizon, 
                                                   costutil[,
                                                     c("Treatment",
                                                       "InitialRecurCost")],
                                                   "RecurCost", 
                                                   discount)
    }

    # End of life
    # Cancer
    endoflife = lifeyears*cancerdeath
    costcd = calc_one_future_cost_indicator(endoflife,
                                            zero_is_zero_cost=TRUE,
                                            treatment,
                                            timehorizon,
                                            costutil[, c("Treatment",
                                                         "EndOfLifeCD")],
                                            "CDCost",
                                            discount)

    # Other causes
    ocdeath = cancerdeath+1
    ocdeath[ocdeath==2] = 0
    endoflife = lifeyears*ocdeath
    costoc = calc_one_future_cost_indicator(endoflife,
                                            zero_is_zero_cost=TRUE,
                                            treatment,
                                            timehorizon,
                                            costutil[,c("Treatment",
                                                        "EndOfLifeOC")],
                                            "OCCost",
                                            discount)

    # If there are annual recurrence costs, subtract off the 
    # final year since we've included those as end-of-life
    # costs
    if (sum(costutil$AnnualRecurCost)!=0) {
        # Only applies to those who have post-recurrence
        # years
        lifeyears_if_recur = ifelse(lifeyears>timetorecur,
                                    lifeyears,
                                    timehorizon+1)
        minuscostrecur = calc_one_future_cost_indicator(
                           lifeyears_if_recur,
                           zero_is_zero_cost=FALSE,
                           treatment,
                           timehorizon,
                           costutil[,c("Treatment",
                                       "AnnualRecurCost")], 
                           "MinusRecurCost", 
                           discount)
        costrecur = costrecur-minuscostrecur
            # It's correct for this to be negative if we're
            # not going through recurrence
    }

    # Add these to total cost
    OnetimeCost = apply(cbind(costrecur, costcd, costoc),
                        1,
                        sum)
    return$TotCost = return$TotCost + OnetimeCost

    # Is there a cost of the test? No need to worry about
    # discounting since it is a one-time cost administered
    # in year 1
    if (!is.null(costoftest)) {
        totcostoftest = costoftest*nrow(lifeyears)
        return = transform(return,
                           TotCost = TotCost+totcostoftest)
    }
    return(return)

### A data frame of total costs and QALYs for each
### simulation
}


############################################################
## calc_ICER
############################################################

calc_ICER = function#Calculate ICER across sims

##description<< Takes in data frames from calc_costs_QALYs
## that represent populations to be compared, calculates the
## ICERs for each sim, and returns summary statistics on the
## ICER as well as on costs and QALYs for each population

(dfNew,
    ### A data frame returned by calc_costs_QALYs of
    ### TotCost, QALY, and Sim, for the population with the
    ### new intervention. If there are multiple
    ### interventions,this can be a list of data frames
 dfCompare,
    ### The corresponding data frame for the comparison
    ### group
 thresholds=NULL,
    ### If data for a cost-effectiveness acceptability curve
    ### is desired, a vector of thresholds to evaluate
 perperson=NULL
    ### To scale the costs and QALYs to be per person, 
    ### specify the number of people in the pop rather than
    ### NULL
 ) {
    
    if (!(is.data.frame(dfNew) | is.matrix(dfNew)))
        stop("calc_ICER() is not yet coded for multiple new interventions")

    # Calculate ICERs
    icer = data.frame(LY0 = dfCompare$LifeYears,
                      LY1 = dfNew$LifeYears,
                      QALY0 = dfCompare$QALY,
                      QALY1 = dfNew$QALY,
                      Cost0 = dfCompare$TotCost,
                      Cost1 = dfNew$TotCost,
                      LYDiff = dfNew$LifeYears - dfCompare$LifeYears,
                      QALYDiff = dfNew$QALY - dfCompare$QALY,
                      CostDiff = dfNew$TotCost - dfCompare$TotCost)
    
    if (!is.null(perperson)) icer = icer/perperson

    icer = transform(icer,
                     ICER_LY = CostDiff/LYDiff,
                     ICER_QALY = CostDiff/QALYDiff, 
                     Quadrant_LY = ifelse(CostDiff>0 &
                                            LYDiff>0,
                                          1, 
                                          ifelse(CostDiff>0 &
                                                    LYDiff<0,
                                                 2, 
                                                 ifelse(CostDiff<0 &
                                                            LYDiff<0,
                                                            3,
                                                            4))),
                     Quadrant_QALY = ifelse(CostDiff>0 &
                                               QALYDiff>0,
                                            1, 
                                            ifelse(CostDiff>0 &
                                                      QALYDiff<0,
                                                   2,
                                                   ifelse(CostDiff<0 &
                                                             QALYDiff<0,
                                                          3,
                                                          4))))
    summary = apply(icer,
                    2,
                    function(x) {
                        return(c(Mean=mean(x),
                                 quantile(x, probs=c(0.025, 0.5, 0.975))))
                    })

    if (!is.null(thresholds)) {
        threshmatrix = t(replicate(nrow(dfNew), thresholds))
        icer_LY = replicate(length(thresholds), icer$ICER_LY)
        icer_QALY = replicate(length(thresholds), icer$ICER_QALY)

        # Now adjust: if in quadrant 2, it's always above
        # the threshold. If in quadrant 4, always below,
        # which will naturally work out since it's negative 
        quad_LY = replicate(length(thresholds), icer$Quadrant_LY)
        quad_QALY = replicate(length(thresholds), icer$Quadrant_QALY)
        icer_LY[quad_LY==2] = max(threshmatrix)+10000
        icer_QALY[quad_QALY==2] = max(threshmatrix)+10000

        # Now calculate acceptability
        accep_LY = colMeans(icer_LY<threshmatrix)
        accep_QALY = colMeans(icer_QALY<threshmatrix)
        accep = data.frame(Thresholds=thresholds,
                           LY=accep_LY,
                           QALY=accep_QALY)
    } else accep=NULL

    return(list(ICERData=icer,
                Summary=summary,
                Acceptability=accep))
                
### A list of 1) The cost, LY, and QALY differences and
### ICERs, 2) a data frame of summary statistics for those
### measures, and 3) if thresholds is not NULL,
### cost-effectiveness acceptability
}


############################################################
## calc_one_future_cost
############################################################

calc_one_future_cost = function#Apply discounting to calculate a one-time future cost

##description<< Calculate the discounted value of a one-time
## future cost by specifying its present value, the time in
## the future it will occur, and the discount rate. 

(times,
    ### Matrix of times at which the event occurs
 timehorizon,
    ### Specify time past which cost does not matter
 cost,
    ### Cost of event
 cost_name = "Cost",
    ### Describe what cost is being calculated
 discount
    ### Discount rate specified as a proportion, e.g. 3% is
    ### 0.03
 ) {
    
    # Apply the time horizon cutoff
    times_that_matter = ifelse(times<timehorizon,
                               times,
                               0)

    # Calculate the discount rate, but if there is no event
    # because of the time horizon, make that cell entry = 0,
    # not 1.
    discount_matrix = 1/(1+discount)^times_that_matter
    discount_matrix[times_that_matter==0] = 0

    # Calculate discounted cost
    cost_matrix = cost*discount_matrix
    cost_matrix = data.frame(colSums(cost_matrix))
    colnames(cost_matrix) = cost_name
    rownames(cost_matrix) = NULL

    return(cost_matrix)

### Data frame of discounted costs; each row is the column
### sum of discounted costs in the original matrix
}


############################################################
## calc_one_future_cost_indicator
############################################################

calc_one_future_cost_indicator = function#Apply discounting to calculate a one-time future cost

##description<< Calculate the discounted value of a one-time
## future cost by specifying its present value, the time in
## the future it will occur, and the discount rate. Cost can
## differ by indicator

(times,
    ### Matrix of times at which the event occurs
 zero_is_zero_cost = FALSE,
    ### Are zeros in the times matrices meant to mean
    ### discount = 0? If so, set to false. If zeros are
    ### meant to mean zero cost, set to TRUE
 indicator,
    ### Matrix of indicator values
 timehorizon,
    ### Specify time past which cost does not matter
 costs,
    ### Data frame with columns of 1)indicator 2)costs 
 cost_name = "Cost",
    ### Describe what cost is being calculated
 discount
    ### Discount rate specified as a proportion, e.g. 3% is
    ### 0.03
 ) {

    # Apply the time horizon cutoff
    times_that_matter = ifelse(times<timehorizon,
                               times,
                               -1)

    # Calculate the discount rate, but if there is no event
    # because of the time horizon, make that cell entry = 0,
    # since we want to assign zero costs to that cell
    discount_matrix = 1/(1+discount)^times_that_matter
    discount_matrix[times_that_matter==-1] = 0

    # Also turn entries to zero that are meant to have no cost
    if (zero_is_zero_cost)
        discount_matrix[times_that_matter==0] = 0

    # Make matrix of costs according to indicator
    cost_matrix = indicator
    if (mode(cost_matrix)=="character") {
        warning("In calc_one_future_cost_indicator(), coercing treatment indicator to numeric")
        cost_matrix = apply(cost_matrix, 2, as.numeric)
    }
    for (i in costs[,1]) {
        cost_matrix[indicator==i] = costs[which(costs[, 1]==i), 2]
    }

    # Calculate discounted cost
    cost_matrix = cost_matrix*discount_matrix
    cost_matrix = data.frame(colSums(cost_matrix))
    colnames(cost_matrix) = cost_name
    rownames(cost_matrix) = NULL

    return(cost_matrix)

### Data frame of discounted costs; each row is the column
### sum of discounted costs in the original matrix
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - PLOTS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################         
## ggplot_summary
############################################################    

ggplot_summary = function# Plots survival results

##description<< Takes in a list of survival summaries to
## plot using ggplot2 and graphs them as survival curves

##note<< I need to refresh my memory on the format of the 
##survival summary

(summ, 
    ### A named list of survival summaries
estimate_column,
    ### Name of the column containing the point estimate
covar.order=NULL,
    ### The order in which the survival summaries should
    ### appear, specified as a vector of names corresponding
    ### to the names of summ
ys=c(0,1),
    ### Limits for the y-axis
ybreaks=seq(0, 1, by=0.1),
    ### Breaks for the y-axis
graph.title=""
    ### Title of graph
) {

    # Function that turns data into tidy df's. Assumes that
    # rownames indicate groups to be plotted in different colors
    make.df = function(mat) {
        df = as.data.frame(mat)
        df$group = rownames(mat)
        df = df[c("x", "2.5%", estimate_column, "97.5%", "group")]
        colnames(df) = c("Time", "Lower", "Estimate", "Upper", "Group")
        return(df)
    }

    if (is.list(summ)) {
        dflist = lapply(names(summ),
                        FUN=function(x) {
                            this.df = make.df(summ[[x]])
                            this.df$Survival = rep(x, nrow(this.df))
                            return(this.df)
                        })
        summ.df = rbind.fill(dflist)
    } else {
        stop("In ggplot_summary(), function expects a list of survivals to plot")
    }

    if (!is.null(covar.order))
        summ.df$Group = factor(summ.df$Group, levels=covar.order)
        
    theme_set(theme_bw())
    g = ggplot(summ.df)
    g = g + facet_grid(.~Survival)
    g = g + geom_line(aes(x=Time,
                          y=Estimate,
                          colour=Group))
    g = g + geom_ribbon(aes(x=Time,
                            ymin=Lower,
                            ymax=Upper,
                            fill=Group),
                        alpha=0.3,
                        colour=NA)
    g = g + scale_y_continuous(limits=ys,
                               breaks=ybreaks,
                               expand=c(0, 0))
    g = g + labs(title=graph.title,
                 x='Time',
                 y='Survival')
    g = g + theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank())
    return(g)

### A ggplot2 object
}


############################################################
## ggplot_ICER
############################################################

ggplot_ICER = function# Plot ICERs on a 4-quadrant cost-effectiveness plane

##description<< Takes the results of calc_ICER and plots the
## ICERs on a 4-quadrant cost-effectiveness plane,
## emphasizing the quadrants where the data lie

(icerdata,
    ### Data frame with columns "CostDiff" and either
    ### "LYDiff" or "QALYDiff"
 xaxis = "QALY",
    ### Choose either "QALY" or "LY" for the x-axis.
    ### Corresponding variable must be in the icerdata data
    ### frame
 thresholds = c(0, 50000, 100000)
    ### Thresholds to indicate in Quad 1 on the
    ### cost-effectiveness plane
 ) {

    # Establish axes
    if (xaxis=="QALY") {
        x = icerdata$QALYDiff 
        xlab = "QALY Difference"
        icer = icerdata$ICER_QALY
        quad = icerdata$Quadrant_QALY
    } else if (xaxis=="LY") {
        x = icerdata$LYDiff
        xlab = "Life-Year Difference"
        icer = icerdata$ICER_LY
        quad = icerdata$Quadrant_LY
    } else stop("In ggplot_ICER(), xaxis must be either QALY or LY.")
    y = icerdata$CostDiff

    # Determine x- and y-axis limits
    get_axis_limits = function(data) {
        min=min(data)
        max=max(data)

        # If the range doesn't span zero, change min or max
        if (!(min<0 & max>0)) {
            if (min<0 & max<0)
                max = (abs(min)/10) else if (min>0 & max>0)
                    min = -(max/10)
        }
        return(c(min, max))
    }
    xlim = get_axis_limits(x)
    ylim = get_axis_limits(y)

    # Determine ICER thresholds to plot as different symbols
    thresholds = c(thresholds, max(icer))
    icer_range = cut(icer,
                     breaks=thresholds,
                     dig.lab=7,
                     ordered_result=TRUE)
    icer_range = factor(icer_range,
                        levels=c("Dominant",
                                 levels(icer_range),
                                 "Dominated"))
    icer_range[quad==2] = "Dominated"
    icer_range[quad==4] = "Dominant"

    q = qplot(x,
              y,
              geom="point",
              shape=icer_range) + 
        scale_x_continuous(limits=xlim,
                           name=xlab) +
        scale_y_continuous(limits=ylim,
                           name="Cost Difference") + 
        geom_hline(yintercept=0) + 
        geom_vline(xintercept=0) + 
        scale_shape(name="ICER Category")
    return(q)
        
### A ggplot of the cost-effectiveness plane
}


############################################################
## ggplot_acceptability
############################################################

ggplot_acceptability = function# Plots a cost-effectiveness acceptability curve

##description<< Takes the results of calc_ICER and plots a
## cost-effectiveness acceptability curve for both LY and
## QALYs overlayed, if desired

(data
    ### Data frame with columns for Thresholds, LY, and QALY
 ) {

    # Melt data
    data = rename(data, c(LY="Life-Years",
                          QALY="QALYs"))
    d = melt(data, id="Thresholds")
    d = rename(d, c(variable="Benefit Measure"))
   
    # Plot
    q = qplot(Thresholds,
              value,
              group=`Benefit Measure`, 
              linetype=`Benefit Measure`,
              data=d,
              geom="line") + 
        scale_x_continuous(name="Cost-Effectiveness Threshold") + 
        scale_y_continuous(name="Probability cost-effective",
                           limits=c(0, 1))
    return(q)
### A ggplot of cost-effectiveness acceptability
}


############################################################
## baseplot_timetoevent
############################################################
    
baseplot_timetoevent = function# Plot parametric fit or KM curve of time to recurrence data

##description<< Fits the standard parametric models to the
## data based on covariate and return the plots of the fit.
## If no covariates desired, just leave covariates=1. If you
## just want to plot the data and leave out the parametric
## fit, use distribution=NULL

(data, 
    ### Time to event data with covariates "time" and
    ### "status"
distribution, 
    ### Parametric distribution to overlay. One of
    ### "weibull", "exponential", "gaussian", "logistic",
    ### "lognormal", "loglogistic" OR NULL -- if NULL, will
    ### plot the KM curve 
covar=1,
    ### Covariates in the model. Set to 1 if no covariates
    ### desired. ***I'm not sure I tested it with real
    ### covariates; it may not work
group_var="rx",
    ### Single covariate in data by which separate KM curves
    ### can be plotted on top of the full KM curve and/or
    ### parametric curve. Set to NULL if there is no grouping
    ### variable of interest
time_max=NULL, 
    ### Maximum time to plot. If NULL, defaults to
    ### max(data$time)
prob_surv_min=0, 
    ### What should be the minimum survival probability
    ### for the plot?
covar.order=NULL,
    ### If covariates are specified, should there be an
    ### order they are displayed in the legend?
empirical_curve=NULL,
    ### summarize_ecdf 'summ' data frame 
    ### Make sure to set time_max appropriately
    ### given that you are overlaying this curve
empirical_curve_color="red",
    ### Applies if empirical_curve is not NULL
labels=NULL 
    ### Optional vector of title, yaxis and xaxis labels
    ### IN THAT ORDER
) {
    
    # Specify the formula for fitting time 
    formula = paste("Surv(time, status) ~",
                       paste(covar, collapse="+"))
    
    # Fit the model
    if (!is.null(distribution)) {
        model = survreg(as.formula(formula),
                           dist=distribution,
                           data=data)
        print("Model:")
        print(formula)
    }
    
    # Now return the plot of the data stratified by
    # group_var, and plot a model fit to the full data
    
    # Plot out to the time_max (if null, max time in the
    # data)
    if (is.null(time_max)) time_max = max(data$time)
        
    if (!is.null(distribution)) {
        # If indicated, fit a model to the full data, no
        # covariates
        full_model = survreg(Surv(time,status)~1,
                                dist=distribution,
                                data=data)
        
        # Get fitted values for a pretty complete curve
        # I checked this for the Weibull against pweibull,
        # and it produced the right curve. The "newdata"
        # command confuses me, not sure why specifying a
        # constant is necessary when there are no
        # covariates, but at least it works
        full_model_fitted = predict(full_model,
                                        newdata=data.frame(1),
                                        type="quantile",
                                        p=seq(0, 1-prob_surv_min, 0.01),
                                        se=TRUE)
        
        # Truncate the values at time_max, for the plot
        fitted_subset = full_model_fitted$fit[
                            full_model_fitted$fit >=0 &
                            full_model_fitted$fit <= time_max]
        prob_subset = seq(0, 1-prob_surv_min, 0.01)[
                         full_model_fitted$fit >=0 &
                         full_model_fitted$fit <= time_max]
        
        # Plot the parametric curve
        plot(fitted_subset,
             1-prob_subset,
             type="l",
             xlab="Time",
             ylab="Probability of not recurring",
             main=distribution,
             ylim=c(0, 1))
        
        # Add the KM curve    
        lines(survfit(Surv(time, status)~1,
                      data=data), 
              col="black")    
    } else {            
        # If no model fitted, just plot a KM of the full
        # data
        plot(survfit(Surv(time, status)~1,
                     data=data),
             col="black",
             xlim=c(0, time_max))
    }
    
    # Add the KM curve for each treatment group
    if (!is.null(group_var)) {
        if (is.null(covar.order)) 
            covar.order =  as.character(sort(unique(data[,group_var])))               
        for (i in 1:length(covar.order)) {
            r = covar.order[i]
            data_subset = data[as.character(data[,group_var])==r, ]
            lines(survfit(Surv(time, status)~1,
                          data=data_subset),
                  col=i+1)
        }
        
        # Add a legend    
        legend("bottomleft",
               fill=1:(length(covar.order)+1), 
               legend=c("Full data", covar.order),
               bty="n",
               ncol=2)
    }

    if (!is.null(empirical_curve)) {
        # Overlay the empirical curve
        lines(empirical_curve[,'x'], empirical_curve[,'Mean'], 
              col=empirical_curve_color)
        lines(empirical_curve[,'x'], empirical_curve[,'2.5%'], 
              col=empirical_curve_color, lty=2)
        lines(empirical_curve[,'x'], empirical_curve[,'97.5%'], 
              col=empirical_curve_color, lty=2)
    }

    # Labels
    if (!is.null(labels)) 
        title(main=labels[1], xlab=labels[2], ylab=labels[3])
    
    # example
    if (1==0) {                
        # Plot the fits of several distributions
            library(survival)
            data(colon)
            these_dist = c("weibull",
                           "exponential",
                           "gaussian",
                           "logistic",
                           "lognormal",
                           "loglogistic")
            pdf(file="parametric fits.pdf", width=8, height=11)
            layout(matrix(1:length(these_dist), ncol=2), widths=c(1,1))
            sapply(these_dist,
                   FUN=baseplot_timetoevent,
                   data=colon)
            dev.off()
    }
### A base package plot
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMMON FUNCTIONS - MISCELLANEOUS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## track_version
############################################################

track_version = function# Creates or appends to a "version info" file

##description<< Takes in a small data frame containing
## version information and either writes it as a new file or
## appends to existing file. 

(version_info, 
    ###A data frame that must have a column called "version"
    ### in which a unique identifier is stored (can be
    ### alphanumeric). Example:
    ### vinfo = data.frame(version="edrn_1", description=
    ### "Linear hazard, varying rules")
filename
    ### Name of .csv file to write or append to
) {
    if (file.exists(filename)) {
        tmp = read.csv(filename, header=TRUE)
        if (!v%in%tmp$version) {
            write.table(version_info, 
                        filename, 
                        sep=",",
                        append=TRUE, 
                        row.names=FALSE,
                        col.names=FALSE)
        }
    } else write.table(version_info, filename, sep=",", row.names=FALSE)
### No return value
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# AREA (A) - PREVENTION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## recreate_pinsky_calc 
############################################################

recreate_pinsky_calc = function# Re-calculate death rate using an approach comparable to that used by Pinsky et al. for REDUCE study

##description<< Matches incidence status and cause-specific
## survival status to each individual's covariate of
## intereset, then calculates the corresponding death rate.
## Expects multiple simulations and returns mean, median, 
## 2.5%, and 97.5% quantiles across simulations.

(data,
    ### Matrix/df of data with observations as rows, and 
    ### columns containing covariate data
 bootrows,
    ### Matrix/df of row indicators that can be applied to
    ### the data to recover different bootstraps of the
    ### data. Each column is a different bootstrap of the
    ### data
 extra_data=NULL,
 extra_data_as_is=NULL,
 extra_data_name=NULL,
    ### Name of the data described in extra_data
 extra_data_bootrows=NULL,
 covar_tab,
    ### Data frame containing covariate strata and desired
    ### order.
 inc_status,
    ### Matrix/df of same dimensions as bootrows. Contains
    ### binary data on incidence status
 css_status
    ### Matrix/df of same dimensions as bootrows. Contains
    ### binary data on cause-specific death status
 ){

    # For each simulation, recreate the bootstrapped data,
    # then calculate the rate by covariate group
    nsims = ncol(bootrows)
    if (is.null(nsims)) nsims = ncol(bootrows[[1]])
    
    # Extract covariate information
    covar = names(subset(covar_tab, select=-c(order)))
    
    # Calculate death rate for each simulation separately
    all_stats = sapply(1:nsims,
        function(x) {
            # Recreate bootstrapped data
            simdata = recreate_simdata(dset=data,
                                       rows=bootrows,
                                       sim_num=x)
            simdata = add_extra_data(dset=simdata,
                                     edata=extra_data,
                                     edata_as_is=extra_data_as_is,
                                     edata_name=extra_data_name,
                                     edata_bootrows=extra_data_bootrows,
                                     sim_num=x)
           
            # Incorporate survival status
            simdata$inc_st = inc_status[, x]
            simdata$css_st = css_status[, x]
            simdata$counter = 1
            
            # Calculate simulation statistics for a single 
            # simulation
            simstats = aggregate(simdata[, c('inc_st', 
                                             'css_st', 
                                             'counter')],
                                 list(cov=simdata[, covar],
                                      rx=simdata$rx),
                                 sum)
            simstats = merge(simstats,
                             aggregate(simstats$counter,
                                       list(rx=simstats$rx),
                                       sum), 
                             all=TRUE)
            simstats = rename(simstats, c(cov=covar,
                                          inc_st='inc_cases',
                                          css_st='deaths',
                                          counter='N',
                                          x='N_total'))
            simstats = within(simstats,
                              {inc_pct = inc_cases/N_total
                               death_rate = deaths/N_total*1000})
            simstats = ddply(simstats,
                .(rx),
                function(y) {
                    r = unique(y$rx)
                    tmp = merge(y, covar_tab, all=TRUE)
                    tmp$rx[is.na(tmp$rx)] <- r
                    return(tmp)
                })
            simstats = simstats[order(simstats$rx, simstats$order), ]
    
            # Return result
            return(simstats$death_rate)
        })
    
    # Calculate the mean, median, and 2.5% and 97.5% 
    # quantiles across simulations
    stats_summ = cbind("mean"=apply(all_stats,
                                    1,
                                    FUN=mean,
                                    na.rm=TRUE),
                       t(apply(all_stats,
                               1,
                               FUN=quantile,
                               probs=c(0.025, 0.5, 0.975),
                               na.rm=TRUE)))
    stats_summ = data.frame(rx=rep(c(0, 1), each=nrow(covar_tab)),
                            cov=rep(covar_tab[order(covar_tab$order),
                                              names(subset(covar_tab,
                                                           select=-c(order,
                                                               mort_rate)))],
                                    2),
                            stats_summ)
    names(stats_summ) <- c('rx', covar, 'Mean', '2.5%', '50%', '97.5%')
    return(stats_summ)

### A data frame with summary statistics for the observed
### death rate across simulations, calculated using the same
### approach as was used by Pinsky et al.
}
 

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# AREA (B) - BIOMARKERS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## clinical_incidence
############################################################

clinical_incidence = function# Generates times to and ages at incidence

##description<< Simulates onset of clinical disease, based on 
## age-specific incidence rates assumed to approximate disease-
## specific hazards

(age_at_entry,
    ### Matrix of ages at entry (i.e., people have survived
    ### incidence-free until this age)
 inc,
    ### Data frame with an "agegroup" and "rate" variable that
    ### specifies the incidence rates for each age category
 age_max,
    ### Maximum age at incidence allowed. Will be used as the
    ### upper draw possible for incidence in the oldest 
    ### age group
 nsim,
    ### Number of simulations
 prefix="sim"
    ### Prefix for column names of each simulation
) {
    # Get age categories of pop members
    #matchedage = assign_to_category(inc$agegroup, pop$age)
    #pop$agegroup = matchedage$categories
    #interval = matchedage$interval

    # Assume that before the first age category specified,
    # incidence is zero. Also format incidence data for
    # the time to incidence simulation function
    inc_times = as.numeric(sapply(strsplit(inc$agegroup,"-"), 
                                  "[", 
                                  1))
    inc_formatted = data.frame(times=inc_times,
                               rate=inc$rate)
    inc_formatted = rbind(rep(0,2),inc_formatted)

    # Now apply the incidences as a step-function constant
    # hazard over age to determine time to clinical incidence
    age_at_inc = sim_piecewiseexp_HRs_restrictedrange(
                     baseline_rates=inc_formatted, 
                     HRs=matrix(1, nrow=nrow(age_at_entry), ncol=nsim), 
                     time_max=age_max, 
                     lower_time_bounds=age_at_entry, 
                     upper_time_bounds=NULL)

    colnames(age_at_inc) = paste0(prefix,1:nsim)
    return(age_at_inc)

### Input population with simulated ages at/times to incidence
}

############################################################
## get_param
############################################################

get_param = function# Calculate parameters for specified distribution, given mean and/or median

##description<< Take in data and return lognorm or exponential parameters

(df,
    ### Data frame with columns mean and median
 distr,
    ### Specify distribution: "lognorm" or "exponential" 
    ### or "truncnorm"
 anchor=NULL
    ### If choosing exponential, specify whether 
    ### its one parameter should be calculated
    ### based off the mean or median. If no anchor
    ### is specified, the default will be to 
    ### use whichever column has non-NA entries
 ) {

    colnames(df) = tolower(colnames(df))

    switch(distr,

           truncnorm = {

               if ("mu"%in%colnames(df)) {
                   # Don't overwrite mu calculated
                   # for other distributions
                   dftot = df
                   df = subset(df, distribution=="truncnorm")
               } 

               # "Calculate" mu and sigma
               df = transform(df,
                              mu=mean,
                              sigma=sd)

               if ("dftot"%in%ls()) {
                   # Sub back in the truncnorm bit
                   dftot[dftot$distribution=="truncnorm",] = df
                   df = dftot
               }

           },

           lognorm = {

                # Check that mean is greater than median
                mean_med = sum(df[df$distribution=="lognorm","mean"]<df[df$distribution=="lognorm","median"])
                if (mean_med>0) stop("In fit_df, mean must be greater than median to fit a lognormal distribution to the data")

               if ("mu"%in%colnames(df)) {
                   # Don't overwrite mu calculated
                   # for other distributions
                   dftot = df
                   df = subset(df, distribution=="lognorm")
               } 

                # Calculate mu and sigma
                df = transform(df,
                                    mu=log(median),
                                    sigma2=2*log(mean/median))
                df = transform(df,
                                    sigma=sqrt(sigma2),
                                    lognorm_variance = (exp(sigma2)-1)*exp(2*mu+sigma2))

                if (length(grep("distr",colnames(df)))==0) {
                    cat("Adding a column in get_param...consider
                        changing this")
                    df = transform(df, 
                                        distr=rep("lognorm", nrow(df)))
                }

                if ("dftot"%in%ls()) {
                   dftot[dftot$distribution=="lognorm",] = df
                   df = dftot
                }
           },

           exponential = {
                # Determine anchor if it isn't specified
                if (is.null(anchor)) {
                    if ('mean'%in%colnames(df)) {
                        namean = sum(is.na(df$mean))
                    } else namean = nrow(df)
                    if ('median'%in%colnames(df)) {
                        named = sum(is.na(df$median))
                    } else named = nrow(df)
                    if (sum(namean+named)==0) {
                        stop("Must specify how to anchor exponential in get_param: mean or median?")
                    } else {
                        anchor = ifelse(namean==0,
                                        "mean",
                                        ifelse(named==0,
                                               "median",
                                               stop("Problem in fitting exponential to data in get_param")))
                    }
                }

                # Calculate lambda
                if (anchor=="mean") {
                    df = transform(df,
                                   lambda=1/mean)
                    df = transform(df,
                                   exponen_median = log(2)/lambda,
                                   exponen_variance = 1/(lambda^2))
                }
                if (anchor=="median") {
                    df = transform(df,
                                   lambda=log(2)/median)
                    df = transform(df,
                                   exponen_mean = 1/lambda,
                                   exponen_variance = 1/(lambda^2))
                }
                if (length(grep("distr", colnames(df)))==0) {
                    cat("Adding a column in get_param...consider
                        changing this")
                    df = transform(df, 
                                   distr=rep("exponential", nrow(df)))
                }
           }
    ) #end switch

    return(df)
}


############################################################
## biomarker_trajectory
############################################################

biomarker_trajectory = function# Generate biomarker trajectories 

##description<< Take in a mean and variance for the starting 
##level and proportion change in a biomarker, and 
##simulates those values for a population using a specified
##distributional assumption normals (can choose regular normals by setting limits of 
# -Inf and Inf)
# For constant marker, use mu_change = sigma_change = 0
 
(biodata, 
    ### Vector (or data frame) of biomarker properties, with 
    ### (column) names:
    ### "mu" for the distribution mean, 
    ### "sigma" for standard deviation, 
    ### "lower" for the lowest value allowed, and 
    ### "upper" for the highest value allowed. 
    ### "lower" can be -Inf and "upper" can be Inf 
 nval,
    ### Number of simulated values to return
    ### If biodata is a data frame with columns 
    ### storing the parameters for each row/simulation,
    ### nval should be set equal to nrow(biodata)
 nsim=1,
    ### If there are multiple simulations being 
    ### generated at once, setting nsim to a larger
    ### number will result in a matrix of size 
    ### nval x nsim being returned, rather than 
    ### a vector of length nval
 distr="truncnorm"
    ### String specifying the distribution: "truncnorm" for
    ### truncated normal (for a regular normal, choose 
    ### "truncnorm" and set the lower and upper limits to -Inf
    ### and Inf respectively), "lognorm", or "exponential"
) {

    switch(distr,
           
           truncnorm = {
            
               require(msm) 

               markersim = rtnorm(nval*nsim, 
                                  mean=biodata[["mu"]], 
                                  sd=biodata[["sigma"]], 
                                  lower=biodata[["lower"]], 
                                  upper=biodata[["upper"]])
           },

           lognorm = {
               
               markersim = rlnorm(nval*nsim, 
                                  meanlog=biodata[["mu"]], 
                                  sdlog=biodata[["sigma"]])
           },

           exponential = {
        
               markersim = rexp(nval*nsim, 
                                rate=biodata[["lambda"]])
           }

    )
           
    # Now determine how the output should be structured,
    # based on input. This step exists because the input
    # is flexible--biodata can specify one distribution,
    # or it can have columns of parameters specifying
    # different distributions for each row
    structure = dim(biodata)[1]
    
    # Turn markersim into a matrix if the structure
    # and nsim indicate that:

    # Simple case: only one distribution
    if (is.null(structure) | structure==1) {
        if (nsim>1) {
            # If biodata had only one row, then all 
            # sims are generated from the same distribution
                markersim = matrix(markersim, ncol=nsim)
        }
    # Multiple distributions
    } else {
        if (nval!=nrow(biodata)) warning("In biomarker_trajectory, number of sims is greater than the number of parameters to simulate from; parameters will be recycled")

        # Create a matrix of results such that 
        # the sims from each parameter are organized
        # by row. We want this because each row may
        # have different distributional parameters
        markersim = matrix(markersim, nrow=nrow(biodata))
        if (nsim!=ncol(markersim)) stop("Error in biomarker_trajectory: improper use of function")
    }
    
    return(markersim)

### Simulated biomarker levels
}

############################################################    
## calc_age_at_threshold
############################################################
    
calc_age_at_threshold = function# Calculate age at which biomarker reaches threshold value

##description<< Uses the specified growth model to calculate age at which biomarker levels reach the given threshold

(age_at_threshold,
    ### Matrix to record ages at threshold
 marker_pre,
    ### Matrix of pre-onset marker values
 marker_post,
    ### Matrix of post-onset marker values
 age_at_onset,
    ### Matrix of ages at disease onset (preclinical incidence)
 age_at_inc,
    ### Matrix of ages at clinical incidence
 marker_method,
    ### "stepfxn" 
    ###     - marker jumps up to post-onset value 
    ###       immediately at onset
    ### "annual_exp" 
    ###     - apply an annual exponential growth between 
    ###       pre-onset and post-onset values, anchoring the 
    ###       post-onset value at clinical incidence
    ### "annual_linear" 
    ###     - apply an annual linear growth rate between
    ###       pre-onset and post-onset values, anchoring the
    ###       post-onset value at clinical incidence
 threshold
    ### Biomarker threshold value
 ) {
    
    # Sojourn time
    sojourn = age_at_inc - age_at_onset

    switch(marker_method,

           stepfxn = {
               
               # Determine when marker is over threshold
               over_threshold = marker_post>threshold
               
               # Turn non-cases and cases whose pre-onset
               # values were already over the threshold to 
               # FALSE
               over_threshold[is.na(over_threshold)] = FALSE
               over_threshold[!is.na(age_at_threshold)] = FALSE
               age_at_threshold[over_threshold] = 
                   age_at_onset[over_threshold]
               
           },
           
           annual_exp = {
 
               # Of form Y=C*exp(a*t)
 
               # Solve for a
               a = log(marker_post/marker_pre)/sojourn
 
               # Solve for t
               time_to_thresh = log(threshold/marker_pre)/a
 
               # Which entries should we add time_to_thresh 
               # to age at onset? Cases who haven't already
               # reached the threshold
               these_entries = a>0 & !is.na(a)
               age_at_threshold[these_entries] = 
                 age_at_onset[these_entries] + 
                 time_to_thresh[these_entries]
           },
 
           annual_linear = {
 
               # Of form Y = m*t + b
 
               # Define m
               m = (marker_post-marker_pre)/sojourn
 
               # Solve for t
               time_to_thresh = (threshold-marker_pre)/m 
 
               # Which entries should we add time_to_thresh 
               # to age at onset? Cases who haven't already
               # reached the threshold
               these_entries = m>0 & !is.na(m)
               age_at_threshold[these_entries] = 
                 age_at_onset[these_entries] + 
                 time_to_thresh[these_entries]
           })

    return(age_at_threshold)

### Matrix of ages at which biomarker threshold is reached
}


############################################################
## summarize_test_outcomes
############################################################

summarize_test_outcomes = function# Summarize test outcomes

##description<< Takes in a list of labeled test outcomes 
## (true and false positive and negative tests) and 
## returns summary statistics

(outcomes,
    ### Labeled list of outcomes. Required elements are
    ### TP, FN, TN, FP, and LT (lead time). Each element
    ### should be a list of matrix/data frames where columns
    ### are simulations and rows are individuals
 marker_ids,
    ### Vector of marker ID's of the same length as outcomes
 row_names=TRUE,
     ### Return outcomes as row names if TRUE, as a column if FALSE
 new_names=NULL
    ### Optional vector of new names for the rows of the result
)
{

    # determine length of each list
    nloops <- lapply(outcomes, length)[1]

    # calculate population-level results per sim
    pop_outcomes <- lapply(outcomes, 
                      function(o) {
                          o <- within(o, {
                                      TP_prob = TP/total_screens
                                      FN_prob = FN/total_screens
                                      TN_prob = TN/total_screens
                                      FP_prob = FP/total_screens
                                    })
                          sums <- sapply(o, colSums, na.rm=TRUE)
                          sums <- transform(sums,
                                            sensitivity=TP/(TP+FN),
                                            specificity=TN/(TN+FP),
                                            PPV=TP/(TP+FP),
                                            NPV=TN/(TN+FN))
                          means <- sapply(o, colMeans, na.rm=TRUE)
                          data.frame(subset(sums,
                                            select=c(TP,FN,FP,TN,
                                                     total_screens,
                                                     sensitivity,
                                                     specificity,
                                                     PPV,
                                                     NPV)),
                                     subset(means,
                                            select=c(age_true_pos,
                                                     lead_time,
                                                     TP_prob,
                                                     FN_prob,
                                                     FP_prob,
                                                     TN_prob)))
                      })

    # summarize across sims
    summary <- lapply(pop_outcomes,
                      summarize_sims,
                      ID='',
                      digits=3)

    if (!is.null(new_names)) summary <- lapply(summary,
                                              function(x) {
                                                  rownames(x) <- new_names 
                                                  return(x)
                                              })
    if (!row_names) summary <- lapply(summary, 
                                      function(x) {
                                          x=data.frame(Outcome=rownames(x),
                                                       x)
                                          rownames(x) <- NULL
                                          return(x)
                                      })
    return(summary)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# AREA (C) - SCREENING
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## shift_stage
############################################################

shift_stage = function# Implement stage shift

##description<< Given a vector with existing stages and
## information about the desired distribution within
## the 'shifted' population, implement a stage shift

(stages,
    ### Vector of stages, in the order denoted by ord
 start_pop,
    ### Vector in which each entry represents an individual
    ### and denotes the stage in which they started
 new_counts,
    ### Number of individuals to be assigned to each stage
    ### in the new, stage-shifted population
 ord
    ### Numeric vector denoting the order in which stage
    ### progression occurs, where 1=least invasive stage
    ### and max(ord)=most invasive stage. Individuals will
    ### be shifted to a stage that is lower than or equal
    ### to their starting stage
) {

    # Combine inputs and tabulate the number of individuals
    # assigned to each stage in the starting population.
    # Combining everything here is essential to keeping the
    # correct ordering for start_counts.
    indata = merge(data.frame(stages=stages,
                              new_counts=new_counts,
                              ord=ord),
                   data.frame(table(start_pop)),
                   by.x='stages',
                   by.y='start_pop')

    # Extract parameters now that everything has the correct
    # order
    stages = as.character(indata$stages)
    start_counts = as.numeric(indata$Freq)
    new_counts = as.numeric(indata$new_counts)
    ord = as.numeric(indata$ord)

    # Initialize a vector to receive stage assignments for
    # new population
    new_pop = rep(NA, length(start_pop))

    # Loop through stages, beginning with the most invasive
    # and working backward to the least invasive
    i = max(ord)
    while (i>=min(ord)) {

        # Randomly select appropriate number of individuals
        # to stay in stage i
        new_pop[sample(which(start_pop==stages[grep(i, ord)]),
                       new_counts[grep(i, ord)])] = stages[grep(i, ord)]

        # For those individuals in stage i who were not 
        # chosen to remain in stage i, temporarily reassign
        # to next less invasive stage (i-1)
        start_pop[start_pop==stages[grep(i, ord)] & is.na(new_pop)] = 
            stages[grep(i-1, ord)]

        # Correct counts to reflect above changes 
        start_counts[grep(i-1, ord)] = start_counts[grep(i-1, ord)] +
            (start_counts[grep(i, ord)]-new_counts[grep(i, ord)])
        start_counts[grep(i, ord)] = new_counts[grep(i, ord)]

        # Increment to next stage
        i = i-1
    }

    return(new_pop)

### A vector in which each entry represents an individual
### and denotes the stage to which they have been shifted
}


############################################################
## bootstrap_shifted_stage
############################################################

bootstrap_shifted_stage = function# Bootstrap a stage-shifted population for multiple simulations

##description<< For each simulation, implement a stage shift
## within a starting population, then boostrap rows to 
## sample

(stgs,
    ### Vector of stages, in the order denoted by ordr
 base_pop,
    ### Vector representing the starting population. Each 
    ### entry represents an individual and denotes the stage
    ### in which they start.
 new_cts,
    ### Number of individuals to be assigned to each stage
    ### in the new, stage-shifted population
 ordr,
    ### Numeric vector denoting the order in which stage
    ### progression occurs, where 1=least invasive stage
    ### and max(ordr)=most invasive stage. Individuals will
    ### be shifted to a stage that is lower than or equal
    ### to their starting stage.
 bootrows,
    ### Matrix/df of row indicators that can be applied to
    ### the data to recover different bootstraps of the
    ### data. Each column is a different bootstrap of the
    ### data.
 prefix="sim"
    ### Prefix for the names of each bootstrapped dataset
) {

    # Determine number of simulations for which to repeat
    nsims = ncol(bootrows)
    if (is.null(nsims)) nsims = ncol(bootrows[[1]])

    # Implement stage shift and return bootstrapped rows
    scr_stage = sapply(1:nsims,
                       function(x) {
                        
                           # Bootstrap sample
                           sim_pop = base_pop[bootrows[, x]]

                           # Implement stage shift
                           d = shift_stage(stages=stgs,
                                           start_pop=sim_pop,
                                           new_counts=new_cts,
                                           ord=ordr)
                           
                           # Identify index for shifted
                           # stage
                           i = sapply(d,
                                      function(y) {
                                          which(stgs==y)
                                       })

                           # Return bootstrapped rows
                           return(i)
                       })
    
    # Format and return
    colnames(scr_stage) = paste0(prefix, 1:nsims)
    rownames(scr_stage) = NULL
    return(scr_stage)

### A matrix of stages in which rows denote individuals and
### columns denote simulations
}



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# AREA (D) - DIAGNOSTICS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## rx_bootstrap
## rx_logit
############################################################

# Updated 4/23/12

# Methods to assign treatment assignment in the population

# Not quite like ttr_bootstrap but maybe that needs to be 
# updated too.

# Instead of bootstrapping from survival curves, bootstrap
# treatment assignment directly by generating nsim datasets.

rx_bootstrap = function# Bootstraps treatment from a dataset of treatment assignments

##description<< Takes a dataset on treatment assignment and
## bootstraps from it by covariate combinations to assign
## treatment in a population

##note<< This function really needs revamping. It's
## ridiculously specific to the exact usage, and that itself
## seems illogical!

(data_bootrows, 
    ### A data frame that has columns sim1...simX indicating
    ### rows for X bootstrapped datasets
rx_var="rx", 
    ### Variable that will be returned from the bootstrapped
    ### datasets
n_sim, 
    ### Number of bootstrapped datasets/simulations
prefix="sim"
    ### Prefix for the names of each bootstrapped dataset
) {

    # Get a matrix of just the rows
    rows = data_bootrows[, grep(prefix, colnames(data_bootrows))]

    # Now get a matrix of the sampled rx_var for each sim
    rxboot = sapply(1:nsim,
                    function(x) {
                        data_bootrows[, rx_var][rows[, x]]
                    })
    colnames(rxboot) = paste0(prefix, 1:nsim)
    
    return(data.frame(rxboot))

### Data frame of nsim treatment assignments, where the
### variable "sim" is the intermediate outcome, in the form
### of "testresult-binarytreatmentindicator"
}

rx_logit = function# Estimates the probability of receiving treatment conditional on specified covariates and simulates treatment assignment

##description<< Runs a logistic regression of treatment
## assignent on each bootstrapped dataset, predicts
## probability of treatment assignment in the population,
## and then simulates a treatment assignment for each person
## based on how the predicted probability compares to a draw
## from the uniform distribution

(data, 
    ### Data frame of treatment assignments and covariates, 
    ### along with bootstrapped row numbers for nsim
    ### datasets
 bootrows,
    ### Matrix/df with row indicators that can be applied to
    ### data to recover different bootstraps of edata. Each
    ### column is a different bootstrap (sim) of the data.
 rxvar='rx', 
    ### Treatment assignment variable
 covars, 
    ### Vector of covariate names
 isfactor
    ### A vector of TRUE/FALSE values that indicate whether
    ### each covariate should be considered a factor variable
) {

    # Specify the formula for fitting time 
   if (!is.null(isfactor))
        covars[isfactor] = paste0("as.factor(", covars[isfactor], ")")
    logit_formula = paste(rxvar, "~", paste(covars, collapse="+"))
    print("Model:")
    print(logit_formula)

    # Fit the model and simulate from the predicted 
    # probability FOR EACH BOOTSTRAPPED DATASET
    nsims = ncol(bootrows)
    if (is.null(nsims)) nsims = ncol(bootrows[[1]])

    rxs = sapply(1:nsims,
        function(x) {
            # Recreate the bootstrapped data
            if (!is.data.frame(data)) {
                simdata = build_simpop(datalist=data,
                                       rowlist=bootrows,
                                       sim=x)
            } else {
                simdata = data[bootrows[, x], ]
            }
            
            # Run model
            rx_model = glm(as.formula(logit_formula),
                           family=binomial,
                           data=simdata)
            
            # Compute the linear predictor for each person
            prob = predict(rx_model,
                           newdata=simdata,
                           type="response",
                           se.fit=FALSE)
                     
            # Simulate a treatment assignment based on the 
            # predicted prob. Use random draws from the 
            # uniform distribution and evaluate using the 
            # predicted probability as the cutoff
            draws = runif(length(prob))
            rxs = ifelse(draws<prob, 1, 0)

            return(rxs)
        })

    return(data.frame(rxs))

### Data frame of nsim treatment assignments, where the
### variable "sim" is the intermediate outcome, in the form
### of "testresult-binarytreatmentindicator"
} 


############################################################
## extract_treatment
############################################################

extract_treatment = function# Return treatment assignment from composite variable

##description<< Given a string variable of form
## "testresult-treatmentindicator", extract and return the
## treatment indicator

(matrix,
    ### Matrix of the string composite indicators
as.numeric=FALSE
    ### If the treatment indicator is numeric, return a
    ### numeric matrix?
) {
    # Extract the treatment info by using gsub to find the
    # hyphen and return the text/numbers after it
    rx = apply(matrix,
               2,
               gsub,
               pattern = "([[:alnum:]]*)(-)([[:alnum:]]*)", 
               replacement = "\\3")

    if (as.numeric) rx = apply(rx, 2, as.numeric)
    return(rx)

### A matrix of treatment indicators
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# AREA (E) - TREATMENT
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################
## calc_and_sim_KM
############################################################

calc_and_sim_KM = function# Estimates KM curves from a dataset and simulates from them

##description<< Estimates KM curves on each 
## bootstrapped dataset, and simulates a time to event
## for each individual

(data, 
    ### Data frame of times to event and (optionally) covariates
    ### Required variables are "time" and "status"
 bootrows,
    ### Matrix/df with row indicators that can be applied to
    ### data to recover different bootstraps of edata. Each
    ### column is a different bootstrap (sim) of the data.
 covars=1,
    ### 1 (no covariates) or vector of covariate names
 extrapolate=FALSE,
    ### If FALSE, will return NA for simulated values beyond 
    ### the maximum event time. If TRUE, requires the 
    ### remaining parameters to be specified
 extrapolation_distr=NULL,
    ### One of the survival regression distributions 
    ### (see help for survreg.distributions).
    ### Will be used to fit a parametric model to the data
    ### for extrapolating beyond the maximum event time
 extrapolation_years=NULL,
    ### Number of years, counting backwards from the 
    ### maximum event time, to use in modeling the parametric
    ### curve for extrapolating beyond the maximum event
    ### time
 prefix=NULL    
    ### Optional prefix for colnames of output
) {

    # Specify the formula for fitting time 
    surv_formula = paste0("Surv(time, status) ~",
                         paste(covars, collapse="+"))

    # Fit the model and simulate from the KM 
    # FOR EACH BOOTSTRAPPED DATASET
    nsims = ncol(bootrows)
    nppl = nrow(data)
    times = matrix(NA, nrow=nppl, ncol=nsims)

    get_times = sapply(1:nsims,
        function(x) {
            # Recreate the bootstrapped data
            simdata = data[bootrows[, x], ]
            
            # Define strata ID if we have covars
            # according to the survfit output style
            no_strata = ifelse(length(covars)>1,
                               FALSE,
                               ifelse(covars==1,
                                      TRUE, FALSE))
            if (!no_strata) {
                strata <- name_strata(data=simdata[,covars],
                                      cov=covars)
                strata_table <- table(strata)
                unique_strata <- names(table(strata))
            } else {
                strata <- rep(1, nrow(bootrows))
                unique_strata <- 1
                strata_table <- nrow(bootrows)
                names(strata_table) <- 1
            }

            # Run model
            km = survfit(as.formula(surv_formula), data=simdata)
            kms = summary(km)

            sim_times = sapply(unique_strata, 
                          
                function(st, k=kms, tab=strata_table, sim=x) {

                    this_stratum = (strata==st)

                    if (!no_strata) {
                        time=k$time[k$strata==st]
                        survival=k$surv[k$strata==st]
                    } else {
                        time=k$time
                        survival=k$surv
                    }
                    time = c(0, time)
                    survival = c(1, survival)

                    sims = sim_KM(survival=survival,
                                  time=time, 
                                  smalltimes=0,
                                  bigtimes=NA,
                                  nsims=tab[st])

                    times[this_stratum,sim] <<- sims

                    # Extrapolate for those simulations that
                    # fell beyond the max event time
                    if (extrapolate) {

                        # Define the subset of data on which to 
                        # fit a parametric model: 
                        # extrapolation_years before the last
                        # event time IN THIS STRATA
                        max_event_time = max(simdata$time[this_stratum])
                        extrap_time_cutoff = 
                            (max_event_time-extrapolation_years)
                        extrap_subset = simdata[this_stratum & 
                                                (simdata$time>
                                                 extrap_time_cutoff),]

                        # Set time=0 to be extrapolation_years before
                        # the last event time. This way, when we 
                        # simulate, we can interpret the result as 
                        # the amount of time to add to the last event time
                        extrap_subset = 
                            transform(extrap_subset, 
                                      time=time-extrap_time_cutoff)

                        # Run a survival regression model on this 
                        # stratum and fill in the NA's with a value
                        # simulated from this parametric model,
                        # conditional on surviving to max_event_time
                        parametric_model_sims = 
                            calc_and_sim_parametric(
                                    data=extrap_subset, 
                                    model_call=1,
                                    distribution=extrapolation_distr,
                                    bootrows=NULL,
                                    newdata=simdata,
                                    time_lower_bound=extrap_time_cutoff)

                        # Add those times to extrap_time_cutoff and 
                        # insert that as the simulated value for 
                        # all people whose simulated times to event
                        # fell beyond the max event time (denoted by
                        # NA times)
                        these_times = is.na(times[,x]) & this_stratum
                        times[these_times,x] <<- 
                            parametric_model_sims[these_times,1] + 
                            extrap_time_cutoff

                    } # end if (extrapolate)
            })
        })

    if (!is.null(prefix)) colnames(times) = paste0(prefix, 1:nsims)
    return(times)

### Matrix of times to event of the same dimensions as 
### bootrows
} 


############################################################
## calc_and_sim_Cox
############################################################

calc_and_sim_Cox = function# Estimates Cox models from a dataset and simulates from them

##description<< Estimates a Cox model on each 
## bootstrapped dataset, and simulates a time to event
## for each individual

(data, 
    ### Data frame of times to event and (optionally) covariates
    ### Required variables are "time" and "status"
 bootrows,
    ### Matrix/df with row indicators that can be applied to
    ### data to recover different bootstraps of edata. Each
    ### column is a different bootstrap (sim) of the data.
 model_call=1,
    ### The right hand side of a model formula, i.e. if your
    ### model is Surv(time,status)~rx+as.factor(surg)*age, 
    ### input 'rx+as.factor(surg)*age'. Enter a strata covariate
    ### in the next input, not here
 strata_covar=NULL,
    ### Name of covariate to use as a stratification variable. 
    ### Different baseline hazards are calculated for 
    ### each level of a stratification covariate
 extrapolate=FALSE,
    ### If FALSE, will return NA for simulated values beyond 
    ### the maximum event time. If TRUE, requires the 
    ### remaining parameters to be specified
 extrapolation_distr=NULL,
    ### One of the survival regression distributions 
    ### (see help for survreg.distributions).
    ### Will be used to fit a parametric model to the data
    ### for extrapolating beyond the maximum event time
 extrapolation_years=NULL,
    ### Number of years, counting backwards from the 
    ### maximum event time, to use in modeling the parametric
    ### curve for extrapolating beyond the maximum event
    ### time
 prefix=NULL    
    ### Optional prefix for colnames of output
) {

    # Dimensions
    nsims = ncol(bootrows)
    nppl = nrow(bootrows)

    # Specify the formula for fitting time 
    surv_formula = paste0("Surv(time, status) ~", model_call)
    if (!is.null(strata_covar)) {
        # Add strata variable to the model formula
        surv_formula = paste0(surv_formula,
                              "+strata(",
                              strata_covar,
                              ")")
    }

    # Fit the model and simulate from the KM 
    # FOR EACH BOOTSTRAPPED DATASET
    times = matrix(NA, nrow=nppl, ncol=nsims)
    draws = matrix(runif(nppl*nsims), 
                   nrow=nppl)

    get_times = sapply(1:nsims,
        function(x) {

            # Recreate the bootstrapped data
            simdata = data[bootrows[, x], ]
            # Identify strata
            if (!is.null(strata_covar)) {
                simdata_strata = name_strata(data=simdata[,strata_covar],
                                             cov=strata_covar)
                unique_strata = unique(simdata_strata)
            } else {
                simdata_strata = rep(1,nppl)
                unique_strata = 1
            }

            # Fit the model on this bootstrap/sim
            cox = coxph(as.formula(surv_formula), data=simdata)
            # Cumulative baseline hazard
            cumbase = basehaz(cox)
            if (is.null(strata_covar)) cumbase$strata=1
            # Linear predictors
            lin_pred = cox$linear.predictors
            # Unique linear predictors and counts
            unique_lin_pred = unique(lin_pred)


            # FOR CHECKING:
            if (1==0) {
                # Use survit.coxph to return each person's survival 
                # curve
                # scurve$surv (see below) is a matrix where columns
                # are people and rows are survival values for each 
                # of the times in scurve$time
                # Should I use Kaplan-Meier or the Nelson-Aaelen curve? Note
                # that scurve$surv has each person as a column and their
                # prediction for each event time as a row
                scurve = survfit(cox,
                                 newdata=data.frame(simdata),
                                 type="kaplan-meier",
                                 conf.type="none")
            }

            sim_times = sapply(unique_strata,
                function(s) {
                    # Indicator for this stratum
                    this_stratum = (simdata_strata==s)

                    # Within this stratum, for unique values of the 
                    # linear predictor, simulate
                    sim_lp =  sapply(unique_lin_pred, 
                                      
                            function(l, st=s, lp=lin_pred, cb=cumbase, sim=x) {

                                time = c(0,cb$time[cb$strata==st])
                                survival = c(1,exp(-(cb$hazard[cb$strata==st]*exp(l))))
                                this_subset = (lp==l & this_stratum)

                                sims = sim_KM(survival=survival,
                                              time=time, 
                                              smalltimes=0,
                                              bigtimes=NA,
                                              nsims=sum(lp==l),
                                              draws=draws[this_subset,sim])

                                times[this_subset,sim] <<- sims

                    }) # end sim_lp

                    # Extrapolate for those simulations that
                    # fell beyond the max event time
                    if (extrapolate) {

                        # Define the subset of data on which to 
                        # fit a parametric model: 
                        # extrapolation_years before the last
                        # event time
                        max_event_time = max(simdata$time[this_stratum])
                        extrap_time_cutoff = 
                            (max_event_time-extrapolation_years)
                        extrap_subset = simdata[this_stratum & 
                                                (simdata$time>
                                                 extrap_time_cutoff),]

                        # Set time=0 to be extrapolation_years before
                        # the last event time. This way, when we 
                        # simulate, we can interpret the result as 
                        # the amount of time to add to the last event time
                        extrap_subset = 
                            transform(extrap_subset, 
                                      time=time-extrap_time_cutoff)

                        # Run a survival regression model on this 
                        # stratum and predict non-NA values for
                        # the whole simdata
                        # conditional on surviving to max_event_time
                        parametric_model_sims = 
                            calc_and_sim_parametric(
                                    data=extrap_subset, 
                                    model_call=1,
                                    distribution=extrapolation_distr,
                                    bootrows=NULL,
                                    newdata=simdata,
                                    time_lower_bound=extrap_time_cutoff)

                        # Add those times to extrap_time_cutoff and 
                        # insert that as the simulated value for 
                        # all people whose simulated times to event
                        # fell beyond the max event time (denoted by
                        # NA times) in this stratum
                        these_times = is.na(times[,x]) & this_stratum
                        times[these_times,x] <<- 
                            parametric_model_sims[these_times,1] + 
                            extrap_time_cutoff

                    } # end if (extrapolate)

            }) # end sim_times (applied to unique strata)
        }) # end get_times (applied to data bootstraps)

    if (!is.null(prefix)) colnames(times) = paste0(prefix, 1:nsims)
    return(times)

### Matrix of times to event of the same dimensions as 
### bootrows
} 

############################################################
# sim_parametric_survreg
############################################################

sim_parametric_survreg = function# Simulate from a parametric survival model run using the survreg command

##description<< Given a survreg object and a vector or matrix of linear 
##predictors, returns random deviates from the distribution used
##to estimate the survreg model

(survreg_object,
    ### The survreg model object
 linear_predictor, 
    ### Vector or matrix of linear predictors. Note that these may 
    ### be derived from the data used to estimate the survreg object
    ### OR from a new dataset
 distribution,
    ### String with the name of the distribution used to estimate survreg
    ### Currently, only exponential and weibull are supported 
 n_deviates,
    ### Number of deviates to return
 time_lower_bound=0
    ### If a conditional simulation is desired, enter the lower 
    ### bound for time
 ) {

    # Define shape parameter
    switch(distribution,
        exponential = {

            weibull_shape=1

            #return(rexp(n, rate=1/exp(linear_predictor)))
        },
        weibull = {

            weibull_shape=1/survreg_object$scale

            #return(rweibull(n, scale=exp(linear_predictor), 
            #                   shape=1/survreg_object$scale))
        }
    )
        
    # Define scale parameter: same in both cases
    weibull_scale=exp(linear_predictor)

    # Define bound for simulation from F(t)
    sim_bound = pweibull(time_lower_bound, 
                         scale=weibull_scale,
                         shape=weibull_shape)

    # Random draws
    draws = runif(n_deviates, min=sim_bound, max=1)

    # Return simulated times
    return(qweibull(draws,
                    scale=weibull_scale,
                    shape=weibull_shape))
}


############################################################
# calc_and_sim_parametric
############################################################
calc_and_sim_parametric = function# Function to calculate a parametric survival model and then simulate from it

##description<< Estimates a parametric survival model and then returns random 
#deviates, taking into account covariates. Simulated values may be generated
#for a new dataset instead of the data on which the model was estimated

(data,
    ### Data frame on which to fit the model
distribution,
    ### One of the survreg.distributions (see corresponding help file)
model_call=1,
    ### The right hand side of a model formula, i.e. if your
    ### model is Surv(time,status)~rx+as.factor(surg)*age, 
    ### input 'rx+as.factor(surg)*age'
bootrows=NULL,
    ### Optional matrix of row indicators defining bootstrapped samples
    ### of the data. Leave as null if you want to run the model
    ### only once, rather than on multiple bootstrapped
    ### samples of the data
newdata=NULL,
    ### Optional data frame for which simulated values will 
    ### be returned. If not null, the process will be
    ### 1) Model is fit to 'data' (or a bootstrapped sample of 'data')
    ### 2) 'newdata' is used to define the value of the linear
    ### predictor for each person
    ### 3) A time to event is simulated for each person using the
    ### appropriate linear predictor
 prefix=NULL,
    ### Optional prefix for colnames of output
 time_lower_bound=0
    ### Optional lower bound for simulated times to event, 
    ### i.e. you may simulate from a conditional distribution, 
    ### conditional on surviving to time_lower_bound
) {

    # Adapt bootrows if it's NA
    if (is.null(bootrows)) bootrows=matrix(1:nrow(data),ncol=1,nrow=nrow(data))

    # Dimensions, and adapt newdata
    nsims = ncol(bootrows)
    if (is.null(newdata)) newdata = data
    nppl = nrow(newdata)
    

    # Specify the formula for fitting time 
    surv_formula = paste0("Surv(time, status) ~", model_call)

    # Fit the model and simulate 
    # FOR EACH BOOTSTRAPPED DATASET
    times = matrix(NA, nrow=nppl, ncol=nsims)
    draws = matrix(runif(nppl*nsims), 
                   nrow=nppl)

    sim_times = sapply(1:ncol(bootrows),
        function(x) {

            # Recreate the bootstrapped data
            simdata = data[bootrows[, x], ]

            # Run the model
            surv_model = survreg(as.formula(surv_formula),
                                 dist=distribution,
                                 data=simdata)

            # Compute linear predictor
            lp = predict(surv_model,
                         newdata=newdata,
                         type="linear",
                         se=FALSE)

            sim_times = sapply(unique(lp),
                function(l) {
                    times[lp==l,x] <<-
                        sim_parametric_survreg(
                            survreg_object=surv_model, 
                            linear_predictor=l, 
                            distribution=distribution, 
                            n_deviates=sum(lp==l),
                            time_lower_bound=time_lower_bound)
            })

            # Check result
            if (1==0) {
               sim_ecdf <- summarize_ecdf(
                                data=sim_times, 
                                xvalues=seq(0, 10, 0.1), 
                                covar=rep(1, nrow(sim_times)))
               pdf('output/net_recur_free_survival_extrap.pdf')
               baseplot_timetoevent(data=simdata,
                                    distribution=NULL,
                                    group_var=NULL,
                                    time_max=10,
                                    empirical_curve=sim_ecdf$summ)
               dev.off()
            }

    })

    if (!is.null(prefix)) colnames(times) = paste0(prefix, 1:ncol(times))
    return(times)

### Simulated times to event in the following form:
### 1) If newdata is NULL and bootrows is not, 
###     a matrix of same dimension as bootrows
### 2) If newdata and bootrows are NULL, a nrow(data)x1 matrix
### 3) If newdata is not NULL, a nrow(newdata)x1 matrix
}

    
