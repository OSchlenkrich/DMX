# Functions for Construction Dataset

# Efficient Source Function to hinder repeating of Loading and Creating of Datasets
source_c = function(source_string, check, force = F) {
  if ((check %in% objects(name=globalenv())) == F | (force == T)) {
    source(source_string)
  } else {
    print(paste(check, ": Already Loaded."))
  }
}


# ECDF (Student T) to scale measurement model outcome to 0 and 1
set.seed(1234)
cdf_trans <- ecdf(rt(n=100000,df=6)) 

cdf = function(x) {
  x = cdf_trans(x)
  x = minmax(x, 1)
  return(x)
}


# MinMax
minmax <- function(data, scale) {
  return((data-min(data, na.rm=T))/(max(data, na.rm=T)-min(data, na.rm=T))*scale)
}

# z-Standardisation
scale_fun = function(x) {
  return(scale(x, scale=T)[,1])
}

# get minimum values
min_fun = function(x) {
  if (all(is.na(x))==F) {
    min_value = min(x, na.rm=T)
  } else {
    min_value = NA
  }
  return(min_value)
}

# Function for Repeating Values between Elections 
fill_elections = function(x, v2x_elecreg) {
  loadedvalue <- NA
  for (ii in 1:length(x)) {
    if (is.na(v2x_elecreg[ii]) == F) {
      if (v2x_elecreg[ii] == 1) {
        if (is.na(x[ii]) == F) {
          loadedvalue <- x[ii]
        }
        x[ii] <- loadedvalue      }
    }
  } 
  return(x)
}

# Opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# Function for Repeating Values between Elections with reference to v2elfrfair (only for PD_C)
fill_elections_w_ref = function(x, v2x_elecreg, v2elfrfair) {
  loadedvalue <- NA
  for (ii in 1:length(x)) {
    if (is.na(v2x_elecreg[ii]) == F) {
      if (v2x_elecreg[ii] == 1) {
        if (is.na(x[ii]) == F) {
          loadedvalue <- x[ii]
        }
        
        if (is.na(x[ii]) == T & is.na(v2elfrfair[ii]) == F) {
          loadedvalue = NA
        }
        
        x[ii] <- loadedvalue      
      }
    }
  } 
  return(x)
}

# Ordered Column Names

ordered_column_names = function(data) {
  var_base = c("country_name","year",names(data)[grep("^v2", names(data))])
  var_nr = names(data)[grep("_nr", names(data))]
  var_base = var_base[which(var_base %!in% var_nr)] # remove _nr from var_base
  var_facto = names(data)[grep("_facto$", names(data))]
  var_core = names(data)[grep("_core$", names(data))]
  var_base_renamed = names(data)[which(names(data) %!in% c(var_base, var_nr, var_facto, var_core))]
  
  return(c(var_base, var_base_renamed, var_facto, var_core, var_nr))
}


# Classification
classification = function(dataset, measurement_level) {
  
  regimeclass_diminst = dataset %>%
    select_at(vars(ends_with(measurement_level))) %>%
    select_at(vars(matches("inst"), matches("dim"), -matches("total"))) %>% 
    mutate_all(function(x) round(x, 2))
  
  temp_class = factor(levels=c("Autocracy", "Hybrid Regime", "Deficient Democracy", "Working Democracy"))
  for (i in 1:dim(regimeclass_diminst)[1]) {
    if (all(is.na(regimeclass_diminst[i, ]) == F)) {
      temp_class[i] = ifelse(all(regimeclass_diminst[i, ] < 0.5), "Autocracy", 
                                       ifelse(all(regimeclass_diminst[i, ] >= 0.75), "Working Democracy", 
                                              ifelse(all(regimeclass_diminst[i, ] >= 0.5), "Deficient Democracy", 
                                                     ifelse(length(which(regimeclass_diminst[i, ] >= 0.5)) >=2, "Hybrid Regime", "Autocracy"))))     
    } else {
      temp_class[i] = NA
    }
  }
  return(temp_class)
}


