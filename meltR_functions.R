library(tidyverse)
library(readxl)
library(ggsci)
library(ggthemes)

#raw data reader----

melting.readR <- function(input.file = 'data/input.xlsx',
                          input.sheet = 'UV-melting',
                          input.range = 'AK8:AM158'){

  raw.abs <- read_xlsx(input.file,
                       sheet = input.sheet,
                       range = input.range)

  raw.abs <- raw.abs %>%
    mutate(id = 1:nrow(raw.abs),
           abs = Absorbance-Blank, #blank correction
           temp = Temperature + 273.15) %>%
    mutate(ramp = if_else(
      id < max(id),
      #assigns ramp based on following point except  for last point
      if_else(lead(temp) < temp,'cooling','heating'
      ),#assigns the ramp to last point based on previous point value
      if_else(lag(temp) < temp, 'heating', 'cooling')
    )
    ) %>%
    select(temp, abs, ramp) #select relevant columns

}

#raw data plottR----
melting.plottR <- function(input.raw = raw.dk33,
                           start.low = c(277,298),
                           end.low = c(282,303),
                           start.high = c(348,360),
                           end.high = c(353,365)){

  p.raw.abs <- input.raw %>%
    ggplot(aes(x=temp, y=abs, color = ramp)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_vline(aes(xintercept = start.low[1]),
               color = 'springgreen4',
               size = 1) +
    geom_vline(aes(xintercept = start.low[2]),
               linetype = 'dashed',
               color = 'springgreen4',
               size = 1) +
    geom_vline(aes(xintercept = end.low[1]),
               linetype = 'dashed',
               color = 'purple4',
               size = 1) +
    geom_vline(aes(xintercept = end.low[2]),
               color = 'purple4',
               size = 1) +
    geom_vline(aes(xintercept = start.high[1]),
               color = 'springgreen4',
               size = 1) +
    geom_vline(aes(xintercept = start.high[2]),
               linetype = 'dashed',
               color = 'springgreen4',
               size = 1) +
    geom_vline(aes(xintercept = end.high[1]),
               linetype = 'dashed',
               color = 'purple4',
               size = 1) +
    geom_vline(aes(xintercept = end.high[2]),
               color = 'purple4',
               size = 1) +
    theme_pander() +
    scale_color_d3() +
    labs(x = "T (K)", y = 'A')
  p.raw.abs
}

#baseline range generator----

#generates all sets of baselines from four ranges of integers defining the
#start and end of low- and high-temp. baselines, having a minimal range of 5°C,
#then randomly samples some

basegenR <- function(start.low = c(275,290),
                     end.low = c(290,305),
                     start.high = c(340,355),
                     end.high = c(345,355),
                     t.range.low = 5,
                     t.range.high = 5,
                     nb.spl = 'max'){

  #determines the largest temperature range
  max.lg <- max(length(start.low[1]:start.low[2]),
                length(end.low[1]:end.low[2]),
                length(start.high[1]:start.high[2]),
                length(end.high[1]:end.high[2]))

  #Creates dataframe with all columns of same length by increasing ranges with extra values
  df <- data.frame(
    min.low = start.low[1]:(start.low[1]+max.lg-1),
    max.low = end.low[1]:(end.low[1]+max.lg-1),
    min.high = start.high[1]:(start.high[1]+max.lg-1),
    max.high = end.high[1]:(end.high[1]+max.lg-1)
  ) %>% #creates all combinations
    expand(., min.low, max.low, min.high, max.high) %>%
    filter( #removes extra values
      max.low <= max(end.low),
      max.high <= max(end.high)
    ) %>%
    #calculates temperature ranges
    mutate(range.low = max.low-min.low,
           range.high = max.high - min.high) %>%
    #discards baselines with ranges below t.range
    filter(range.low >= t.range.low,
           range.high >= t.range.high) %>%
    select(-c(range.low, range.high)) #discards useless columns to save memory

  if(nb.spl == 'max'){
    df <- df
  } else {
    df <- df %>%
      sample_n(size = nb.spl, replace = TRUE) #samples random rows
  }


}
#regresseR----

regresseR <- function(input.data, ramps, min.temp, max.temp){

  #function to perform linear regression on a given temperature range
  #from a selected ramp

  #subset raw data as indicated by function variables
  input.data <- input.data %>%
    filter(temp > min.temp,
           temp < max.temp,
           ramps == ramp)

  #linear regression on subet data
  lm.data <- lm(data = input.data,
                formula = abs ~ temp)

  #extracts linear regression coefficients
  output.data <- lm.data$coefficients

  return(output.data)
}

#tm.interpolatR----

tm.interpolatR <- function(raw.input, theta.base.temp = 20,
                           min.low.cool, max.low.cool,
                           min.high.cool, max.high.cool,
                           min.low.heat, max.low.heat,
                           min.high.heat, max.high.heat){

  #Calculates the baselines over the full temperature range from the linear
  #regression coefficients


  if (length(unique(raw.input$ramp))==2) {

    bases <- raw.input %>%
    mutate(
      #calculates low temp baselines
      low.bl = if_else(
        ramp == 'cooling', #works on ramps independently
        regresseR(raw.input, 'cooling', min.low.cool, max.low.cool)[1]
        +regresseR(raw.input, 'cooling', min.low.cool, max.low.cool)[2]*temp,
        regresseR(raw.input, 'heating', min.low.heat, max.low.heat)[1]
        +regresseR(raw.input, 'heating', min.low.heat, max.low.heat)[2]*temp),
      #calculates high temp baselines
      high.bl = if_else(
        ramp == 'cooling', #works on ramps independently
        regresseR(raw.input, 'cooling', min.high.cool, max.high.cool)[1]
        +regresseR(raw.input, 'cooling', min.high.cool, max.high.cool)[2]*temp,
        regresseR(raw.input, 'heating', min.high.heat, max.high.heat)[1]
        +regresseR(raw.input, 'heating', min.high.heat, max.high.heat)[2]*temp),
      #adds variable to identify the baselines from their temp ranges
      min.low.cool = min.low.cool,
      max.low.cool = max.low.cool,
      min.high.cool = min.high.cool,
      max.high.cool = max.high.cool,
      min.low.heat = min.low.heat,
      max.low.heat = max.low.heat,
      min.high.heat = min.high.heat,
      max.high.heat = max.high.heat,
      #creates a baseline id from variables above
      id = if_else(
        ramp == 'cooling',
        paste(min.low.cool, max.low.cool, min.high.cool, max.high.cool, sep = '/'),
        paste(min.low.heat, max.low.heat, min.high.heat, max.high.heat, sep = '/'))
    ) %>%
    group_by(id) %>%
    mutate( #calculates baseline median
      med.bl = (low.bl + high.bl)/2,) %>%
    group_by(ramp, temp, id) %>%
    #calculates folded fraction
    mutate(theta = (high.bl - abs)/(high.bl - low.bl)) %>%
    ungroup()

  } else {

    bases <- raw.input %>%
      mutate(ramp = 'single') %>%
      mutate(
        #calculates low temp baselines
        low.bl = regresseR(raw.input, 'single', min.low.cool, max.low.cool)[1]
          +regresseR(raw.input, 'single', min.low.cool, max.low.cool)[2]*temp,
        #calculates high temp baselines
        high.bl = regresseR(raw.input, 'single', min.high.cool, max.high.cool)[1]
          +regresseR(raw.input, 'single', min.high.cool, max.high.cool)[2]*temp,
        #adds variable to identify the baselines from their temp ranges
        min.low.cool = min.low.cool,
        max.low.cool = max.low.cool,
        min.high.cool = min.high.cool,
        max.high.cool = max.high.cool,
        #creates a baseline id from variables above
        id = paste(min.low.cool, max.low.cool, min.high.cool, max.high.cool, sep = '/')
      ) %>%
      group_by(id) %>%
      mutate( #calculates baseline median
        med.bl = (low.bl + high.bl)/2,) %>%
      group_by(ramp, temp, id) %>%
      #calculates folded fraction
      mutate(theta = (high.bl - abs)/(high.bl - low.bl)) %>%
      ungroup()
  }



  thetas <- bases  %>%
    #discard irrelevant columns to save memory
    select(temp, theta, ramp, id) %>%
    group_by(ramp, id) %>%
    #interpolates data and find the tm at theta = 0.5
    summarise(tm = approxfun(x = theta[temp>290], y = temp[temp>290])(0.5),
              theta.t = approxfun(x = temp, y = theta)(theta.base.temp+273.15))

  return(list(bases = bases, thetas = thetas))

}



#tm.extractoR----

tm.extractoR <- function(abs.data, temp.df, theta.base.temp = 20){

  temp.df <- temp.df
  abs.data <- abs.data

  for (i in 1:nrow(temp.df)) {
    if (i == 1) {
      tm <- tm.interpolatR(abs.data, theta.base.temp,
                           temp.df$min.low[i],  temp.df$max.low[i],
                           temp.df$min.high[i],  temp.df$max.high[i],
                           temp.df$min.low[i],  temp.df$max.low[i],
                           temp.df$min.high[i],  temp.df$max.high[i]
      )$thetas
    } else {
      tm <- tm %>%
        rbind(tm.interpolatR(abs.data, theta.base.temp,
                             temp.df$min.low[i], temp.df$max.low[i],
                             temp.df$min.high[i],  temp.df$max.high[i],
                             temp.df$min.low[i],  temp.df$max.low[i],
                             temp.df$min.high[i],  temp.df$max.high[i]
        )$thetas
        )
    }
  }

  results <- tm

}

base.plottR <- function(abs.data, temp.df, theta.base.temp = 20){

  temp.df <- temp.df
  abs.data <- abs.data

  for (i in 1:nrow(temp.df)) {
    if (i == 1) {
      bases <- tm.interpolatR(abs.data, theta.base.temp,
                              temp.df$min.low[i],  temp.df$max.low[i],
                              temp.df$min.high[i],  temp.df$max.high[i],
                              temp.df$min.low[i],  temp.df$max.low[i],
                              temp.df$min.high[i],  temp.df$max.high[i]
      )$bases
    } else {
      bases <- bases %>%
        rbind(tm.interpolatR(abs.data, theta.base.temp,
                             temp.df$min.low[i], temp.df$max.low[i],
                             temp.df$min.high[i],  temp.df$max.high[i],
                             temp.df$min.low[i],  temp.df$max.low[i],
                             temp.df$min.high[i],  temp.df$max.high[i]
        )$bases
        )
    }
  }

  results <- bases

}

p.base.plottR <- function(input.results, replicates, input.oligo){
  input.results %>%
    filter(oligo == input.oligo,
           rep %in% replicates) %>%
    ggplot(aes(x = temp)) +
    geom_point(aes(y = abs),
               color = 'grey',
               show.legend = FALSE) +
    geom_line(aes(y = med.bl, color = id.bl, group = factor(id.bl)),
              linetype = 'dashed',
              show.legend = TRUE) +
    geom_line(aes(y = low.bl, color = id.bl, group = (id.bl)),
              show.legend = FALSE) +
    geom_line(aes(y = high.bl, color = id.bl, group = (id.bl)),
              show.legend = FALSE) +
    facet_grid(rep~ramp) +
    theme_pander() +
    scale_color_viridis_c(name = bquote(Delta*A)) +
    labs(x = "T (K)", y = 'A')
}


p.fraction.plotR <- function(input.results, replicates, input.oligo){
  input.results %>%
    # group_by(ramp, rep) %>%
    # mutate(mean.ramp.theta = mean(theta)) %>%
    # group_by(ramp, rep, id) %>%
    # mutate(id.bl = mean(theta)/mean.ramp.theta) %>%
    filter(oligo == input.oligo,
           rep %in% replicates) %>%
    ggplot(aes(x = temp)) +
    geom_point(aes(y = theta, color = id.bl),
               show.legend = FALSE) +
    facet_grid(rep~ramp
               # scales = 'free_y',
               ) +
    theme_pander() +
    scale_color_viridis_c(name = bquote(Delta*A)) +
    labs(x = "T (K)", y = bquote(theta)) +
    scale_y_continuous(limits = c(0,1))
}


#data summary----
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}


#Savitzky-Golay function----
sav <- function(input, diff.order, poly.order, win.size){
  input %>%
    #adapts number of row to the smoothing output
    slice_min(n = nrow(input)-(win.size-1)/2, order_by = T.K) %>% #removes coldest temp
    slice_max(n = nrow(input)-(win.size-1), order_by = T.K) %>% #removes hottest temp
    mutate(emp = abs(#absolute value
      prospectr::savitzkyGolay(#Savitzky-Golay smooth
        X = input$abs.melt, #input is absorbance
        m = diff.order, #differential order
        p = poly.order, #polynomilal order
        w = win.size #window size
      )
    ))
}
