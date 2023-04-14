# script uploaded on https://github.com/ndelsol/colonial_ranching_ecology.git


########################### Extraction of paleoenvironmental data from PHYDA
library(stars)
library(ncmeta)



temp_DF <- read_ncdf("da_hydro_DecFeb.nc", var = "tas_mn") #import phyda file and get the temperature variable
pts <- read_sf("sites_updated.shp") # import sample points
data <- st_extract(temp_DF, pts) # extract temperature data at sample points
write.csv(data,"tas_data_DF.csv", row.names = FALSE)


temp_JA <- read_ncdf("da_hydro_JunAug.nc", var = "tas_mn") #import phyda file and get the temperature variable
pts <- read_sf("sites_updated.shp") # import sample points
data <- st_extract(temp_JA, pts) # extract temperature data at sample points
write.csv(data,"tas_data_JA.csv", row.names = FALSE)

temp_AM <- read_ncdf("da_hydro_AprMar.nc", var = "tas_mn") #import phyda file and get the temperature variable
pts <- read_sf("sites_updated.shp") # import sample points
data <- st_extract(temp_AM, pts) # extract temperature data at sample points
write.csv(data,"tas_data_AM.csv", row.names = FALSE)



spei_DF <- read_ncdf("da_hydro_DecFeb.nc", var = "spei_mn") #import phyda file and get the spei variable
pts <- read_sf("sites_updated.shp") # import sample points
data <- st_extract(spei_DF, pts) # extract spei data at sample points
write.csv(data,"spei_data_DF.csv", row.names = FALSE)

spei_JA <- read_ncdf("da_hydro_JunAug.nc", var = "spei_mn") #import phyda file and get the spei variable
pts <- read_sf("sites_updated.shp") # import sample points
data <- st_extract(spei_JA, pts) # extract spei data at sample points
write.csv(data,"spei_data_JA.csv", row.names = FALSE)

spei_AM <- read_ncdf("da_hydro_AprMar_r.1-2000_d.05-Jan-2018.nc", var = "spei_mn") #import phyda file and get the spei variable
pts <- read_sf("sites_updated.shp") # import sample points
data <- st_extract(spei_AM, pts) # extract spei data at sample points
write.csv(data,"spei_data_AM.csv", row.names = FALSE)


################################ Extraction of modern climatic data from CHELSA

library(terra)
library(magrittr)

# Set working directory
setwd("C:/Users/Nicolas/OneDrive - University of Florida/Postdoc/Zooarch Beyond Subsistence_chapter/Chelsa")

# Load raster data
bio1 <- rast("CHELSA_bio1_1981-2010_V.2.1.tif")
bio12 <- rast("CHELSA_bio12_1981-2010_V.2.1.tif")
bio5 <- rast("CHELSA_bio5_1981-2010_V.2.1.tif")
bio6 <- rast("CHELSA_bio6_1981-2010_V.2.1.tif")
ai <- rast("CHELSA_ai_1981-2010_V.2.1.tif")

# Define sites of interest
sites <- list(
  ann = ext(-76.5275, -76.5255, 38.9385, 38.9409),
  ant = ext(-90.7349, -90.7329, 14.5587, 14.5607),
  cha = ext(-79.9462, -79.9442, 32.8772, 32.8792),
  esp = ext(-2.5313,-2.5293, 43.1666, 43.1686),
  flo = ext(-81.3158, -81.3138, 29.9198, 29.9218),
  mer = ext(-89.6222, -89.6202, 20.9643, 20.9663),
  mex = ext(-99.1352, -99.1332, 19.4327, 19.4347),
  pre = ext(-72.1089, -72.1069, 19.6959, 19.6979),
  que = ext(-73.5879, -73.5859, 45.6071, 45.6091)
  
  ############################## Visualization of paleoclimatic datasets (temperatures and SPEI)
  
  library(dplyr)
  library(ggplot2)
  
  # Set the working directory
  setwd("C:/Users/Nicolas/OneDrive - University of Florida/Postdoc/ESA 2022")
  
  # Create a vector of file names
  file_names <- c("ann_temp.csv", "ant_temp.csv", "cha_temp.csv", "esp_temp.csv", "flo_temp.csv", "mer_temp.csv", "mex_temp.csv", "pre_temp.csv", "que_temp.csv")
  
  # Create an empty data frame to store the mean temperatures and standard deviations
  all_mean_temps <- data.frame(year = numeric(0), mean_temp = numeric(0), site = character(0), sd_temp = numeric(0))
  
  # Loop through each file and calculate the mean temperature and standard deviation for each year
  for (file_name in file_names) {
    
    # Read in the data
    data <- read.csv(file_name)
    
    # Calculate the mean temperature and standard deviation for each year
    mean_temps <- data %>%
      group_by(year) %>%
      summarise(mean_temp = mean(c(winter, spring, summer), na.rm = TRUE),
                sd_temp = sd(c(winter, spring, summer), na.rm = TRUE)) %>%
      ungroup()
    
    
    
    # Add a column with the site name
    site_name <- gsub("_temp.csv", "", file_name)
    # Rename the site names
    if (site_name == "ann") {
      site_name <- "Annapolis"
    } else if (site_name == "ant") {
      site_name <- "Antigua Guatemala"
    } else if (site_name == "cha") {
      site_name <- "Charleston"
    } else if (site_name == "esp") {
      site_name <- "Spain"
    } else if (site_name == "flo") {
      site_name <- "Florida"
    } else if (site_name == "mer") {
      site_name <- "Merida"
    } else if (site_name == "mex") {
      site_name <- "Mexico"
    } else if (site_name == "pre") {
      site_name <- "Puerto Real"
    } else if (site_name == "que") {
      site_name <- "Quebec"
    }
    
    mean_temps$site <- rep(site_name, nrow(mean_temps))
    
    # Add the mean temperatures and standard deviations to the all_mean_temps data frame
    all_mean_temps <- rbind(all_mean_temps, mean_temps)
  }
  
  # Plot the mean temperatures and standard deviations for all sites on the same graph
  ggplot(all_mean_temps, aes(x = year, y = mean_temp, color = site, fill = site)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), alpha = 0.05) +
    labs(title = "Average temperatures per year", x = "Year", y = "Temperature (C)", color = "Site", fill = "Site") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  ############ Make the graph for the SPEI ##############
  
  # Create a vector of file names
  file_names_spei <- c("ann_spei.csv", "ant_spei.csv", "cha_spei.csv", "esp_spei.csv", "flo_spei.csv", "mer_spei.csv", "mex_spei.csv", "pre_spei.csv", "que_spei.csv")
  
  # Create an empty data frame to store the mean spei and standard deviations
  all_mean_spei <- data.frame(year = numeric(0), mean_spei = numeric(0), site = character(0), sd_spei = numeric(0))
  
  # Loop through each file and calculate the mean temperature and standard deviation for each year
  for (file_name in file_names_spei) {
    
    # Read in the data
    data_spei <- read.csv(file_name)
    
    # Calculate the mean temperature and standard deviation for each year
    mean_spei <- data_spei %>%
      group_by(year) %>%
      summarise(mean_spei = mean(c(winter, spring, summer), na.rm = TRUE),
                sd_spei = sd(c(winter, spring, summer), na.rm = TRUE)) %>%
      ungroup()
    
    
    
    # Add a column with the site name
    site_name <- gsub("_spei.csv", "", file_name)
    # Rename the site names
    if (site_name == "ann") {
      site_name <- "Annapolis"
    } else if (site_name == "ant") {
      site_name <- "Antigua Guatemala"
    } else if (site_name == "cha") {
      site_name <- "Charleston"
    } else if (site_name == "esp") {
      site_name <- "Spain"
    } else if (site_name == "flo") {
      site_name <- "Florida"
    } else if (site_name == "mer") {
      site_name <- "Merida"
    } else if (site_name == "mex") {
      site_name <- "Mexico"
    } else if (site_name == "pre") {
      site_name <- "Puerto Real"
    } else if (site_name == "que") {
      site_name <- "Quebec"
    }
    
    mean_spei$site <- rep(site_name, nrow(mean_spei))
    
    # Add the mean temperatures and standard deviations to the all_mean_temps data frame
    all_mean_spei <- rbind(all_mean_spei, mean_spei)
  }
  
  # Plot the mean temperatures and standard deviations for all sites on the same graph
  ggplot(all_mean_spei, aes(x = year, y = mean_spei, color = site, fill = site)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = mean_spei - sd_spei, ymax = mean_spei + sd_spei), alpha = 0.05) +
    labs(title = "Average SPEI per year", x = "Year", y = "SPEI", color = "Site", fill = "Site") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  ################# Mosaic of the SPEI graphs ##########
  
  library(ggplot2)
  
  # Create a vector of file names
  file_names_spei <- c("ann_spei.csv", "ant_spei.csv", "cha_spei.csv", "esp_spei.csv", "flo_spei.csv", "mer_spei.csv", "mex_spei.csv", "pre_spei.csv", "que_spei.csv")
  
  # Create a list to store the ggplot objects
  ggplot_list <- list()
  
  # Loop through each file and create a ggplot object for each site
  for (file_name in file_names_spei) {
    
    # Read in the data
    data_spei <- read.csv(file_name)
    
    # Calculate the mean temperature and standard deviation for each year
    mean_spei <- data_spei %>%
      group_by(year) %>%
      summarise(mean_spei = mean(c(winter, spring, summer), na.rm = TRUE),
                sd_spei = sd(c(winter, spring, summer), na.rm = TRUE)) %>%
      ungroup()
    
    # Add a column with the site name
    site_name <- gsub("_spei.csv", "", file_name)
    
    # Rename the site names
    if (site_name == "ann") {
      site_name <- "Annapolis"
    } else if (site_name == "ant") {
      site_name <- "Antigua Guatemala"
    } else if (site_name == "cha") {
      site_name <- "Charleston"
    } else if (site_name == "esp") {
      site_name <- "Spain"
    } else if (site_name == "flo") {
      site_name <- "Florida"
    } else if (site_name == "mer") {
      site_name <- "Merida"
    } else if (site_name == "mex") {
      site_name <- "Mexico"
    } else if (site_name == "pre") {
      site_name <- "Puerto Real"
    } else if (site_name == "que") {
      site_name <- "Quebec"
    }
    
    # Create a ggplot object for the site
    ggplot_obj <- ggplot(mean_spei, aes(x = year, y = mean_spei)) +
      geom_line(color = "deeppink1") +
      geom_ribbon(aes(ymin = mean_spei - sd_spei, ymax = mean_spei + sd_spei), alpha = 0.05, fill = "red") +
      labs(title = site_name, x = "Year", y = "SPEI")
    
    # Add the ggplot object to the list
    ggplot_list[[site_name]] <- ggplot_obj
  }
  
  # Plot the ggplot objects as a "mosaic" of 3 rows and 3 columns
  gridExtra::grid.arrange(grobs = ggplot_list, nrow = 3, ncol = 3)
  
  
  library(Matrix)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(car)
  library(RVAideMemoire)
  library(DHARMa)
  library(MuMIn)
  library(tidyverse)
  library(glmmTMB)
  library(vegan)
  library(hillR)
  library(ape)
  library(agridat)
  library(ggplot2)
  library(dplyr)
  
  setwd("C:/Users/Nicolas/OneDrive - University of Florida/Postdoc/ESA 2022")
  
  data1 <-read_csv("bostaurus_original.csv")
  data2 <-read_csv("bostaurus2.csv")
  
  head(data1)
  head(data2)
  
  
  # Histograms
  hist(data1$si)
  data_si=data.frame(x = data1$si)
  ggplot(data_si, aes(x=x)) +
    geom_histogram(binwidth = 0.005, fill="lightblue", color="blue3", alpha=0.75) +
    stat_function(fun = dnorm, args = list(mean = mean(data_si$x), sd = sd(data_si$x)), 
                  color="purple", size = 0.8) +
    labs(x="Size Index", y="Frequency") +
    theme(plot.title = element_text(face="bold", size=20),
          axis.title = element_text(face="bold", size=12),
          axis.text = element_text(size=12))
  
  data_si2=data.frame(x = data2$si)
  histo_bos <- ggplot(data_si, aes(x=x)) + 
    geom_histogram(binwidth = 0.01)
  print(histo_bos)
  
  # Check the structure of the data
  str(data2$si)
  
  # Perform ANOVA
  anova_result <- aov(si~site, data2)
  
  # Print ANOVA table
  summary(anova_result)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Print test result
  tukey_result
  
  #first we make a boxplot just to check
  #par(mfrow = c(1, 4)) #puts the boxplot on a figure
  boxplot(si~clim_temp, data=data1, col="magenta")
  boxplot(si~clim_prec, data=data1, col="cyan")
  
  boxplot(si~country, data=data1, col="brown")
  boxplot(si~phase, data=data1, col="blue")
  boxplot(si~t_ave, data=data1, col="pink")
  boxplot(si~alt, data=data1, col="pink")
  
  ggplot(data1, aes(x=site, y=si, color=alt)) + geom_jitter(height=.2, width=0.2)
  
  
  ggplot(data1, aes(x=clim_temp, y=si, color=alt)) + geom_jitter(height=.2, width=0.2) + theme_light()
  
  #Colorful boxplot
  ggplot(data1, aes(prec,si, fill= as.factor(site)))+
    geom_boxplot() +
    theme(legend.position="right") +
    labs(x="Site", y="Cattle Size Index", fill="Site")
  
  ggplot(data1, aes(clim_temp,si))+
    geom_boxplot(color="red", fill="orange", alpha=0.2) +
    theme(legend.position="right") +
    labs(x="Climate (temperatures)", y="Cattle Size Index")
  
  ggplot(data1, aes(clim_prec,si))+
    geom_boxplot(color="blue", fill="blue2", alpha=0.2) +
    theme(legend.position="right") +
    labs(x="Climate (precipitation)", y="Cattle Size Index")
  
  ggplot(data1, aes(phase,si))+
    geom_boxplot(color="green4", fill="green", alpha=0.2) +
    theme(legend.position="right") +
    labs(x="Phase", y="Cattle Size Index")
  
  ggplot(data1, aes(x=site, y=si, fill=site)) +
    geom_boxplot(alpha=0.3) +
    theme(legend.position="right") +
    scale_fill_brewer(palette="BuPu")+
    scale_fill_discrete (labels=c('Annapolis', 'Antigua', 'Charleston', 'Spain', 'Florida', 'Merida', 'Mexico', 'Puerto Real', 'Quebec')) +
    labs(x="Site", y="Cattle Size Index", fill="Site")
  
  ggplot(data1, aes(x=site, y=si, fill=site)) +
    geom_boxplot(alpha=0.3) +
    theme(legend.position="right") +
    scale_fill_brewer(palette="BuPu")+
    scale_fill_discrete (labels=c('Annapolis', 'Antigua', 'Charleston', 'Spain', 'Florida', 'Merida', 'Mexico', 'Puerto Real', 'Quebec')) +
    labs(x="Site", y="Cattle Size Index", fill="Site") +
    facet_wrap(~clim_temp, nrow=1)
  
  # box plot si vs temperatures comparison
  
  library(gridExtra)
  
  p1 <- ggplot(data1, aes(x=site, y=si, fill=site)) +
    geom_boxplot(alpha=0.3) +
    theme(legend.position="right") +
    scale_fill_brewer(palette="BuPu")+
    scale_fill_discrete (labels=c('Annapolis', 'Antigua', 'Charleston', 'Spain', 'Florida', 'Merida', 'Mexico', 'Puerto Real', 'Quebec')) +
    labs(x="Site", y="Cattle Size Index", fill="Site") +
    facet_wrap(~clim_temp, nrow=1)
  
  p2 <- ggplot(data1, aes(clim_temp,si)) +
    geom_boxplot(color="red", fill="orange", alpha=0.2) +
    theme(legend.position="right") +
    labs(x="Climate (temperatures)", y="Cattle Size Index")
  
  grid.arrange(p2, p1, ncol=1, heights=c(1, 1))
  
  
  # box plot si vs precipitation comparison
  
  library(gridExtra)
  
  p3 <- ggplot(data1, aes(x=site, y=si, fill=site)) +
    geom_boxplot(alpha=0.3) +
    theme(legend.position="right") +
    scale_fill_brewer(palette="BuPu")+
    scale_fill_discrete (labels=c('Annapolis', 'Antigua', 'Charleston', 'Spain', 'Florida', 'Merida', 'Mexico', 'Puerto Real', 'Quebec')) +
    labs(x="Site", y="Cattle Size Index", fill="Site") +
    facet_wrap(~clim_prec, nrow=1)
  
  p4 <- ggplot(data1, aes(clim_prec,si)) +
    geom_boxplot(color="blue4", fill="darkturquoise", alpha=0.2) +
    theme(legend.position="right") +
    labs(x="Climate (precipitation)", y="Cattle Size Index")
  
  grid.arrange(p4, p3, ncol=1, heights=c(1, 1))
  
  # box plot si vs altitude comparison
  
  
  p5 <- ggplot(data1, aes(x=site, y=si, fill=site)) +
    geom_boxplot(alpha=0.3) +
    theme(legend.position="right") +
    scale_fill_brewer(palette="BuPu")+
    scale_fill_discrete (labels=c('Annapolis', 'Antigua', 'Charleston', 'Spain', 'Florida', 'Merida', 'Mexico', 'Puerto Real', 'Quebec')) +
    labs(x="Site", y="Cattle Size Index", fill="Site") +
    facet_wrap(~alt_cat, nrow=1)
  
  p6 <- ggplot(data1, aes(alt_cat,si)) +
    geom_boxplot(color="green4", fill="green1", alpha=0.2) +
    theme(legend.position="right") +
    labs(x="Altitude", y="Cattle Size Index")
  
  grid.arrange(p6, p5, ncol=1, heights=c(1, 1))
  
  #basic box plot 
  ggplot(data1, aes(x=site, y=si, fill=si)) +
    geom_boxplot() +
    theme(legend.position="none")+
    labs(x="Site", y="Cattle Size Index", fill="site")
  
  
  
  ggplot(data=data1, aes(x=phase, y=si)) +  geom_bar(stat="summary") + 
    theme_classic()+ 
    labs(x= "phase", y = "si") + 
    theme(legend.position = "bottom")    
  
  ggplot(data=data1, aes(x=site , y=si)) + 
    geom_errorbar(data=data1, aes(ymin=mean("si")), ymax=(mean("si")), width=.2, position=position_dodge(width=0.5), lwd=1.5) +
    geom_point(data=data1,size=5, stroke=1.5, position=position_dodge(width=0.5), pch=21) 
  
  
  #same as the boxpplot but with little dots
  
  ggplot(data=data1, aes(x=alt, y=si, color=si))+
    geom_jitter(height=0, width=.01)+
    #annotate("text" ,label="OD-P", x=log10(12587), y=17.48, color="black", size=4)+
    #annotate("text" ,label="NB", x=log10(12587), y=28.13, color="black", size=4)+
    geom_smooth(method="glm",method.args = list(family = 'poisson'))
  
  
  ggplot(data=data1, aes(x=t_ave, y=si, color=si))+
    geom_jitter(height=0, width=.01)+
    #annotate("text" ,label="OD-P", x=log10(12587), y=17.48, color="black", size=4)+
    #annotate("text" ,label="NB", x=log10(12587), y=28.13, color="black", size=4)+
    geom_smooth(method="glm",method.args = list(family = 'poisson'))
  
  ggplot(data=data1, aes(x=spei, y=si, color=si))+
    geom_jitter(height=0, width=.01)+
    #annotate("text" ,label="OD-P", x=log10(12587), y=17.48, color="black", size=4)+
    #annotate("text" ,label="NB", x=log10(12587), y=28.13, color="black", size=4)+
    geom_smooth(method="glm",method.args = list(family = 'poisson'))
  
  #no need to run this, Im messing around 
  glmer(data2 ~ site*t_ave*alt, family='binomial')
  
  index_nb <- glmer(si~alt*t_ave+
                      (1|site), 
                    family='Poisson',
                    data=data1)
  summary(index_nb)
  #no need to run this, Im messing around 
  
  #this compares variables
  library(GGally)
  ggpairs(data2[,c("site","phase", "t_ave", "alt", "spei")])
  
  ggpairs(data2[,c("site","phase", "t_ave", "alt", "spei")]) +
    theme(axis.text.x = element_text(size = 10))
  
  
  #if highly correlated variables explain the same thing, do not put in the model!
  
  
  
  #Model
  
  #this is a PCA with many variables
  
  #Please check if index and rda is appropiate 
  
  rda1 <- capscale(log(data2[,4:6]+1) ~ t_ave+alt+spei, data = data2, dist = "euclidian", add = "lingoes", binary=T) 
  
  anova.cca(rda1, permuations = how(nperm = 999), by = "margin")
  
  #need to run the link to get the plot
  
  source ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/triplot.rda.R')
  
  triplot.rda(rda1,site.sc = "wa",scaling = 1, label.sites = F)
  
  
  
  #I run couple of models... So, this one works. It says that there are no 
  #significant differences between your variables, so they don't explain sizes.
  #Since your data have a normal distribution hist(data1$si) then, I just used 
  #a linear model
  
  #Model 1
  Size_index <- lmer(si~alt* t_ave*spei+
                       (1|site), 
                     data=data2)
  
  summary(Size_index)
  
  #Model 2
  Size_index2 <- lmer(si~phase*alt*t_ave*spei+
                        (1|site), 
                      data=data2)
  
  summary(Size_index2)
  
  #Model 3 site and pahse as random effects. Honestly, I like this one better.
  #Variability in Model 1 and 2 might be just because phase and site don't account for 
  #for enviromental variables
  #I am not using spei because in the RDS plot spei, althought is not correlated 
  # with alt, it account almost for the same samples. 
  
  Size_index3 <- lmer(si~alt*t_ave+
                        (1|site/phase), 
                      data=data1)
  
  summary(Size_index3)
  Anova(Size_index3)
  plot(Size_index3)
  
  #Model 4. Here is the phase as a random effect
  
  Size_index4 <- lmer(si~t_ave* alt+
                        (1|country/phase), 
                      data=data1)
  summary(Size_index4)
  emmeans(Size_index4)
  Anova(Size_index4)
  plot(Size_index4)
  
  #Model 5 with Country as a variable to account for other sources of variation 
  #between populations
  
  #no hay difrencias -
  
  
  Size_index5 <- lmer(si~country*alt+
                        (1|phase), 
                      data=data1)
  
  summary(Size_index5)
  Anova(Size_index5)
  plot(Size_index3)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #this compares variables
  library(GGally)
  ggpairs(data1[,c("site","phase", "t_mod", "alt", "prec")])
  #if highly correlated variables explain the same thing, do not put in the model!
  
  
  
  #Model
  
  #this is a PCA with many variables
  
  #Please check if index and rda is appropiate 
  
  rda1 <- capscale(log(data1[,c(5,8,9)]+1) ~ alt+t_mod+prec, data = data1)
  anova(rda1, perm = 999, by = "margin")
  #need to run the link to get the plot
  
  source ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/triplot.rda.R')
  
  triplot.rda(rda1,site.sc = "wa",scaling = 1, label.sites = T)
  
  
  

  
  #GLMM site and pahse as random effects. 
  
  Size_index3 <- lmer(si~alt*t_mod+
                        (1|site/phase), 
                      data=data1)
  
  summary(Size_index3)
  Anova(Size_index3)
  plot(Size_index3)
  

  

  