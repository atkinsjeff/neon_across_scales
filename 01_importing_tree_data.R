# LEt's try to bring in all these files

require(tidyverse)
require(data.table)
require(ggplot2)
# options(stringsAsFactors=FALSE)

# this brings in all of the apparentindividual files with plot info and dbh
list.files(path = "./data/NEON_struct-woody-plant",
               pattern = "*apparent*", 
               full.names = T) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
    data.frame() -> df.ind

# this brings in the species and mapping information 
list.files(path = "./data/NEON_struct-woody-plant",
pattern = "*mapping*", 
full.names = T) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
    data.frame() -> df.map


# merge to single df
df.ind %>%
    filter(measurementHeight == 130) %>%
    data.frame() -> df

# adding taxon and species
df$taxonID <- df.map$taxonID[match(df$individualID, df.map$individualID)]
df$latitude <- df.map$taxonID[match(df$individualID, df.map$individualID)]
# species richness

# species by plot
df %>%
    group_by(siteID, taxonID) %>%
    count(taxonID) %>%
    data.frame() -> df.count

# Species richness
df %>%
    group_by(siteID, plotID, domainID) %>%
    count(species.rich = n_distinct(taxonID)) %>%
    data.frame() -> df.richness


df.richness %>%
    group_by(domainID) %>%
    summarize(sd.richness = sd(species.rich)) %>%
    data.frame -> domain.level

df.richness %>%
    group_by(siteID) %>%
    summarize(sd.richness = sd(species.rich)) %>%
    data.frame -> site.level

df.richness %>%
    summarize(sd.richness = sd(species.rich)) %>%
    data.frame -> us.level


### box plot
x11()
ggplot(site.level, aes(x = sd.richness))+
    geom_density(alpha= 0.5, fill="blue", color = "black")+
    geom_density(data = domain.level, alpha = 0.5, fill = "purple", color = "black")+
    geom_vline(xintercept = 5.578, color = "black", size = 2)+
    theme_bw()

unique(df$taxonID)


####################################################
# BIOMASS

#bring in jenkins model from jenkins et al. 2003

jenkins_model <- c("aspen/alder/cottonwood/willow", "soft maple/birch", "mixed hardwood", "hard maple/oak/hickory/beech", "cedar/larch", "doug fir", "fir/hemlock", "pine", "spruce", "juniper/oak/mesquite")

model_name <- c("hw1", "hw2", "hw3", "hw4", "sw1", "sw2", "sw3", "sw4", "sw5", "wl")

beta_one <- c(-2.20294, -1.9123, -2.4800, -2.0127, -2.0336, -2.2304, -2.5384, -2.5356, -2.0773, -0.7152)

beta_two <- c(2.3867, 2.3651, 2.4835, 2.4342, 2.2592, 2.4435, 2.4814, 2.4349, 2.3323, 1.7029)

jenkins <- data.frame(jenkins_model, model_name, beta_one, beta_two)

tax.jenk <- read.csv("./data/neon_taxon_jenkins.csv")

jenkins_plus <- merge(tax.jenk, jenkins)

df2 <- merge(df, jenkins_plus, by = "taxonID", all = TRUE)


# filters out plant status
df2 %>%
    filter(plantStatus == "Live" | plantStatus == "Live, disease damaged" | plantStatus == "Live, physically damaged" | plantStatus == "Live,  other damage" | plantStatus == "Live, broken bole" | plantStatus == "Live, insect damaged" ) -> df3

df3$biomass <- exp(df3$beta_one + (df3$beta_two * log(as.numeric(df3$stemDiameter))))

# plot level
df3 %>%
    group_by(siteID, plotID, domainID) %>%
    count(plot.biomass = sum(biomass, na.rm = TRUE)) %>%
    data.frame() -> df.biomass


df.biomass %>%
    group_by(domainID) %>%
    summarize(sd.richness = sd(plot.biomass)) %>%
    data.frame -> domain.level

df.richness %>%
    group_by(siteID) %>%
    summarize(sd.richness = sd(species.rich)) %>%
    data.frame -> site.level

df.richness %>%
    summarize(sd.richness = sd(species.rich)) %>%
    data.frame -> us.level


# change to per hectare
plot.npp$npp <- plot.npp$npp * 12.5

# change to Mg per hectare
plot.npp$npp <- plot.npp$npp * 0.001

taxons <- unique(df3$taxonID)

write.csv(taxons, "./data/taxons_from_dataset.csv")
