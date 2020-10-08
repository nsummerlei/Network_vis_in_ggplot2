# Load the packages

library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(scales)
library(broom)
library(stringr)
library(cowplot)
library(wesanderson)
library(dplyr)

convert.to.num <- function(x){
  x <- gsub(',','',x)
  x <- as.numeric(as.character(x))
}

setwd('/Volumes/Xongkoro/Publication efforts/Migration and Vaccination Paper/Datasets/')

# 1. Load the data

# 1.1 Population estimates (mid-2018)
pop <- read.csv('./R-Population and density 2019.csv',header = TRUE)
pop <- pop[,c('LA.code','LA.name','population2018')]

# 1.2 Migration matrix (2019)
# Data source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/matricesofinternalmigrationmovesbetweenlocalauthoritiesandregionsincludingthecountriesofwalesscotlandandnorthernireland
# (Also needs to) turn matrix into edge list
migration.matrix <- read.csv('./19-internal migration UK.csv', header = TRUE)

network.use <- as.matrix(migration.matrix[,-1])
diag(network.use) <- 0
network.use <- apply(network.use, c(1,2), convert.to.num)

edgelist <- data.frame(source.ons = rep(colnames(network.use),each = 341), 
                       target.ons = rep(colnames(network.use), by = 341),
                       counts = 0)
for (i in 1:341) {
  in.ons <- colnames(network.use)[i]
  for (j in 1:341) {
    out.ons <- colnames(network.use)[j]
    edgelist[edgelist$source.ons == out.ons & edgelist$target.ons == in.ons,'counts'] <- network.use[i,j]
    cat(i,":",j,"\n")
  }
}

# Eliminate Scotland and Northern Ireland
edgelist2 <- edgelist[str_detect(edgelist$source.ons, paste(c('E','W'), collapse = '|')) &
                        str_detect(edgelist$target.ons, paste(c('E','W'), collapse = '|')),]

saveRDS(edgelist,'internal_migration_in_out_la.rds')

# 1.3 Geographic data
la.long.lat <- read.csv('./Local_Authority_Districts__December_2019__Boundaries_UK_BFC.csv',header = TRUE)
ltla.shp <- readOGR(dsn = '.',layer = 'Local_Authority_Districts__December_2019__Boundaries_UK_BFC')
ltla.shp.tran <- spTransform(ltla.shp,CRS('+proj=longlat +datum=WGS84'))  # Convert to Longitude / Latitude with WGS84 Coordinate System 
ltla.shp.f <- tidy(ltla.shp.tran)  # fortify(utla.shp.tran, region = 'objectid)
ltla.shp.f  <- ltla.shp.f[order(ltla.shp.f$order),]

saveRDS(ltla.shp.f,'uk_graphic.rds')

# 1.4 Regional movement
reg.matrix <- read.csv('./19-regional migration.csv', header = TRUE)
reg.list <- colnames(reg.matrix)[2:13]
reg.matrix <- as.matrix(reg.matrix[,-1])
diag(reg.matrix) <- 0
reg.matrix <- apply(reg.matrix, c(1,2), convert.to.num)

edgelist_reg <- data.frame(source.ons = rep(reg.list,12),
                           target.ons = rep(reg.list, each = 12),
                           counts = 0)

for (i in 1:12) {
  in.ons <- reg.list[i]
  for (j in 1:12) {
    out.ons <- reg.list[j]
    edgelist_reg[edgelist_reg$source.ons == out.ons & edgelist_reg$target.ons == in.ons,'counts'] <- reg.matrix[i,j]
    cat(i,":",j,"\n")
  }
}

reg.cd_nm <- read.csv('./region name and code.csv', header = TRUE)

# eliminate Scotland and Northern Ireland

edgelist_reg2 <- edgelist_reg[str_detect(edgelist_reg$source.ons, paste(c('E','W'), collapse = '|')) &
                                str_detect(edgelist_reg$target.ons, paste(c('E','W'), collapse = '|')),]

# 2. Visualisation
# 2.1 Circular plot

library(circlize)

edgelist_reg2$counts <- edgelist_reg2$counts/100000

reg.list <- c("E12000001","E12000002","E12000003","E12000004","E12000005","E12000006",
              "E12000007","E12000008","E12000009","W92000004")

# colour list
df1 <- data.frame(region = reg.list, 
                  order = 1:10, 
                  col1 = c('#40A4D8','#33BEB7','#B2C224','#FECC2F','#FBA127','#F66320',"#F72585",'#DB3937','#A463D7','#0C5BCE'),
                  reg1 = c('North','North','Yorkshire and','East','West','East of','London','South','South','Wales'),
                  reg2 = c('East','West','the Humber','Midlands','Midlands','England','','East','West',''))

circos.clear()
chordDiagram(x = edgelist_reg2)

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.2, 0.2), points.overflow.warning = FALSE)

par(mar = rep(0, 4))

chordDiagram(x = edgelist_reg2 %>% select(1:3), grid.col = df1$col1, 
             #transparency = 0.25,
             order = df1$region, directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)

##add in labels and axis

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = df1$reg1[df1$region == sector.index]
    reg2 = df1$reg2[df1$region == sector.index]
    circos.text(x = mean(xlim), y = ifelse(test = nchar(reg2) == 0, 
                                           yes = 5, no = 5.5), 
                labels = reg1, facing = "bending.inside", cex = 2)
    circos.text(x = mean(xlim), y = 4, labels = reg2, 
                facing = "bending.inside", cex = 2)
    circos.axis(h = "top", labels.cex = 2,
                major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
                minor.ticks = 1, major.tick.percentage = 0.5,
                labels.niceFacing = FALSE)})
dev.copy2pdf(file = "/Volumes/Xongkoro/V9migration.pdf")

# 2. Geographic map
# 2.1 Preparation for the data involved in the map

# 2.1.1 Background map with region coloured
uk_map <- readRDS('./uk_graphic.rds')

ltla.reg <- read.csv('./Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv',header = TRUE)
names(ltla.reg)[2] <- 'lad19cd'
england <- merge(la.long.lat,ltla.reg,by = 'lad19cd') %>% select(c(2,14))
ni <- la.long.lat[str_detect(la.long.lat$lad19cd,'N'),] %>% 
  mutate(RGN19NM = 'Northern Ireland') %>% select(1,11)
wales <- la.long.lat[str_detect(la.long.lat$lad19cd,'W'),] %>% 
  mutate(RGN19NM = 'Wales') %>% select(1,11)
scotland <- la.long.lat[str_detect(la.long.lat$lad19cd,'S'),] %>% 
  mutate(RGN19NM = 'Scotland') %>% select(1,11)

uk <- rbind(england,wales,ni,scotland)
names(uk) <- c('id','region')
uk$id <- as.character(uk$id - 1)
uk_map <- merge(uk_map,uk,by = 'id', all.x = TRUE)

saveRDS(uk_map,'uk_graphics_region.rds')

# Eliminate Scotland and Northern Ireland

ew_map <- uk_map[!uk_map$region %in% c('Scotland','Northern Ireland'),]

saveRDS(ew_map,'ew_graphics_region.rds')

# 2.1.2 Add long and lat to "edgelist2"
#       Here I use 'Glasgow' and 'Belfast' as the representative of Scotland and NI
engwal.long.lat <- la.long.lat[str_detect(la.long.lat$lad19cd,paste(c('E','W'),collapse="|")),] %>% select(2,7:8)
ewsn.long.lat <- rbind(engwal.long.lat,
                       la.long.lat[la.long.lat$lad19cd == 'N09000003',c('lad19cd','long','lat')],
                       la.long.lat[la.long.lat$lad19cd == 'S12000049',c('lad19cd','long','lat')])
ewsn.long.lat[ewsn.long.lat$lad19cd == 'N09000003','lad19cd'] <- 'N92000002'
ewsn.long.lat[ewsn.long.lat$lad19cd == 'S12000049','lad19cd'] <- 'S92000003'

# Eliminate Scotland and Northern Ireland 

ew.long.lat <- ewsn.long.lat[str_detect(ewsn.long.lat$lad19cd, paste(c('E','W'), collapse = '|')),]

names(ew.long.lat) <- c('source.ons','source.long','source.lat')
edge.long.lat <- merge(edgelist2, ew.long.lat, by = 'source.ons', all.x = TRUE)
names(ew.long.lat) <- c('target.ons','target.long','target.lat')
edge.long.lat <- merge(edge.long.lat, ew.long.lat, by = 'target.ons', all.x = TRUE)

edge.long.lat$level <- 0

edge.long.lat[edge.long.lat$counts <= 250,'level'] <- 'a'
edge.long.lat[between(edge.long.lat$counts,251,500),'level'] <- 'b'
edge.long.lat[between(edge.long.lat$counts,501,1000),'level'] <- 'c'
edge.long.lat[between(edge.long.lat$counts,1001,2500),'level'] <- 'd'
edge.long.lat[edge.long.lat$counts > 2500,'level'] <- 'e'

edge.long.lat <- edge.long.lat[edge.long.lat$target.ons != edge.long.lat$source.ons,]

saveRDS(edge.long.lat, 'edge_long_lat.rds')

# 2.1.3 Nodes
#       needs to additionally add the population for four districts 
#       population data for those four councils were found on wikipedia
library(ggnewscale)

nodelist <- edge.long.lat %>% select(2,4,5) %>% unique()
names(nodelist) <- c('LA.code','long','lat')

pop.add <- rbind(pop,
                 data.frame(LA.code = c("E07000004","E07000005","E07000006","E07000007"),
                            LA.name = c("Aylesbury Vale","Chiltern","South Bucks","Wycombe"),
                            population2018 = c(199448,95927,70043,174641)))

node.pop <- merge(nodelist, pop.add, by = 'LA.code')

node.pop$pop.size <- 0
node.pop[node.pop$population2018 <= 50000,'pop.size'] <- 'A'
node.pop[between(node.pop$population2018,50001,100000),'pop.size'] <- 'B'
node.pop[between(node.pop$population2018,100001,250000),'pop.size'] <- 'C'
node.pop[between(node.pop$population2018,250001,500000),'pop.size'] <- 'D'
node.pop[node.pop$population2018 > 500000,'pop.size'] <- 'E'

node.pop$In.mig <- 0
node.pop$Out.mig <- 0

for (i in 1:nrow(node.pop)) {
  la <- node.pop$LA.code[i]
  node.pop$In.mig[i] <- edgelist2[edgelist2$target.ons == la, 'counts'] %>% sum()
  node.pop$Out.mig[i] <- edgelist2[edgelist2$source.ons == la, 'counts'] %>% sum()
}

node.pop$net.mig <- node.pop$In.mig - node.pop$Out.mig
node.pop$net.rate <- node.pop$net.mig / node.pop$population2018 * 100

saveRDS(node.pop,'nodepop.rds')

# 2.2  Visualisation

region.map <- ggplot() +
  geom_polygon(data = ew_map,
               aes(long, lat, group = group, fill = region),
               alpha = 0.1,
               color = 'grey78') +
  scale_fill_manual(limits = unique(ew_map$region),
                    values = c('#40A4D8','#B2C224','#F66320','#A463D7',
                               '#DB3937','#33BEB7','#FECC2F','#FBA127',
                               '#FECC2F','#0C5BCE')) +
  geom_curve(data = edge.long.lat,
             size = 0.5,
             aes(x = source.long, y = source.lat,
                 xend = target.long, yend = target.lat,
                 alpha = level, size = level), 
             arrow = arrow(length = unit(0.06,'inches'), # increase the unit can make the arrow look bigger
                           type = 'closed'),curvature = -0.2) +
  scale_alpha_manual(values = c(0,0.125,0.25,0.5,0.75)) +
  scale_size_manual(values = c(1,1,1,1.5,2)) +
  #     Plot nodes
  new_scale_fill() +
  geom_point(data = node.pop,
             aes(x = long,y = lat, 
                 fill = net.rate,size = pop.size),
             shape = 21, alpha = 0.5) +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue"),
                       midpoint = 0) +
  scale_size_manual(values=c(1,2.5,4,5.5,7)) +
  theme_bw() +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none')

# Prepare the legends
library(cowplot)

p1 <- ggplot() + geom_point(data = node.pop,
                            aes(x = long,y = lat, fill = net.rate),
                            shape = 21) +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue"),
                       midpoint = 0) +
  labs(fill = 'Net internal \nmigration rate (%)') +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.direction = 'horizontal')
leg1 <- get_legend(p1)

p2 <- ggplot(data.frame(Population = c('0-50,000','50,001-100,000','100,001-250,000','250,001-500,000','>500,000'),
                        x = 1:5, y = 1:5), aes(x,y)) +
  geom_point(aes(size = Population),shape = 21) +
  scale_size_manual(values = c(1,2.5,4,5.5,7), 
                    limits = c('0-50,000','50,001-100,000','100,001-250,000','250,001-500,000','>500,000')) +
  labs(size = 'Population \n') +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'))
leg2 <- get_legend(p2)

p3 <- ggplot(data.frame(Rw = c('0-250','251-500','501-1,000','1,001-2,500','>2,500'),
                        xs = 1:5, ys =6:10, xe = 11:15,ye = 16:20)) +
  geom_curve(aes(x = xs,y = ys,xend = xe,yend = ye,alpha = Rw),
             arrow = arrow(length = unit(0.06,'inches'),type = 'closed'),curvature = -0.2) +
  labs(alpha = 'Inflow') +
  scale_alpha_manual(values = c(0,0.125,0.25,0.5,0.75),
                     limits = c('0-250','251-500','501-1,000','1,001-2,500','>2,500')) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'))
leg3 <- get_legend(p3)

my_plot_1 <- ggdraw(region.map) +
  draw_plot(leg1,.13,-0.2,.5,.5) +
  draw_plot(leg2,.40,-0.168,.5,.5) +
  draw_plot(leg3,.60,-0.168,.5,.5)
ggsave('./region_map.png',my_plot_1,dpi = 600, height = 12, width = 11)







