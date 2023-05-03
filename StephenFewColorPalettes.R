# Create the three Useful Color Palettes ("Light", "Medium", "Dark & Bright") 
# from Appendix I of "Show Me the Numbers" (2012) by Stephen Few
# created palettes are called: sfc.light, sfc.medium, sfc.dark
# set additionalpalettes=TRUE if you want additional shades (sfc.lightest, sfc.lighter, sfc.darker, sfc.darkest)
# access colors by name (e.g. sfc.light["red"]) or position (e.g. sfc.dark[5])  
# also displays a colored grid containing hex codes

require("scales")
require("colorspace")


# set basic parameters
intensities <- c("light", "medium", "dark")
colornames <- c("black", "blue", "orange", "green", "pink", "brown", "purple", "yellow", "red")
additionalpalettes <- TRUE  

#RGB values as provided by Few (2012):
colorvalues.light <- matrix(ncol=3, byrow = T,
                            data=c(
                              140, 140, 140,
                              136, 189, 230,
                              251, 178, 88 ,
                              144, 205, 151,
                              246, 170, 201,
                              191, 165, 84 ,
                              188, 153, 199,
                              237, 221, 70 ,
                              240, 126, 110 ))

colorvalues.medium <- matrix(ncol=3, byrow = T,
                             data=c(
                               77 , 77 , 77 ,
                               93 , 165, 218,
                               250, 164, 58 , 
                               96 , 189, 194,
                               241, 124, 176,
                               178, 145, 47 ,
                               178, 118, 178,
                               222, 207, 63 ,
                               241, 88 , 84 ))

colorvalues.dark <- matrix(ncol=3,  byrow = T,
                           data=  c(
                             0  , 0  , 0  ,     
                             38 , 93 , 171,  
                             223, 92 , 36 ,  
                             5  , 151, 72 ,    
                             229, 18 , 111, 
                             157, 114, 42 , 
                             123, 58 , 150, 
                             199, 180, 46 , 
                             203, 32 , 39 ))


# Function to create the individual palettes
create.sfc.palette <- function(intensity){
  
  colorvalues=get(paste0("colorvalues.", intensity))
  
  mypal= unlist(lapply(1:9, function(x) 
    rgb(colorvalues[x,1],colorvalues[x,2],colorvalues[x,3], maxColorValue = 255, alpha=255)))
  
  assign(paste0("sfc.", intensity), mypal, envir=.GlobalEnv)
  
}

# Apply function to the three intensities
lapply(intensities, create.sfc.palette)


# Create additional palettes if prompted
if(additionalpalettes==TRUE){
  sfc.lightest <- lighten(sfc.light, .85)
  sfc.lighter <- lighten(sfc.light, .5)
  sfc.darker <- darken(sfc.dark, .5)
  sfc.darkest <- darken(sfc.dark, .85)
}


# name the colors in each palette with colornames 
palnames <- c(rev(ls(pattern = "^sfc.l")), "sfc.medium", ls(pattern = "^sfc.d"))
mylist <- lapply(palnames, get)
mylist <- lapply(mylist, function(x) {names(x) <- colornames; return(x)})

for(i in seq_along(mylist)){
  assign(palnames[i], unlist(mylist[i]), envir = .GlobalEnv)
}

# plot a grid of the created palettes
npals <- length(mylist)

plotdata <- data.frame(bind_rows(mylist)) %>% 
  mutate(palname=palnames) %>% 
  pivot_longer(-palname, names_to = "colorname", values_to = "hex") %>% 
  mutate(palname=factor(palname, levels=palnames),
         colorname=factor(colorname, levels=colornames),
         labelcol=ifelse(!grepl("light", palname), "white", "black"))

paletteplot <- ggplot(plotdata, aes(x=colorname, y=palname, fill=hex))+
  geom_tile(show.legend = F)+
  geom_text(aes(label=hex, color=labelcol))+
  scale_fill_identity()+
  scale_color_identity()+
  labs(y="Palette Name", x="Color Name")+
  theme_minimal()

paletteplot










# try out new colors
require("tidyverse")
set.seed(3442523)

ngroups <- 5
npergroup <- 100/ngroups

testdata5 <- data.frame(Time=seq(0,99,1)) %>% 
  mutate(Group=rep(1:ngroups,npergroup)) %>% 
  group_by(Group) %>% 
  mutate(Effect=round(runif(1,.1,.8),2),
         Value=Effect*runif(npergroup, 0, 100) + rnorm(npergroup)) 


# To use the whole palette, you must unname it
ggplot(testdata5, aes(x=Time, y=Value, color=factor(Group)))+
  geom_point(size=3, alpha=.5)+
  geom_smooth(method = "lm", se=F, formula = y~x)+
  theme_bw()+
  scale_color_manual(values=unname(sfc.medium), name="SFC Medium")


# More than 9 colors (not recommended to work with that many colors, but may still work in your case)
ngroups <- 10
npergroup <- 100/ngroups

testdata10 <- data.frame(Time=seq(0,99,1)) %>% 
  mutate(Group=rep(1:ngroups,npergroup)) %>% 
  group_by(Group) %>% 
  mutate(Effect=round(runif(1,.1,.8),2),
         Value=Effect*runif(npergroup, 0, 100) + rnorm(npergroup)) 

ggplot(testdata10, aes(x=Time, y=Value, color=factor(Group)))+
  geom_point(size=3, alpha=.5)+
  geom_smooth(method = "lm", se=F, formula = y~x)+
  theme_bw()+
  scale_color_manual(values=unname(c(sfc.light, sfc.dark)), name="SFC Light & Dark")


# Use individual values from different palettes for gradient
testdata.gradient <- data.frame(X=rep(1:9,7),
                                Y=rep(1:7,9)) %>% 
  mutate(FV=X*Y)

ggplot(testdata.gradient, aes(x=X, y=Y, fill=FV))+
  geom_tile(show.legend = F)+
  scale_fill_gradientn(colors=c(sfc.darker["blue"], sfc.medium["green"], sfc.light[c(8,5)], sfc.lighter[5]), name="") +
  labs(x="", y="")+
  theme_void()
