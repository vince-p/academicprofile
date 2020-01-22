# This file opens a csv with all media items and converts to hugo-academic projects
# This script uses the custom /content/media folder (A slightly customkised projects/portfolio template).

media <- read.csv("vscripts/medialist.csv")
media$date<-as.Date(media$date,"%d/%m/%Y")

wl <- function(input){
  cat(input, sep="\n")
}

for (i in 1:nrow(media)) {
  fold <- paste0("content/media/",media$date[i],"_",media$source[i])
  dir.create(fold,showWarnings = FALSE)
  sink(file(paste0(fold,"/index.md")))

  wl("---")
  wl(paste0('title: "',media$title[i],'"'))
  wl(paste("summary:",media$source[i]))
  wl("tags:")
  wl(paste0(" - ",media$type[i]))
  if (i <=3) { #tag top 3 records as recent
    wl(" - recent")
  }
  wl(paste("date:",media$date[i]))
  wl(paste("external_link:",media$url[i]))
  wl("image:")
  wl('  focal_point: "Center"')
  wl("---")
  sink()

  #rename fist image in the folder to featured.jpg/png
  file.rename(paste(fold,list.files(path = fold, pattern="*.jpg")[1],sep="/"),paste0(fold,"/featured.jpg"))
  file.rename(paste(fold,list.files(path = fold, pattern="*.png")[1],sep="/"),paste0(fold,"/featured.png"))
}
