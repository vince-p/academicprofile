# This file opens a csv with all media items and converts to hugo-academic projects
# This script uses the custom /content/media folder (A slightly customised projects/portfolio template).

library(stringi)

media <- read.csv("vscripts/medialist.csv")
media$date<-as.Date(media$date,"%d/%m/%Y")
media$image_url<-as.character(media$image_url)
# replace curly apostraphes
media$title <- stri_replace_all_fixed(media$title, pattern = c("’", "‘"), replacement = c("'"), vectorize_all = FALSE)


wl <- function(input){
  cat(input, sep="\n")
}

for (i in 1:nrow(media)) {
  fold <- paste0("content/media/",media$date[i],"_",media$source[i])
  if(media$date[i] > Sys.Date()) media$date[i] <- Sys.Date()
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

  # additional processing if no images exist
  if(!file.exists(paste0(fold,"/featured.jpg")) & !file.exists(paste0(fold,"/featured.png"))){

    # see if there is a URL ending in png
    ifelse(substr(media$image_url[i],nchar(media$image_url[i])-2,nchar(media$image_url[i]))=="png",
     #if there is then download and rename
     download.file(media$image_url[i],paste0(fold,"/featured.png"), method = "curl"),
     #if there is not, then check if there is a URL ending in jpg
     ifelse(substr(media$image_url[i],nchar(media$image_url[i])-2,nchar(media$image_url[i]))=="jpg",
        #if there is then download and rename
        download.file(media$image_url[i],paste0(fold,"/featured.jpg"), method = "curl"),
          #if there is not then print a notice
          print(paste(fold,"DOES NOT HAVE AN IMAGE"))
      )
     )
  }
}
