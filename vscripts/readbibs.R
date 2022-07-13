#adapted from https://lbusett.netlify.com/post/automatically-importing-publications-from-bibtex-to-a-hugo-academic-blog/
# Need a better way of cleaning escape characters

bibfile <- "c:/bib/papers.bib"
out_fold   <- "content/publication"

pacman::p_load(pacman)
p_load(RefManageR)
p_load(dplyr)
p_load(stringr)
p_load(anytime)
p_load(lubridate)

# Remove existing directories
junk <- list.dirs(out_fold,recursive = FALSE) # https://www.geeksforgeeks.org/obtain-list-of-directories-in-r/
unlink(junk, recursive = TRUE)


# settings
overwrite=TRUE
abstract=TRUE

  # read refs from zotero
  # collection found from the URL in the zotero web interface
  # api and username from zotero settings
  z <- ReadZotero(user = "4226", .params = list(collection = "QIKM8IVA", key = "76GElUBWjuYxOPpdBzm5jSeX", limit=200),delete.file = TRUE)
  # clean paper titles
  z$title<-gsub("[{}]", "", z$title)
  zframe <- select(as.data.frame(z), -abstract)


    # Import the local bibtex file and convert to data.frame
    l   <- ReadBib(bibfile, check = "warn", .Encoding = "UTF-8")
    # clean paper titles
    l$title<-gsub("[{}]", "", l$title)
    #l$title<-gsub("â€“", "-", l$title) # PROBLEM WITH HYPHENS. NEED TO MANUALLY EDIT RECORDS IN ZOTERO :(
    # select only title and files and abstract
    # for an unknown reason the local abstract handles special characters better
    lframe <- select(as.data.frame(l),title,file,abstract)

    #join dataframes to get nice formatting from zotero + filenames from local
    mypubs <- left_join(zframe,lframe)

    #code to remove bibTex and text formatting errors
    mypubs$journal<-gsub('\\', '', mypubs$journal, fixed=TRUE)
    mypubs$booktitle<-gsub("[{}]", "", mypubs$booktitle)
    mypubs$author<-gsub("[{}=]", "", mypubs$author)
    mypubs$abstract<-gsub("[{}]", "", mypubs$abstract)
    mypubs$abstract<-gsub('"', "'", mypubs$abstract)
    mypubs$abstract<-gsub("\\\\", "", mypubs$abstract)
    mypubs$abstract<-gsub("\n", " ", mypubs$abstract)
    mypubs$abstract<-gsub('textendash ', '-', mypubs$abstract)


    # manually recode long author list for many-analysts project
    mypubs[mypubs$title == "A many-analysts approach to the relation between religiosity and well-being",]$author <- "Suzanne Hoogeveen and Alexandra Sarafoglou and Balazs Aczel ... and Vince Polito et al."

    #correct missing month
    mypubs$month[is.na(mypubs$month)]<-"jan"


    # assign "categories" to the different types of publications
    # Original script had 'document_type' rather than bibtype
    # Perhaps bibtype is specific to zotero
    mypubs   <- mypubs %>%
        dplyr::mutate(
            pubtype = dplyr::case_when(bibtype == "Article" ~ "2",
                                       bibtype == "Article in Press" ~ "2",
                                       bibtype == "InProceedings" ~ "1",
                                       bibtype == "Proceedings" ~ "1",
                                       bibtype == "Conference" ~ "1",
                                       bibtype == "Conference Paper" ~ "1",
                                       bibtype == "MastersThesis" ~ "7",
                                       bibtype == "PhdThesis" ~ "7",
                                       bibtype == "Manual" ~ "4",
                                       bibtype == "TechReport" ~ "4",
                                       bibtype == "Book" ~ "5",
                                       bibtype == "InCollection" ~ "6",
                                       bibtype == "InBook" ~ "6",
                                       bibtype == "Misc" ~ "4",
                                       TRUE ~ "0"))

    #only use the first file listed when there are multiple attachments
    mypubs$file<-sub(';.*', '', mypubs$file)
    #sub(".*;", "", mypubs$file)

    mypubs$oldfile<-gsub("\\\\:",":",mypubs$file)
    mypubs$oldfile<-gsub("\\\\\\\\","/",mypubs$oldfile)

    #clear out NAs in language (as this will be used to mark selected pubs)
    mypubs$language[is.na(mypubs$language)]<-""

    if (is.na(mypubs$year)) {
        mypubs$year <- 2999
    }

    extrapath <- paste(mypubs[["year"]], mypubs[["title"]] %>%
                           str_replace_all(fixed(" "), "_") %>%
                           str_remove_all(fixed(":")) %>%
                           str_sub(1, 30), sep = "_")

    mypubs$extrapath<-paste(out_fold,extrapath,sep="/")

    mypubs$filename <- paste0(mypubs$extrapath,"/",extrapath,".pdf")



    # create a function which populates the md template based on the info
    # about a publication
    create_md <- function(x) {
        # start writing
        ifelse(!dir.exists(file.path(x[["extrapath"]])), dir.create(file.path(x[["extrapath"]])), FALSE)


        if (!file.exists(file.path(paste(x[["extrapath"]],"index.md",sep="/"))) | overwrite) {
            fileConn <- file.path(paste(x[["extrapath"]],"index.md",sep="/"))
            write("+++", fileConn)

            # Title and date
            write(paste0("title = \"", x[["title"]], "\""), fileConn, append = T)
            #use lubridate function to parse month and year fields into date
            write(paste0("date = \"", dmy(paste(01,x[["month"]],x[["year"]],sep="-")), "\""), fileConn, append = T)
            write(paste0("publishdate = \"2000-01-01\""),fileConn, append = T)

            # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
            auth_hugo <- str_replace_all(x["author"], " and ", "\", \"")
            auth_hugo <- stringi::stri_trans_general(auth_hugo, "latin-ascii")
            write(paste0("authors = [\"", auth_hugo,"\"]"), fileConn, append = T)

            # Publication type. Legend:
            # 0 = Uncategorized, 1 = Conference paper, 2 = Journal article
            # 3 = Manuscript, 4 = Report, 5 = Book,  6 = Book section
            write(paste0("publication_types = [\"", x[["pubtype"]],"\"]"),
                  fileConn, append = T)


            # script did not handle book chapters. This is a makedo solution.
            if (!is.na(x[["booktitle"]])) x[["journal"]] <- x[["booktitle"]]

            # Publication details: journal, volume, issue, page numbers and doi link
                        publication <- x[["journal"]]
            if (!is.na(x[["volume"]])) publication <- paste0(publication,
                                                             ", (", x[["volume"]], ")")
            if (!is.na(x[["number"]])) publication <- paste0(publication,
                                                             ", ", x[["number"]])
            if (!is.na(x[["pages"]])) publication <- paste0(publication,
                                                            ", _pp. ", x[["pages"]], "_")
            if (!is.na(x[["doi"]])) publication <- paste0(publication,
                                                          ", ", paste0("https://doi.org/",
                                                                       x[["doi"]]))

            write(paste0("publication = \"", publication,"\""), fileConn, append = T)
            write(paste0("publication_short = \"", publication,"\""),fileConn, append = T)

            # Abstract and optional shortened version.
            if (abstract) {
                write(paste0("abstract = \"", x[["abstract"]],"\""), fileConn, append = T)
            } else {
                write("abstract = \"\"", fileConn, append = T)
            }
            write(paste0("summary = \"","\""), fileConn, append = T)

            # other possible fields are kept empty. They can be customized later by
            # editing the created md

            write("image_preview = \"\"", fileConn, append = T)

            # If a zotero record has the language set as "X", then mark this as a Selected Output
            if (x[["language"]]=="X") {
                write("featured = true", fileConn, append = T)
            } else {
                write("featured = false", fileConn, append = T)
            }

            #If a zotero record has a project listed in the copyright field, parse it here
            if (!is.na(x[["copyright"]])) {
                write(paste0("projects = ['",x[["copyright"]],"']"), fileConn, append = T)
            } else {
                write("projects = []", fileConn, append = T)
            }

            write("tags = []", fileConn, append = T)
            #links
            write("url_pdf = \"\"", fileConn, append = T)
            write("url_preprint = \"\"", fileConn, append = T)
            write("url_code = \"\"", fileConn, append = T)
            write("url_dataset = \"\"", fileConn, append = T)
            write("url_project = \"\"", fileConn, append = T)
            write("url_slides = \"\"", fileConn, append = T)
            write("url_video = \"\"", fileConn, append = T)
            write("url_poster = \"\"", fileConn, append = T)
            write("url_source = \"\"", fileConn, append = T)
            #other stuff
            write("math = true", fileConn, append = T)
            write("highlight = true", fileConn, append = T)
            # Featured image
            write("[header]", fileConn, append = T)
            write("image = \"\"", fileConn, append = T)
            write("caption = \"\"", fileConn, append = T)

            write("+++", fileConn, append = T)
        }

    file.copy(x[["oldfile"]],x[["filename"]])
    }

    # apply the "create_md" function over the publications list to generate
    # the different "md" files.
    apply(mypubs, FUN = function(x) create_md(x), MARGIN = 1)
