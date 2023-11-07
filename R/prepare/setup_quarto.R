
if (!("stories" %in% list.files("www"))){
  dir.create("www/stories")
}

browseURL("https://stifterverband.sharepoint.com/:f:/s/DatenmenscheninSVundTchtern-Meilen/Evphz-nTs7dIlTnzNZcvhRUB2VVqiJ6YxYSTbV63QKqCOA?e=lgIGq7")
message("Bitte den Stories-ZIP-Ordner herunterladen und in www/stories entpacken, sodass die index.html direkt im Ordner www/stories liegt.")
