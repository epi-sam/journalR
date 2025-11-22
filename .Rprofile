if(interactive()){
   message("Loading .Rprofile packages")
   .pkgs <- c(
      "devtools"
   )
   for(.pkg in .pkgs){
      message(" -- ", .pkg)
      library(.pkg, character.only = TRUE)
   }

   # devtools::load_all() # can't load if we want to knit readme

   rm(.pkgs, .pkg)

   # used to include NCH libary in .libPaths()
   # - used for data_raw structures
   if(file.exists('~/.Rprofile')) source('~/.Rprofile')
}
