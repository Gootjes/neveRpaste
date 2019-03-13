
#' @export
installWordIntegration <- function() {
  paths <- findWordStartupFolders()
  if(length(paths)==0) {
    stop("No valid path available to copy .dotm file to")
  }
  installInFolders(paths = paths)
}

findWordStartupFolders <- function() {
  .this <- environment()
  versions <- c()
  paths <- c()
  os <- .Platform$OS.type
  if(os == "windows") {
    version <- 12 # because of macro's, .dotm
    while(version < 20) { #20 is a magic number

      exists <- F

      tryCatch({
        #Location 1
        pa <- file.path("Software", "Microsoft", "Office", paste0(version, ".0"), "Word", "Options", fsep = "\\")
        value <- readRegistry(key = pa, hive = "HCU")
        .this$versions <- c(.this$versions, version)
        exists <- T
        if("STARTUP-PATH" %in% names(value)) {
          .this$paths <- c(.this$paths, value$`STARTUP-PATH`)
        }
      }, error = function(e){

      })
      tryCatch({
        #Location 2
        pa <- file.path("Software", "Microsoft", "Office", paste0(version, ".0"), "Common", "General", fsep = "\\")
        value <- readRegistry(key = pa, hive = "HCU")
        if(!exists) {
          exists <- T
          .this$versions <- c(.this$versions, version)
        }

        ad <- Sys.getenv("APPDATA")
        if("Startup" %in% names(value)) {
          .this$paths <- c(.this$paths,
                           file.path(ad, "Microsoft", "Word", value$`Startup`, fsep = "\\")
                           )
        }
      }, error = function(e){

      })
      tryCatch({
        #Location 3
        pa <- file.path("Software", "Microsoft", "Office", paste0(version, ".0"), "Word", fsep = "\\")
        value <- readRegistry(key = pa, hive = "HLM")

        if(!exists) {
          exists <- T
          .this$versions <- c(.this$versions, version)
        }
      }, error = function(e){

      })

      version <- version + 1
    }
  } else {
    stop("Not implemented for your platform, install .dotm file manually")
  }

  if(length(.this$versions) == 0) {
    return(NULL)
  }

  if(length(.this$paths) == 0) {
    return(file.path(Sys.getenv("APPDATA"), "Microsoft", "Word", "Startup", fsep = "\\"))
  }

  paths
}

installInFolders <- function(paths) {
  f <- system.file("NeverPaste.dotm", package = "neverpaste")
  if(nchar(f) == 0) {
    stop("File does not exist")
  }
  for(path in paths) {
    if(file.exists(path)) {
      dest <- file.path(path, "NeverPaste.dotm")
      file.copy(from = f, to = dest, overwrite = T, recursive = F, copy.date = T, copy.mode = F)
      cat("NeverPaste.dotm was copied to ", dest, "\n", sep = "")
    }
  }
}
