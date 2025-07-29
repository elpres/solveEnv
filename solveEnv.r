#!/usr/bin/Rscript
library('rjson')
conda <- function(args){
  system2('conda', c('create', '-n', '_temp_', '-d', '--json', args), stdout=TRUE, stderr=NULL)
}
pdb <- available.packages(repos=findCRANmirror("web"))
solve_ <- function(request){
  cat('Solving:', paste(request, collapse=' '), '\n')
  with_r <- NULL
  exclusions <- rownames(installed.packages(priority="base"))
  pkg <- request
  while(length(pkg) > 0){
    result <- suppressWarnings(
      fromJSON(paste0(conda(paste0('r-', tolower(pkg))), collapse=''))
    )
    is.null(result$exception_name) && break
    stopifnot(result$exception_name == "PackagesNotFoundError")
    missing_pkgs <- sapply(gsub('^r-', '', result$packages), \(x){pkg[x == tolower(pkg)]})
    cat(paste('Not on Conda:', paste(missing_pkgs, collapse=' ')), '\n')
    for (missing_pkg in missing_pkgs){
      # keep it if the user exlicitly wanted this one
      if (missing_pkg %in% request){
        with_r <- union(with_r, missing_pkg)
      }
      # update pkg with the dependencies of the missing package
      if (!(missing_pkg %in% exclusions)){
        deps <- setdiff(unlist(tools::package_dependencies(missing_pkg, db=pdb)), exclusions)
        pkg <- union(setdiff(pkg, missing_pkg), deps)
        exclusions <- union(exclusions, missing_pkg)
      }
    }
  }
  solution <- list(conda=paste0('r-', sort(tolower(pkg))), r=sort(with_r))
  if(length(solution$conda) > 0){
    cat('\nConda:\n conda install', paste(solution$conda, collapse=' '), '\n')
  }
  if(length(solution$r) > 0){
    cat('\nR:\n ', paste0("install.packages(c('", paste(solution$r, collapse="', '"), "'))"), '\n', sep="")
  }
  return(solution)
}

check_names <- function(pkg){
  # check for package availability and fix capitalization of package names
  found <- NULL
  missing <- NULL
  matches <- sapply(pkg, \(x){grep(paste0('^', x, '$'), rownames(pdb), ignore.case=T, value=T)})
  for(m in names(matches)){
    if (length(matches[[m]]) > 0){
      found <- union(found, matches[[m]])
    } else {
      missing <- union(missing, m)
q    }
  }
  list(found=sort(found), missing=sort(missing))
}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) > 0){
  check_pkg <- check_names(args)
  if (length(check_pkg$missing) > 0){
    cat(paste('Not found on CRAN:', paste0(check_pkg$missing, collapse=' '), '\n'))
  } else {
    solution <- solve_(check_pkg$found)
    env_path <- Sys.getenv('CONDA_PREFIX')
    if (env_path != ''){
      cat('\nAn active Conda environment detected. Proceed with installation? [y/N] ')
      if (tolower(substr(readLines("stdin", n=1), 1, 1)) == 'y'){
        system2('conda', c('install', '-y', (if(length(solution$conda) > 0) solution$conda else 'r-base')))
        if(length(solution$r) > 0){
          command <- paste0("\"install.packages(c('", paste(sort(solution$r), collapse="','"),
                            "'), lib='", env_path, "/lib/R/library', repos='https://cloud.r-project.org')\"")
          system2('env', c(paste0('--unset=', grep('^R_', names(Sys.getenv()), value=T)), 'Rscript', '-e', command))
        }
      }
    }
  }
} else {
  cat('Usage: solve_env package1 package2 ...\n')
}
