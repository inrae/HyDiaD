catchToList <- function(expr) {
  val <- NULL
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  myError <- NULL
  eHandler <- function(e) {
    myError <<- e$message
    NULL
  }
  val <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
  list(value = val, warnings = myWarnings, error=myError)
} 

temp <- catchToList({warning("warning 1");warning("warning 2");1})
catchToList(acisturio.Scen1.base)


withWarnings <- function(expr) {
  myWarnings <- NULL
  tm <- list()
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  val <- (withCallingHandlers(expr, warning = wHandler))
  tm <- myWarnings
  list(value = val, warnings = myWarnings, tm = length(myWarnings))
  #return(tm)
} 

#purrr----------------------------------------------------------------
library(purrr)
library(lubridate)

datelist <- list(a = "12/12/2002", b = "12-12-2003", c = "24-03-2005")

# get all the everything
quiet_list <- map(datelist, quietly(mdy))

# find the elements which produced warnings
quiet_list %>% map("warnings") %>% keep(~ !is.null(.))

# or 
quiet_list %>% keep(~ length(.$warnings) != 0)

#Take 3----------------------------------------
dat <- data.frame(k=c(3,5,-2,7), logk=NA, warnMsg=NA, nm = NA)
dat2 <- list()
withCallingHandlers({
  for(i in 1:nrow(dat)){
    w <- length(warnings())
    dat$logk[i] <- log(dat$k[i])
  }
}, warning = function(w){
  dat$warnMsg[i] <<- w$message
  dat2 <- dat$warnMsg[i]
  invokeRestart("muffleWarning")
})

dat
dat2

wWarn <- function(expr) {
  myWarnings <- NULL
  wHandler <- (w) {
    myWarnings <<- c(myWarnings, w$message)
    out$warn[i] <- length(myWarnings)
    invokeRestart("muffleWarning")
  })
  }
})
