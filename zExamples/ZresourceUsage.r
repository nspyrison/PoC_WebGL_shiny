##### Session level resource profiling.

### Memory and obj sizes -----
# http://adv-r.had.co.nz/memory.html

#install.packages("pryr")
library(pryr)

object_size(1:10)
object_size(ls())

mem_used() ## 214MB;; 214/531.2 =~ .4 of what Task Manager Claims; after running shiny and RGL.

### CPU usage -----
# https://stackoverflow.com/questions/47318401/r-how-to-check-how-many-cores-cpu-usage-available
## Linux
splitted <- strsplit(system("ps -C rsession -o %cpu,%mem,pid,cmd", intern = TRUE), " ")
df <- do.call(rbind, lapply(splitted[-1], 
                            function(x) data.frame(
                              cpu = as.numeric(x[2]),
                              mem = as.numeric(x[4]),
                              pid = as.numeric(x[5]),
                              cmd = paste(x[-c(1:5)], collapse = " "))))
df
## try to return windows CPU
a <- system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime", intern = TRUE)
df <- do.call(rbind, lapply(strsplit(a, " "), function(x) {x <- x[x != ""];data.frame(process = x[1], cpu = x[2])}))
df[grepl("Rgui|rstudio", df$process),]
## Doesn't seem accurate; Task manager claiming near 97% usage. restarting the R session made it drop quickly.
# process cpu
# 124 rstudio   0


### 
warning("After using shiny and rgl, CPU usage is need 97% for 1 rstudio session, even after rgl.close() and clearing Viewer pane.")
message("restarting R session Rstudio hotkey (Cntl + Shift + F10) or call q(), clears CPU quickly.")

# For restarting R from code see:
?rstudioapi::restartSession()
install.packages("startup")
library(startup)
?startup::restart()
if (F) {
  rstudioapi::restartSession()
}