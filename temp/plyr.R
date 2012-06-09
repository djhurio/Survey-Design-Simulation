### plyr

require(plyr)

mlply(cbind(1:4, 4:1), rep)


ldply(1:10, run, design.1, .progress = "tk", .parallel = F)
