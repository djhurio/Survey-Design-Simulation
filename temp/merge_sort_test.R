d1 <- data.frame(key.char = "a", key.num = 1:20, stringsAsFactors = F)

merge(d1, d1, sort = T)
merge(d1, d1, by = c("key.char", "key.num"), sort = T)
merge(d1, d1, by = c("key.num", "key.char"), sort = T)

d2 <- data.frame(key.num.1 = 20:1, key.num.2 = 1:20)
merge(d2, d2, sort = T)
merge(d1, d1, by = c("key.char", "key.num"), sort = T)
merge(d1, d1, by = c("key.num", "key.char"), sort = T)
