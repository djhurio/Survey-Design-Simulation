domeni<-function(X, D, teksts) {

  require(foreach)

   # X
   X<-as.matrix(X)
   n <- nrow(X)
   m <- ncol(X)
   if (any(is.na(X))) stop("X ir nezinamas vertibas")
   if (is.null(colnames(X))) stop(paste(teksts, " nav defineti kolonu nosaukumi"))
   
   # D
   D<-as.matrix(D)
   
   if (nrow(D) != n) stop("D un X nav vienaads rindinju skaits")
   if(is.null(colnames(D))) stop("Matricai D nav defineti kolonu nosaukumi")
  
   Dom0 <- unique(D)
   Dom_agg<-Dom0[do.call(order, lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
   Dom_agg<-as.matrix(Dom_agg)
   colnames(Dom_agg)<-colnames(D)
   L <- nrow(Dom_agg)
   n<-nrow(X)

   domen<-foreach(i = 1:ncol(X), .combine = cbind) %do% {
      X_dom <- foreach(k = 1:L, .combine = cbind) %do% 
        ifelse(rowSums(D == matrix(Dom_agg[k, ], n, ncol(D), T)) == ncol(D), X[,i], 0)}
 
     domen<-as.matrix(domen)
     nosaukD<-function(X,D) {
         d <- colnames(X)
         b <- colnames(D)
         Dom0 <- unique(D)
         Dom_agg<-Dom0[do.call(order, lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
         Dom_agg<-as.matrix(Dom_agg)
         colnames(Dom_agg)<-colnames(D)
         L <- nrow(Dom_agg)
         h <- c()
    
         for (i in 1:L) {
               c <- paste(b, Dom_agg[i,], sep=".")
               h[i] <- do.call(paste, as.list(c(c, sep="_")))}
    
         g <- foreach(i = 1:ncol(X), .combine = cbind) %do% {
                nsk1 <- paste(d[i], h, sep="_")}
    
         s<-paste(as.list(g))
         s }
     colnames(domen) <- nosaukD(X, D)
     domen
}
