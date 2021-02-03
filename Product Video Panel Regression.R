installed.packages(c("readxl","plm"))
library(readxl)
library(plm)

# Regression on full data set for focal products
full<-read_excel("full.xlsx")
full_plm<-plm(Sales~VidWk+ProdPrice+factor(ProdCat)+factor(Wk)+PriceDiscWk+EmailWk+CatalogWk+HomePgWk+CatPgWk,data = full, index=c("ProdID","Wk"),model="within")
summary(full_plm)
full_se<-as.data.frame(sqrt(diag(vcovHC(full_plm, method="arellano", type="HC1"))))

# Regression on Pre data set for focal products
pre<-read_excel("pre.xlsx")
pre_plm<-plm(Sales~Vid+ProdPrice+factor(ProdCat)+PriceDiscWk+EmailWk+CatalogWk+HomePgWk+CatPgWk,data = pre, index=c("ProdID","Wk"),model="pooling")
summary(pre_plm)

# Regression on Vid data set for focal products
vid<-read_excel("vid.xlsx")
vid_plm<-plm(Sales~Vid+ProdPrice+factor(ProdCat)+PriceDiscWk+EmailWk+CatalogWk+HomePgWk+CatPgWk,data = vid, index=c("ProdID","Wk"),model="pooling")
summary(vid_plm)

# Regression on Post data set for focal products
post<-read_excel("post.xlsx")
post_plm<-plm(Sales~Vid+ProdPrice+factor(ProdCat)+PriceDiscWk+EmailWk+CatalogWk+HomePgWk+CatPgWk,data = post, index=c("ProdID","Wk"),model="pooling")
summary(post_plm)

# Regression on full data set for coordinating products
fullcp<-read_excel("fullcp.xlsx")
fullcp_plm<-plm(CpSales~VidWk+factor(Wk)+FpPriceDiscWk+FpEmailWk+FpCatalogWk+FpHomePgWk
                +FpCatPgWk+CpPriceDiscWk+CpEmailWk+CpCatalogWk+CpHomePgWk
                +CpCatPgWk,data = fullcp, index=c("CpordID","Wk"),model="within")
summary(fullcp_plm)
fullcp_se<-as.data.frame(sqrt(diag(vcovHC(fullcp_plm, method="arellano", type="HC1"))))

# Regression on pvid (pre=vid) data set for coordinating products
pvid<-read_excel("pvid.xlsx")
pvid_plm<-plm(CpSales~VidWk+factor(Wk)+FpPriceDiscWk+FpEmailWk+FpCatalogWk+FpHomePgWk
               +FpCatPgWk+CpPriceDiscWk+CpEmailWk+CpCatalogWk+CpHomePgWk
               +CpCatPgWk,data = pvid, index=c("CpordID","Wk"),model="within")
summary(pvid_plm)

# Regression on vidp (vid+post) data set for coordinating products
vidp<-read_excel("vidp.xlsx")
vidp_plm<-plm(CpSales~VidWk+factor(Wk)+FpPriceDiscWk+FpEmailWk+FpCatalogWk+FpHomePgWk
              +FpCatPgWk+CpPriceDiscWk+CpEmailWk+CpCatalogWk+CpHomePgWk
              +CpCatPgWk,data = vidp, index=c("CpordID","Wk"),model="within")
summary(vidp_plm)

# Regression on vid data set for coordinating products
vidcp<-read_excel("vidcp.xlsx")
vidcp_plm<-plm(CpSales~VidWk+factor(Wk)+FpPriceDiscWk+FpEmailWk+FpCatalogWk+FpHomePgWk
               +FpCatPgWk+CpPriceDiscWk+CpEmailWk+CpCatalogWk+CpHomePgWk
               +CpCatPgWk,data = vidcp, index=c("CpordID","Wk"),model="pooling")
summary(vidcp_plm)


# Regression with interaction for focal products
full_plm_i<-plm(Sales~VidWk+ProdPrice+factor(ProdCat)+factor(Wk)+PriceDiscWk+EmailWk+CatalogWk+HomePgWk+CatPgWk+VidWk*(PriceDiscWk+EmailWk+CatalogWk+HomePgWk+CatPgWk),data = full, index=c("ProdID","Wk"),model="within")
summary(full_plm_i)
full_se_i<-as.data.frame(sqrt(diag(vcovHC(full_plm_i, method="arellano", type="HC1"))))

# Regression with interaction for coordinating products
fullcp_plm_i<-plm(CpSales~VidWk+factor(Wk)+FpPriceDiscWk+FpEmailWk+FpCatalogWk+FpHomePgWk
                +FpCatPgWk+CpPriceDiscWk+CpEmailWk+CpCatalogWk+CpHomePgWk
                +CpCatPgWk+VidWk*(FpPriceDiscWk+FpEmailWk+FpCatalogWk+FpHomePgWk
                                  +FpCatPgWk+CpPriceDiscWk+CpEmailWk+CpCatalogWk+CpHomePgWk
                                  +CpCatPgWk),data = fullcp, index=c("CpordID","Wk"),model="within")
summary(fullcp_plm_i)
fullcp_se_i<-as.data.frame(sqrt(diag(vcovHC(fullcp_plm_i, method="arellano", type="HC1"))))

