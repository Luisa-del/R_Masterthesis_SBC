

## Loading Packages -------------------------------------------------------------------
pkg = c("raster", "randomForest", "caret", "doParallel", 
        "corrplot", "cluster", "vtreat", "ithir")
sapply(pkg, require, character.only=T)


## Functions ----------------------------------------------------------------------------------------------
MODE <- function(dataframe){
  #  dataframe = dd
  #  x
  DF <- as.data.frame(dataframe)
  #x = DF[,96]
  MODE2 <- function(x){      
    if (is.numeric(x) == FALSE){
      df <- as.data.frame(table(x))  
      df <- df[order(df$Freq), ]         
      m <- max(df$Freq)        
      MODE1 <- as.vector(as.character(subset(df, Freq == m)[, 1]))
      sum(df$Freq)
      #if (sum(df$Freq)/length(df$Freq)==1){
      # warning("No Mode: Frequency of all values is 1", call. = FALSE)
      #}else{
      return(MODE1[1])
      #}
      
    }else{ 
      df <- median(x, na.rm = T)  
    }
  }
  return(as.vector(lapply(DF, MODE2)))
}


separa_variaveis_categoricas <- function(df) {
  ###separando as variáveis contínuas das categóricas
  ##
  nc = ncol(df) ##separa as variáveis selecionadas (sem as redundantes)
  vf = logical(nc) ##cria arquivo que separa os campos de fatores do campo de numéricos (arquivo com TRUE e FALSE)
  for (i in 1:nc) {
    vf[i] = is.factor(df[,i])
  } ##coloca num arquivo vf as variáveis que são fatores
  return(vf)
}

comparara_classificadores <- function(d1,index) {
  inicio = Sys.time()
  # m.svmRadial = predicao.formula(df = d1, metodo = "svmRadial", index = index)
  m.rf = predicao.formula(df = d1, metodo = "rf", index = index)
  # m.svm = predicao.formula(df = d1, metodo = "svmPoly", index = index)
  m.gbm = predicao.formula(df = d1, metodo = "gbm", index = index)
  m.lmt = predicao.formula(df = d1, metodo = "LMT", index = index)
  m.C50 = predicao.formula(df = d1, metodo = "C5.0", index = index)
  results = list(rf=m.rf, gbm = m.gbm, lmt =  m.lmt, C50 = m.C50)
  print(Sys.time() - inicio)
  return(results)
  
}

predicao <- function(df,metodo,index) {
  set.seed(313)
  nfolds = 10
  seeds <- vector(mode = "list", length = nfolds + 1)
  for(i in 1:nfolds) seeds[[i]] = sample.int(n=1000, 50)
  seeds[[nfolds + 1]]<-sample.int(1000, 1)
  
  cl <- makePSOCKcluster(8)
  registerDoParallel(cl)
  x = df[,-1]
  y = df[,1]
  
  m <- train(x=x,y=y, method = metodo,tuneLength = 5,
             trControl = trainControl(method = "cv", number = nfolds, index = index,
                                      seeds = seeds))
  stopCluster(cl)
  return(m)
}

predicao.formula <- function(df,metodo,index) {
  set.seed(313)
  cl <- makePSOCKcluster(6)
  registerDoParallel(cl)
  m <- train(classe ~. , data = df, method = metodo,tuneLength = 10,
             trControl = trainControl(method = "cv", number = 10, index = index))
  stopCluster(cl)
  return(m)
}

### reading point files -------------------------------------------------------
setwd("K:/results_poscampo/2015/class_espec_allvars")
dados = read.csv("espectral_allvar2.csv", sep = ",", dec = ".")
colnames(dados)
dados$X = NULL
colnames(dados)

####reading sample file with mode and median values ---------------------------------------------
vpol = unique(dados$RASTERVALU)

colnames(dados)
fatores = c(1,2,15,98:99)
for (i in fatores) {
  dados[,i] = as.factor(dados[,i])
}

for (i in vpol) {
  print(i)
  dd = dados[dados$RASTERVALU == i,]
  classe = data.frame(dd[1]$GRID_CODE[1])
  dd = dd[,-c(1:2)]
  #ddl = MODE(dd)
  #dl = data.frame(t(unlist(ddl)),stringsAsFactors = F)
  md = data.frame(t(unlist(MODE(dd))),stringsAsFactors = F)
  md = cbind(classe,md)
  colnames(md)[1] = "GRID_CODE"
  if (i == 1) { 
    df.med = md  
  } else {
    df.med = rbind(df.med,md)    
  }
  
}

colnames(df.med)
fac = c(1,14,97:98)

df.med1 = df.med
n = ncol(df.med)
for (i in 1:n) {
  cond = i %in% fac
  if (cond == TRUE) {
    df.med[,i] = as.factor(df.med[,i])
  } else {
    df.med[,i] = as.numeric(df.med[,i])
  }
}

colnames(md)
colnames(df.med)


is.na(df.med)  ### checking no datas

df = df.med[complete.cases(df.med),] ########checking for missing fields
colnames(df)

write.csv(df,"G:/results_poscampo/2015/class_espec_allvars/med_moda2.csv")



####reading sample files with mode and median values ---------------------------------------------
setwd("G:/results_poscampo/2015/class_espec_allvars")
total = read.csv("med_moda2.csv")
colnames(total)
total$X = NULL

colnames(total)
total$GRID_CODE = as.factor(total$GRID_CODE)
total$geo = as.factor(total$geo)
total$solo_inc = as.factor(total$solo_inc)
total$solos = as.factor(total$solos)


#total = df
colnames(total) 

is.na(total)  ###verificando a presença de no datas


nz = nearZeroVar(total) ##checking variables with zero variance
nz
total = total[,-nz] ##remove variables with zero variance

## variable separation --------------------------------------------------------------------
vf = separa_variaveis_categoricas(total)  ##separates categorical variables
vf
total.fator = total[,c(vf)]
total.fator$GRID_CODE = NULL
colnames(total.fator)
total.continuo = total[, !c(vf)]
colnames(total.continuo)

## transforming categorical variables into dummy variables ------------------------------------
total.fator$classe = NULL
dmvar <- dummyVars(~., data = total.fator, fullRank = T)
dmvar
total.dum = predict(dmvar, total.fator)
names(total.dum)
dftotal = cbind(total.dum,total.continuo, classe=total[,1])
names(dftotal)


## fseparating data for external validation ---------------------------------------
set.seed(313)
va = createDataPartition(dftotal$classe, times = 1, p = 0.75, list = F)
va

dftest = dftotal[-va,]
dftreino = dftotal[va,] 
dftreino = dftreino %>% dplyr::select(classe, geo.2:slope)
names(dftreino)

##Correlation Analysis and Variable Selection by the rfe method ----------------------
mcor = cor(dftreino[,-1])

vvc = as.integer()
cont = 1
ni = 25

df_rfe_result  = data.frame(i = integer(ni), nvar= integer(ni), list_var = list(ni),  
                            var_eliminadas = list(ni),profile = list(ni), kappa = numeric(ni) )
set.seed(313)
nfolds = 10
seeds <- vector(mode = "list", length = nfolds + 1)
for(i in 1:nfolds) seeds[[i]] = sample.int(n=1000, 50)
seeds[[nfolds + 1]]<-sample.int(1000, 1)
cont = 1
i  = 75

for (i in 75:99) {
  cut = i / 100
  vc = findCorrelation(mcor,cut)
  print (i)
  print(length(vc))
  vvc[cont] = ncol(dftreino[,-1]) - length(vc)
  x = dftreino[,-vc]
  y = dftreino$classe  
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  rfProfile.all <- rfe(x = x[,-1], y = y,  sizes = c(5:20), metric = "Kappa",
                   rfeControl = rfeControl(method = "cv",functions = rfFuncs, number = nfolds, 
                                           seeds = seeds))
  

  print(rfProfile.all)
  dfr = rfProfile.all$results
  df_rfe_result$i[cont] = i
  df_rfe_result$X25.1[cont] = list(names(dftreino)[vc])
  df_rfe_result$X25.2[cont] = list(rfProfile.all)
  df_rfe_result$nvar[cont] = rfProfile.all$optsize
  df_rfe_result$kappa[cont] =  dplyr::filter(dfr,Variables == rfProfile.all$bestSubset)[3]
  df_rfe_result$X25 [cont] = list(rfProfile.all$optVariables)
  print("selecionadas")
  print(rfProfile.all$optVariables)
  stopCluster(cl)
  cont = cont + 1
}


library(gtable)
library(grid)
library(scales)



# two plots
dfr.graf= data.frame(Correlação = df_rfe_result$i, Kappa = unlist(df_rfe_result$kappa))
dfr.graf2= data.frame(Correlação = df_rfe_result$i, Variáveis = unlist(df_rfe_result$nvar))

grid.newpage()

p1 = ggplot(data = dfr.graf, aes(x = Correlação, y = Kappa)) + 
  geom_smooth(colour = 'black') +
  xlab("Correlação") + 
  ylab("Kappa") +theme_bw() +
  theme_gray(base_size = 16) +
  geom_point(shape=15, colour = 'black', size =1)  +
  theme(axis.title.y = element_text(colour = "black")) + facet_wrap(scales = "free" )
  


p2 = ggplot(data = dfr.graf2, aes(x = Correlação, y = Variáveis)) +  
  theme_gray(base_size = 16) +
  geom_point(colour = 'blue') +
  geom_smooth(colour = 'blue') +
  ylab("Número de Variáveis") + 
  theme(axis.title.y = element_text(colour = "blue")) +
  theme(axis.title.y=element_text(angle = 90)) %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, 
                     g2$grobs[[which(g2$layout$name == "panel")]], 
                     pp$t, pp$l, pp$b, pp$l)
g2$layout$name
# axis tweaks
alab <- g2$grobs[[which(g2$layout$name=="ylab-l")]]
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + 
  unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], 
                     length(g$widths) - 1 )
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], 
                     length(g$widths) - 1 )
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 2, pp$b)
g <- gtable_add_grob(g, alab, pp$t, length(g$widths) - 1, pp$b)

grid.draw(g)




## Determining best fit and performing prediction ----------------------------------------------
mx = which.max(df_rfe_result$kappa)
mx
nv = df_rfe_result[mx,1]
nv

df_rfe_result$X25[mx]

## validando com conjunto de dados externos (teste)
fit = df_rfe_result$X25.2[[mx]]
fit

v = predict(fit, dftest)[1]
confusionMatrix(v[,1],dftest$classe)
goofcat(dftest$classe, v[,1])


save.image("K:/5.Doutorado/qualificacao/all_vars.RData")
load("K:/5.Doutorado/qualificacao/all_vars.RData")

## Selecting the threshold with the best and most parsimonious Kappa

sel94 = c("ndvi_seca", "secac4", "secac5", "secac6", "secac3", "chuvac3", "savi_chuva", "secac1",
          "chuvac6", "dif_ndvi", "ed_urbana", "dif_savi", "razao_savi", "solos.5", "mbio16_utm",
          "mprec2_utm", "mbio12_utm")



dsel94 = dftreino[,sel94]
names(dftreino)
dfsel94 = cbind(classe=dftreino$classe, dsel94)
names(dfsel94)


##### making name vector of selected variables and adding .asc extension
nome94 = strsplit(sel94,"[.]")
nrow(nome94)
vsel = character()
vn2 =  character()
lnrow <- sapply(nome94, NROW)
l = length(lnrow)
i = 11
for(i in 1:l) {
  vsel[i] = nome94[[i]][1]
  vsel[i] = paste(vsel[i],".asc",sep = "")
  vn2[i] = vsel[i][1] 
}
vsel

rm(st)

### stacking selected variables --------------------------------------------------------------
setwd(choose.dir())
setwd("K:/results_poscampo/2015/class_espec_allvars/asc15")

st = stack(vsel)
st

names(st) = vn2
st1= as(st, "SpatialPixelsDataFrame")
st2= as.data.frame(st1)
st2 = na.omit(st2)
names(st2)

st2$solos.asc = as.factor(st2$solos.asc)
dmvar2 <- dummyVars(~., data = st2, fullRank = T)
dmvar2

df.pred = predict(dmvar2, st2)
dd= as.data.frame(cbind(st2$x,st2$y,df.pred))
names(dd)
df.final= dd %>% dplyr::select(c(ndvi_seca.asc:razao_savi.asc, solos.asc.5,mbio16_utm.asc:y)) %>% 
  rename(ndvi_seca = ndvi_seca.asc, secac4=secac4.asc, secac5=secac5.asc,
secac6=secac6.asc, secac3=secac3.asc,chuvac3=chuvac3.asc, savi_chuva=savi_chuva.asc, 
secac1=secac1.asc, chuvac6=chuvac6.asc, dif_ndvi=dif_ndvi.asc, ed_urbana=ed_urbana.asc, 
dif_savi=dif_savi.asc,razao_savi=razao_savi.asc,solos.5=solos.asc.5,
mbio16_utm = mbio16_utm.asc, mprec2_utm=mprec2_utm.asc,mbio12_utm=mbio12_utm.asc)

names(df.final)                
names(dfsel90)

### joining coordinate and prediction ---------------------------------------------

df.final =as.data.frame(df.final)
r1 = predict(fit, df.final[,-c(18:19)])
pred = cbind(df.final[,c(18:19)],r1)  
coordinates(pred) = ~x + y

# create raster -------------------------------------------------------------
gridded(pred) <- TRUE
rasterDF <- raster(pred)
plot(rasterDF)
rasterDF[is.na(rasterDF)] = 0

getwd()
writeRaster(rasterDF, "all_vars_valida_externo", format = "GTiff", NAflag = 0 )

save.image("K:/results_poscampo/2015/class_espec_allvars/nao_espectrais.RData")
load("E:/results_poscampo/2015/class_espec_allvars/predicao.RData")


