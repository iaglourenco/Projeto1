leArquivos<-function(extencao, diretorio)
  
{
  #arquivo<-NULL
  arquivo <- matrix(nrow=1999,ncol=4097)
  setwd(diretorio)
  nomesArq = list.files(pattern=extencao)
  
  for(i in 1:1999){
    temp<-nomesArq[i]
    nomeArqSep<-unlist(strsplit(x = temp, split = ""))
    categoria<-as.numeric(nomeArqSep[1], base = 0L) 
    #categoria
    
    #categoria<-c("zero", "um", "dois","tres","quatro", "cinco", "seis", "sete", "oito", "nove")
    #categoria[1]
    
    num<-read.csv(file = nomesArq[i], header = FALSE, sep = " ")
    num<-num[-1,]
    num<-num[-1,]
    num<-num[-1,]
    #num
    num<-num[,-18]
    
    linha<-as.vector(t(num))
    linha<-as.numeric(linha, base = 0L)
    linha[4097] = categoria
    #linha
    
    
    colnames(linha) <- NULL
    rownames(linha) <- NULL
    
    
    if(length(linha) > 4097){#Identifica arquivo fora do padrÃ£o
      print(nomesArq[i])
    }else{
      #arquivo <- InsertRow(arquivo, NewRow = linha, RowNum = i)
      #arquivo<-rbind(arquivo, linha)
      #arquivo<-rbind(arquivo, linha, make.row.names=FALSE)
      arquivo[i,]<-linha
    }
    
    
  }
  return(arquivo)
}

numeros<-NULL
numeros<-leArquivos("*.pgm", "./DigitosCompleto")

#Impressao dos numeros------------------------------------------------------------
mat<-matrix(numeros[75,-4097], byrow = TRUE, 64,64)#Transforma vetor em matriz
image(mat, useRaster = TRUE, axes = FALSE)#Imprime conteudo da linha
#---------------------------------------------------------------------------------


set.seed(123)#Mesmo valor em cada execu??o, se o valor n?o for alterado
#Algoritimo KNN-------------------------------------------------------------------

indiceTreino<-sample(1:nrow(numeros), 0.8*nrow(numeros))#Pega elemetos aleatorios

dadosTreino<-numeros[indiceTreino,]
dadosTeste<-numeros[-indiceTreino,]

table(dadosTreino[,4097])#Qtd elementos, por classe
table(dadosTeste[,4097])

especiesTreino<-dadosTreino[,4097]
dadosTreino<-dadosTreino[,-4097]
especiesTeste<-dadosTeste[,4097]
dadosTeste<-dadosTeste[,-4097]

#k = 5, 7, 13
library(class)

modelo<-knn(train = dadosTreino, test = dadosTeste, cl = especiesTreino, k = 1)#CL = classificacao

summary(modelo)
table(especiesTeste)

table(modelo)
table(especiesTeste)

matrizConf<-table(especiesTeste, modelo)
matrizConf
diag(matrizConf)
sum(diag(matrizConf))
sum(matrizConf)

sum(diag(matrizConf)/sum(matrizConf))
  mean(modelo == especiesTeste)

calculaKN<-function(treino, test, espTreino, espTeste, vizinhos)
{
  for(i in 1: length(vizinhos))
  {
    modelo<-knn(train = treino, test = test, cl = espTreino, k = vizinhos[i])
    matrizConf<-table(espTeste, modelo)
    matrizConf
    print(paste("k:", i))
    
    print(sum(diag(matrizConf)/sum(matrizConf)))
  }
}

k<-c(1, 7, 13, 29)
calculaKN(dadosTreino, dadosTeste, especiesTreino, especiesTeste, k)
#---------------------------------------------------------------------------------

#Arvores de decisao
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#transfomacao para dataframe
numeros <- as.data.frame(numeros)

#separo os dados de treino e teste
indiceTreino<-sample(1:nrow(numeros), 0.8*nrow(numeros))#Pega elementos aleatorios
dadosTreino<-numeros[indiceTreino,]
dadosTeste<-numeros[-indiceTreino,]

#gero a arvore
modeloArvore <- rpart(dadosTreino$V4097~.,dadosTreino,method = "class",                      
                      control = rpart.control(minsplit = 1))
summary(modeloArvore)
#ploto a arvore
plot<-rpart.plot(modeloArvore,type = 3,box.palette = list("blue","green","yellow","red","pink","brown","purple","orange","white","cyan"))

#testo a arvore com o dadosTeste
test<-dadosTeste[,-4097]
pred<-predict(modeloArvore,test,type = "class")

#Avalio o desempenho
matrizConf <- table(dadosTeste[,4097],pred)
matrizConf
sum(diag(matrizConf)/sum(matrizConf))

#---------------------------------------------------------------------------------
#SVM

library(e1071)


testaKernels<-function(dadosTreino, teste, kernels)
{
  for(i in 1: length(kernels))
  {
    classifier = svm(formula = dadosTreino$V4097~.,
                     data=dadosTreino,
                     type = 'C-classification',
                     kernel = kernels[i])
    pred = predict(classifier,newdata = teste)
    
    matrizConf<-table(dadosTeste[,4097], pred)
    print(matrizConf)
    print(paste("kernel=", kernels[i]))
    print(sum(diag(matrizConf)/sum(matrizConf)))
  }
}


#tranformo em dataframe
numeros <- as.data.frame(numeros)
#separo os dados de treino e teste
indiceTreino<-sample(1:nrow(numeros), 0.8*nrow(numeros))#Pega elementos aleatorios
dadosTreino<-numeros[indiceTreino,]
dadosTeste<-numeros[-indiceTreino,]

testaKernels(dadosTreino,dadosTeste[,-4097],c("linear","polynomial","radial","sigmoid"))


