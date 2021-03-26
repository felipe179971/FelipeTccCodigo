#Carregando os pacotes
dependency<-c("purrr",
              "dplyr",
              "plotly")
################################
package.check <- lapply(
  dependency,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#Função com a ANOVA
ANOVA<-function(dataset,var1,var2){
  modelo<-aov(dataset[[var1]]~dataset[[var2]])
  return(modelo)
}
#Função para o Scott-Knott
scott<-function(dataset,var1,var2,alpha,v,MSE){
  yi<-sort(tapply(dataset[[var1]],dataset[[var2]],mean,na.rm=TRUE),decreasing=TRUE)
  bar_y<-mean(yi)
  k<-length(yi)
  #Função para pegar o n de cada Tratamento sem contar o NA 
  length.na.rm <- function(x){sum(!is.na(x))}
  #S2 modificado por Conrado
  s2_c<-mean(MSE/tapply(dataset[[var1]],dataset[[var2]],length.na.rm), na.rm=TRUE)
  
  #Lista com a quantidade de subconjuntos possíveis (k-1)
  sub<-as.list(1:(k-1))
  #Criando os subGrupos 
  subG<-map(sub, function(sub){list(yi[c(1:sub)], yi[-c(1:sub)])}) 
  #Calculando B0 para cada um dos subGrupos
  B0<-as.list(map(as.numeric(sub),function(x) {
    #Definindo
    T1<-sum(subG[[x]][[1]])
    T2<-sum(subG[[x]][[2]])
    k1<-length(subG[[x]][[1]])
    k2<-length(subG[[x]][[2]])
    #Fórmula do B0=T1^2/k1 + T2^2/k2 - (T1+T2)^2/(k1+k2)
    list( (T1^2/k1) + (T2^2/k2) - (sum(c(T1,T2))^2)/sum(c(k1,k2)),
          subG[[x]])
  })) 
  #Pegando o maior B0.
  B0max<-B0[which.max(map(B0,1))][[1]][[1]]
  #Pegando o conjunto que dá o maior B0
  SubB0max<-B0[which.max(map(B0,1))][[1]][[-1]]
  #Calculando o sigma=( sum[(yi-y_barra)^2] + v*s2_c) / (k+v)
  sigma<-(sum((yi-bar_y)^2)+(v*s2_c))/(k+v) 
  #Calculando o lambda=pi / 2*(pi-2) * B0max/sigma (Estatística do Teste)
  lambda<-(pi/(2*(pi-2)))*(B0max/sigma)
  #Calculando o Qui-Quadrado
  limite<-qchisq((1-alpha), df = k/(pi - 2))
  # Se a Estatística do teste lambda>Qui-Quadrado, rejeito H0
  rejeita<-ifelse(lambda>limite, TRUE, FALSE) 
  #Saída
  return( list(rejeita=rejeita,SubB0max=SubB0max) )
}
#Função que transforma os números em letras ordenadas
number_to_letters<-function(a){
  j<-1
  modificado<-c()
  for (i in 1:(length(a)-1)) {
    letras<-letters[1:(length(a)-1)]
    modificado[1]<-letters[1]
    if(a[i+1]==a[i]){
      modificado[i+1]<-modificado[i]
      j<-j
    }else{
      modificado[i+1]<-letters[j+1]
      j<-j+1
    }
  }
  return(modificado)
}
#Função para rodar o Scott-Knott
RUNscott<-function(dataset,var1,var2,alpha,v,MSE){
  dados<-dataset
  dados$Treatment<-dataset[[var2]]
  dados$y<-dataset[[var1]]
  i<-1;Group<-1
  dados$Group<-1
  
  #Rodando o teste para todas as partições possíveis e classificando os grupos
  #Grupo 1,2,.. até quantos Grupos tenho (variável)
  while(i <=max(Group)){ 
    #Se tenho mais de 1 tratamento no Grupo, então posso fazer o teste
    if(length(unique(dados[which(dados$Group==i),]$Treatment))>1){
      #Rodando o teste de sccott-knott para o grupo i 
      Segundo<-scott(dados[which(dados$Group==i),],var1,var2,alpha,v,MSE)
      #Se rejeito H0, atribuo os Grupos
      if(Segundo[[1]][[1]]==TRUE){ 
        #Será atribuido com um número diferente de qualquer um já existente
        Group<-c(as.numeric(max(dados$Group))+1,as.numeric(max(dados$Group))+2)
        #O código divide em 2 Grupos (Faz pro 1 e depois pro 2)
        for(j in 1:2){ 
          #Pega do primeiro ao último tratamento pertencente a esse grupo
          for (k in 1:length(Segundo[[2]][[j]])) {
            #Olha todas as linhas dos dados para ver se na th linha temos o tratamento
            for(nr in 1:nrow(dados)){
              #Coloca o grupo na linha do seu tratamento
              if(is.element(dados$Treatment[nr], names(Segundo[[2]][[j]]))==TRUE){
                dados$Group[nr]<-Group[j]
              }
            }
          }
        }
        #Caso tenha rejeitado H0 e o grupo >1, o Grupo do "while" é atualizado, 
        Group<-as.numeric(levels(as.factor(dados$Group)))
      }
    }
    #Atualizando o i do "while
    i<-i+1
  }
  #Tabela com os dados
  #Olhando o resultado
  R<-dados %>%group_by(Treatment,Group)%>%
    summarise(`Mean`=round(mean(y,na.rm=TRUE),2),
              min=min(y,na.rm=TRUE),
              max=max(y,na.rm=TRUE))%>%
    arrange(desc(`Mean`))
  #Ordem será usado unicamente no gráfico (não imprimir ao usuário)
  R$Ordem<-as.numeric(row.names(R))
  #Transformando de número para letra ordenada
  R$Group<-c(number_to_letters(as.numeric(R$Group)))
  #No gráfico é importante o grupo ser um fator para as cores não serem de calor
  R$Group<-as.factor(R$Group)
  return(R)
}
#Função para o gráfico
Graphic<-function(R){
  RR<-R%>%
    ggplot(aes(Treatment=Treatment,y=Ordem, x=`Mean`,color=Group)) +
    geom_point() +
    scale_y_continuous(breaks=c(R$Ordem),label=c(as.character(R$Treatment)))+
    geom_errorbar(aes(xmin=min,xmax=max),width = 0.2,size  = 0.7) +
    theme_bw()+
    labs(title = "Scott-Knott") +
    theme(plot.title = element_text(size = 9, colour = "#7f7f7f"))
  
  Grafico<-ggplotly(RR,tooltip = c("Treatment","x","Group"))%>%
    layout(title = "Scott-Knott",
           xaxis = list(title = "Mean",titlefont=list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")),
           yaxis = list(title = ""),
           showlegend = FALSE)
  print(Grafico)
}

###################### Função Unbalanced Scott Knott (USK)
USK<-function(dataset,var1,var2,alpha,graphic,ANOVA){
  if(is.factor(dataset[[var2]])==FALSE){
    stop('The argument "var2" must be factor')
  }else if(is.numeric(dataset[[var1]])==FALSE){
    stop('The argument "var1" must be numeric')
  }else if(length(unique(dataset[[var2]]))<2){
    stop('The variable "var2" must have more than 1 treatment')
  }else if(max(table(dataset[[var2]]))<2){
    stop('The variable "var2" must have more than 1 observations')
  }else{
    #Scott-Knott
    #Só fazer se rejeitou o teste da ANOVA
    #  if (summary(modelo)[[1]][["Pr(>F)"]][[1]]<alpha){
    #    R<-c("--- P-value is greater than the significance level, you do not have enough evidence to reject the null hypothesis that the population means are all equal---")
    #  }else{
    
    #Gráfico
    if(graphic==T){
      Graphic(as.data.frame(RUNscott(dataset,var1,var2,alpha,ANOVA(dataset,var1,var2)[["df.residual"]],summary(ANOVA(dataset,var1,var2))[[1]][["Mean Sq"]][[2]])))
    }
    #ANOVA
    if(ANOVA==T){
      print("##########################ANOVA###########################")
      print(summary(ANOVA(dataset,var1,var2)))
      print("#######################Scott-Knott########################")
      
    }
    #Retorna a tabela com o resultado Scott
    return(RUNscott(dataset,var1,var2,alpha,ANOVA(dataset,var1,var2)[["df.residual"]],summary(ANOVA(dataset,var1,var2))[[1]][["Mean Sq"]][[2]])[,-6]) 
  }
}
###########################Teste
taus=c(4,4,-4,-4,9,-9)
n<-length(taus)
mu=2
repet<-3 #blocos
Tratamento<-as.factor(rep(c(paste("trat",seq(1:n))),repet))
erro<-rnorm(repet*n,0,1)
y<-mu+taus+erro
#Criando 3 missings aleatórios (Sorteando números aleatórios Dist. Uniforme)
y[round(runif(3,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

####Para testar os avisos
#y não numerico
dados$y2<-as.character(dados$y)
#Tratamento não fator
dados$Tratamento2<-as.character(dados$Tratamento)
#Tratamento só com 1 fator
dados$Tratamento3<-as.factor(c(1))

#Rodando
USK(dados,"y","Tratamento",0.05,graphic=T,ANOVA=T)

###########################Teste de velocidade
library(profvis)
profvis({USK(dados,"y","Tratamento",0.05,graphic=T,ANOVA=T)})

###########################Simulação

simulacao<-function(taus,observacoes,mu,ausentes,mu_erro,sigma_erro,alpha,iteracoes,grupos){
  numero_trat<-length(taus)
  Treatment<-as.factor(rep(c(seq(1:numero_trat)),observacoes))
  #ceiling: Arrendonda para cima
  recebe_NA<-ceiling(observacoes*numero_trat*(ausentes/100))
  resultado<-c()
  
  for(i in 1:iteracoes){
    erro<-rnorm(observacoes*numero_trat,0,sigma_erro)
    y<-mu+taus+erro
    #ceiling: Arrendonda para cima
    y[ceiling(runif(recebe_NA,min=1,max=length(y)))]<-NA
    dados<-data.frame(y,Treatment)
    scott<-data.frame(USK(dados,"y","Treatment",alpha,graphic=F,ANOVA=F))
    
    rejeitaH0<-ifelse(length(levels(as.factor(scott$Group)))==grupos,paste0("Grupos=",grupos),"Diferente")
    resultado[i]<-(rejeitaH0)
  }
  return(prop.table(table(resultado)))
}
#Erro Tipo I (Rejeitar quando H0 verdadeiro)
simulacao(taus=c(2,2,-2,-2),
          #Número de observações (ou blocos) em cada tratamentos
          observacoes=9,
          mu=2,
          ausentes=1, #em porcentagem ("ausentes=1" = 1% de y vai receber NA de forma aleatória"
          sigma_erro=1,
          #Erro Tipo I
          alpha=0.05,
          #Quantidade de casos a serem avaliados
          iteracoes=100,
          #Exemplo, se taus=([2,2],[-2,-2]) serão 4 tratamentos em 2 grupos
          grupos=2 
)

#OBS.: Não consegui me livrar da menssagem do dplyr: `summarise()` has grouped output by 'Treatment'. You can override using the `.groups` argument.

