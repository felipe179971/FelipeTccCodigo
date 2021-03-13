###################### Fun??o
teste<-function(dataset,var1,var2,alpha,graphic,ANOVA){
  
  #ANOVA
  modelo<-aov(dataset[[var1]]~as.factor(dataset[[var2]]),dataset)
  
  #Dados "fixos" do Scott-Knott
  v<-modelo[["df.residual"]] 
  MSE<-summary(modelo)[[1]][["Mean Sq"]][[2]]
  
  #Scott-Knott
  #S? fazer se rejeitou o teste da ANOVA
  #  if (summary(modelo)[[1]][["Pr(>F)"]][[1]]<alpha){
  #    R<-c("--- P-value is greater than the significance level, you do not have enough evidence to reject the null hypothesis that the population means are all equal---")
  #  }else{
  
  #Fun??o para o Scott-Knott
  scott<-function(dataset,var1,var2,alpha,v,MSE){
    yi<-sort(tapply(dataset[[var1]],dataset[[var2]],mean,na.rm=TRUE),decreasing=TRUE)
    bar_y<-mean(yi)
    k<-length(yi)
    #Fun??o para pegar o n de cada Tratamento sem contar o NA 
    length.na.rm <- function(x){sum(!is.na(x))}
    #S2 modificado por Conrado
    s2_c<-mean(MSE/tapply(dataset[[var1]],dataset[[var2]],length.na.rm), na.rm=TRUE)
    
    library(purrr)
    
    #Lista com a quantidade de subconjuntos poss?veis (k-1)
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
      #F?rmula do B0=T1^2/k1 + T2^2/k2 - (T1+T2)^2/(k1+k2)
      list( (T1^2/k1) + (T2^2/k2) - (sum(c(T1,T2))^2)/sum(c(k1,k2)),
            subG[[x]])
    })) 
    #Pegando o maior B0.
    B0max<-B0[which.max(map(B0,1))][[1]][[1]]
    #Pegando o conjunto que d? o maior B0
    SubB0max<-B0[which.max(map(B0,1))][[1]][[-1]]
    #Calculando o sigma=( sum[(yi-y_barra)^2] + v*s2_c) / (k+v)
    sigma<-(sum((yi-bar_y)^2)+(v*s2_c))/(k+v) 
    #Calculando o lambda=pi / 2*(pi-2) * B0max/sigma (Estat?stica do Teste)
    lambda<-(pi/(2*(pi-2)))*(B0max/sigma)
    #Calculando o Qui-Quadrado
    limite<-qchisq((1-alpha), df = k/(pi - 2))
    # Se a Estat?stica do teste lambda>Qui-Quadrado, rejeito H0
    rejeita<-ifelse(lambda>limite, TRUE, FALSE) 
    #Sa?da
    return( list(rejeita=rejeita,SubB0max=SubB0max) )
  }
  
  #Rodando o Teste 
  
  dados<-dataset
  dados$Treatment<-dataset[[var2]]
  dados$y<-dataset[[var1]]
  i<-1;Group<-1
  dados$Group<-1
  
  #Rodando o teste para todas as parti??es poss?veis e classificando os grupos
  #Grupo 1,2,.. at? quantos Grupos tenho (vari?vel)
  while(i <=max(Group)){ 
    #Se tenho mais de 1 tratamento no Grupo, ent?o posso fazer o teste
    if(length(unique(dados[which(dados$Group==i),]$Treatment))>1){
      #Rodando o teste de sccott-knott para o grupo i 
      Segundo<-scott(dados[which(dados$Group==i),],var1,var2,alpha,v,MSE)
      #Se rejeito H0, atribuo os Grupos
      if(Segundo[[1]][[1]]==TRUE){ 
        #Ser? atribuido com um n?mero diferente de qualquer um j? existente
        Group<-c(as.numeric(max(dados$Group))+1,as.numeric(max(dados$Group))+2)
        #O c?digo divide em 2 Grupos (Faz pro 1 e depois pro 2)
        for(j in 1:2){ 
          #Pega do primeiro ao ?ltimo tratamento pertencente a esse grupo
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
        #Caso tenha rejeitado H0 e o grupo >1, o Grupo do "while" ? atualizado, 
        Group<-as.numeric(levels(as.factor(dados$Group)))
      }
    }
    #Atualizando o i do "while
    i<-i+1
  }
  #Tabela com os dados
  library(dplyr)
  #Olhando o resultado
  R<-dados %>%group_by(Treatment,Group)%>%
    summarise(`Mean`=round(mean(y,na.rm=TRUE),2),
              min=min(y,na.rm=TRUE),
              max=max(y,na.rm=TRUE))%>%
    arrange(desc(`Mean`))
  #Ordem ser? usado unicamente no gr?fico (n?o imprimir ao usu?rio)
  R$Ordem<-as.numeric(row.names(R))
  #No gr?fico ? importante o grupo ser um fator para as cores n?o serem de calor
  R$Group<-as.factor(R$Group)
  #Gr?fico
  if(graphic==T){
    library(plotly)
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
  R<-R[,-6]
  #}
  if(ANOVA==T){
    print("############ANOVA#############")
    print(summary(modelo))
    print("#########Scott-Knott##########")
    
  }
  return(R) 
}
###########################Teste
taus=c(2,2,-2,-2,4,4,-4,-4,9,8,-9,-8)
n<-length(taus)
mu=2
repet<-4 #blocos
Tratamento<-as.factor(rep(c(paste("trat",seq(1:n))),repet))
erro<-rnorm(repet*n,0,1)
y<-mu+taus+erro
#Criando 3 missings aleat?rios (Sorteando n?meros aleat?rios Dist. Uniforme)
y[round(runif(3,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

teste(dados,"y","Tratamento",0.05,graphic=T,ANOVA=T)
###########################Simula??o

simulacao<-function(taus,repet,mu,ausentes,mu_erro,sigma_erro,alpha,repeticao,grupos){
  n<-length(taus)
  Treatment<-as.factor(rep(c(seq(1:n)),repet))
  recebe_NA<-round(repet*n*(ausentes/100),0)
  resultado<-c()
  
  for(i in 1:repeticao){
    erro<-rnorm(repet*n,mu_erro,sigma_erro)
    y<-mu+taus+erro
    y[round(runif(recebe_NA,min=1,max=length(y)),0)]<-NA
    dados<-data.frame(y,Treatment)
    scott<-data.frame(teste(dados,"y","Treatment",alpha,graphic=F,ANOVA=F))
    
    rejeitaH0<-ifelse(length(levels(as.factor(scott$Group)))==grupos,paste0("Grupos=",grupos),"Diferente")
    resultado[i]<-(rejeitaH0)
  }
  return(prop.table(table(resultado)))
}
#Erro Tipo I (Rejeitar quando H0 verdadeiro)
simulacao(taus=c(0,0,0,0),
          repet=9, #Blocos
          mu=2,
          ausentes=1, #em porcentagem ("ausentes=1" = 1% de y vai receber NA de forma aleat?ria"
          mu_erro=0, #N?o precisa
          sigma_erro=1, 
          alpha=0.10,
          repeticao=100, #Quantidade de amostras
          grupos=1 #(2,2),(-2,-2) ser? 4 tratamentos em 2 grupos
)

#OBS.: N?o consegui me livrar da menssagem do dplyr: `summarise()` has grouped output by 'Treatment'. You can override using the `.groups` argument.

