# O problema dos príncipes

#### William Giani Duani Martins

Uma princesa decide se casar. Vários príncipes se candidatam e ela decide escolher aquele que lhe oferecer a maior quantia em dinheiro. Ela está em uma sala, entra-se um príncipe e a diz a quantia cujo a oferece. Caso ela aceite, os príncipes restantes não dirão sua oferta, e ela pode acabar por escolher aquele que não lhe dê a maior oferta. Por outro lado, ela pode recusar alguém que tenha lhe oferecido muito esperando por ter mais, e não se casar com o mais rico. Como ajudar esta princesa na escolhe através do uso de simulações? Uma boa estratégia seria deixar passar os primeiros lances, e, quando se dissesse um lance maior que os anteriores que se passaram, escolher. Mas, qual o **melhor número de lances se deve deixar passar?**

Primeiro iremos definir uma função simples de simulação, cujo irá receber como primeiro argumento o número de príncipes interessados, e como segundo argumento, a quantidade de lances que iremos deixar passar:

```{r}
simular <- function(n,n0)
{
  #realizando amostragem
  ordens <- sample.int(n,n)
  
  #qual o menor índice dentre os que eu pulei?
  #o menor índice será o maior dote
  min <- min(ordens[1:n0])
  
  i = 1
  #enquanto o indice que saiu é maior que o menor visto
  while((ordens[n0+i] > min) && (n0+i < n)){
    i = i + 1
  }
  if(ordens[n0+i] == 1)
  {
    return(1)
  }
  return(0)
}
```

A título didático, já estamos convertendo o melhor lance para um índice de 1 a n. Onde que o melhor lance é o 1, e o pior lance é o n-ésimo. Faremos agora uma função para gerar uma repetições dos lances. Isto é, repetir n vezes a ocasião que deixaríamos passar j lances, e computar quantas vezes teríamos escolhido o melhor valor. (O primeiro maior em comparação aos j lances vistos)

```{r}
library(Rfast)
repeticoes <- function(n_princesas,max_rep=10000)
{
  
  #realizando o nested loop da maneira mais otimizada
  armazenagem <- lapply(seq.int(max_rep), function(i){
    sapply(1:(n_princesas-1),simular,n=n_princesas) 
  })
  
  #juntando todas as listas em uma matriz
  armazenagem <- do.call(cbind,armazenagem)
  
  saida <- as.matrix(rowsums(armazenagem),ncols=1)
  rownames(saida) <- 1:(n_princesas-1)
  return(
    saida
  )
}
```

Para definirmos uma regressão, deveremos ter também uma função cujo extraia qual foi o melhor pulo de cada situação.

```{r}
encontrarExpressao <- function(nmin,nmax)
{
  aux <- sapply(seq.int(nmin,nmax), repeticoes)
  melhorPulo <- lapply(aux,which.max)
  saida <- do.call(rbind,melhorPulo)
  return(
    saida
  )
}
```

Vamos analisar a saída de uma simulação:
```R
resultados <- repeticoes(n_princesas = 12)
   [,1]
1  2509
2  3378
3  3778
4  3902
5  3803
6  3730
7  3296
8  2954
9  2191
10 1580
11  799
```
Esta simulação nos mostra que, decidindo pular apenas 1 príncipe e escolhendo já logo em seguida o maior valor, a princesa teria conseguido o maior lance em 2452 vezes de 10000 tentativas. Por outro lado, perceba que o melhor teria sido escolher pular 4, e então escolher o melhor lance que visse, pois, dessa forma, teria encontrado o melhor príncipe em 3880 vezes de 10000 tentativas.

Esta simulação foi feita considerando um número fixo de 12 príncipes. Mas, e se quisermos avaliar para n casos distintos? Rodar a função se tornou um pouco demorado mas o resultado será visto a seguir:

```{r}
encontrarFormula <- c(1,1,2,2,2,3,3,3,4,4,4,5,5,7,7,7,
                      6,8,8,9,9,10,10,10,11,11,10,13,12,11,
                      12,14,15,14,14,12,14,13,15,15,18,18,17,
                      17,14,17,18,16,18,18,21,21,16,22,21,20,
                      20,21,22,22,24,26,24,22,27,22,24,26,26,
                      31,27,26,30,26,28,31,29,27,32,29,31,29,
                      32,32,37,32,36,35,36,31,38,41,33,36,32,
                      33,37,37)
```

Este é um vetor cujo possui os melhores pulos cujo maximizaram a probabilidade de escolher o melhor príncipe considerando um número de príncipes concorrentes de 3 a 100.

```{r}
library(ggplot2)
dados <- data.frame(princesas=seq.int(98),melhor_pulo=encontrarFormula)

ggplot(dados, aes(x=princesas,y=melhor_pulo)) + 
  geom_point() + 
  labs(x="Número de princesas.", y="Melhor pulo.", title="Relação melhor pulo x número de princesas") +
  geom_smooth(method = lm, col="red")
```

![1566157296855](https://i.imgur.com/VzwtvAp.png)

Fazendo uma análise do gráfico de dispersão, pode se ver a alta correlação entre a quantidade de candidatos e o melhor pulo, pode se perceber também que ao passo de que se aumenta o número de príncipes, a variabilidade do melhor pulo também. Em conclusão, uma boa fórmula em função das simulações para encontrar o pulo que aumente sua probabibilidade de se escolher o maior valor é dada por: ***0.6395960+0.3721541\*n_príncipes***

Para se divertir, pode-se tentar avaliar para qualquer n através da função a seguir:
```{r}
predictMelhorPulo <- function(x)
{
  saida <- 0.6395960 + 0.3721541 * x
  return(
    saida
  )
}
```

Esperamos ter ajudado na vida da princesa e contribuído a você leitor, com algumas dicas de otimização com o R.
