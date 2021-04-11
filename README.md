# PF_A_B_C  
## Problemas programação funcional: A, B e C com o Manuel Carvalho
| Done | Problema | Descrição |
| ------ | ------ | ------ |
| Yes | A | A resposta é 42! |
| No | B | Thumbnails |
| No | C | Os trocos trocados |

# Problema A

Vamos jogar um jogo simples que envolve dinheiro. Se eu lhe der uma soma inicial, digamos N euros, deverá devolver-me dinheiro desta soma conforme regras pre-estabelecidas que se aplicam até nenhuma se aplicar mais. Se conseguir, com essas regras ficar com exactamente 42 euros em mão, então ganhou o jogo e fica com esta soma.

## Se assumirmos que tem m euros em mão, as regras são:

(regra 1) se m for par, pode devolver-me m/2 euros;
(regra 2) se m for um múltiplo de 3 ou de 4 então pode multiplicar os dois últimos dígitos de m e devolver-me esta quantidade em euros;
(regra 3) se m for um múltiplo de 5 então pode devolver-me exactamente 42 euros.
Entrada
Uma linha com um inteiro N, a soma inicalmente proposta para o jogo.

## Saída
De duas, uma:
- Uma linha com um inteiro p que representa o número mínimo de passos (o número de regras) que usou para ganhar o jogo;
- Ou uma linha com o texto BAD LUCK, caso não consiga ganhar.
## Limites
0 < N ≤ 1  000  000


-------------------//-------------------//-------------------//-------------------//-------------------//-------------------//-------------------//-----------
# Problema B

## Considere o problema da representação e manipulação eficiente de imagens quadradas.

Uma imagem quadrada, na sua forma básica, é uma matriz n por n de pixeis, em que n é o tamanho da imagem e um píxel é a unidade básica (um ponto) com a informação da cor. Neste exercício vamos considerar que n é uma potencia de dois (1, 2, 4, 8, 16 etc.) e que cada pixel tem uma de duas cores : preta ou branca.

Para a manipulação de uma imagem, é muitas vezes mais cómodo e eficiente usar representações alternativas à representação matricial. É o que faremos neste exercício. Usaremos árvores quaternárias quadtrees para representar estas imagens. Este tipo de árvore está codificado no tipo OCaml image apresentado a seguir.

type color = W | B (* W: White, B: Black *)
type image = L of color (* leaf of one color *)
           | N of image * image * image * image  (* node with four children *)
           
Este formato tem a vantagem de poder compactar a representação de uma imagem (num tamanho menor do que a matriz subjacente) tirando proveito de padrões cromáticos presentes na imagem. A definição do tipo aqui dada o é a título de exemplo. Este poderá ser alterado conforme eventuais necessidades.

A construção da árvore a partir da matriz é feita de forma recursiva. Uma imagem (matriz) é dividida ordenadamente em 4 partes iguais. As partes NW, NE, SE e SW.

<p align="center">
<img src="https://user-images.githubusercontent.com/46536704/114282669-263ac380-9a3d-11eb-867b-de6160fa26be.png">
</p>                                             

## Entrada
A entrada começa pela especificação de uma imagem no formato ppm sem comentários A linha final contém o inteiro p, potência de 2, que indica o tamanho do thumbnail por calcular.

## Saída
Uma linha com o valor inteiro que indica a profundidade da folha mais alta da árvore calculada. Uma linha que indica o número de folhas totais da árvore. Uma matriz p por p que contém o thumbnail calculado. Esta matriz está organizada em p linhas de p inteiros

## Limites
Os valores de n e p são potências de dois. É garantido que 0 < p ≤ n ≤ 1024.

## Exemplo de Entrada

P1
8 8
0 0 0 0 1 0 1 0
0 0 0 0 0 0 0 1
0 0 0 0 0 0 1 1
0 0 0 0 0 1 1 1
0 0 0 0 1 1 1 1
0 0 0 0 1 1 1 1
0 0 0 1 1 1 1 1
0 0 1 1 1 1 1 1
4
## Exemplo de Saída

1
22
0 0 0 1
0 0 0 1
0 0 1 1 
0 1 1 1 
-------------------//-------------------//-------------------//-------------------//-------------------//-------------------//-------------------//-----------
# Problema C

Vamos neste exercício exemplificar as diferenças entre programação dinâmica e algoritmos gulosos. Para tal, olhemos para o problema clássico dos trocos em diversos sistemas monetários, alguns esotéricos.
Assumimos aqui que temos moedas ou notas em quantidade suficiente e que o sistema monetário considerado permite a devolução de qualquer troco. Para dar o troco de 32 euros com o menor número de moedas, basta escolher o máximo número de moedas de maior valor abaixo da quantidade por devolver (32) cuja soma não ultrapassa esta quantidade. Aqui trata-se de uma nota de 20 euros. Restando 12 euros por devolver. Aplicamos a mesma receita com o valor 12 e resulta numa nota de 10 euros e, finalmente com os dois euros restantes, uma moeda de 2 euros.
O processo seguido aqui é um clássico algoritmo guloso. Funciona sempre no sistema monetário do Euro. Mas nem todos os sistemas monetários que existiram ou existentes partilham esta benesse1.
Vejamos o sistema em que há moedas de 1, 3 e 4. Se tivermos de devolver trocos para o valor 6, o método acima descrito escolherá três moedas : uma moeda de 4 e duas de 1. Mas com duas moedas de 3 teríamos o troco devolvido com menos uma moeda.
É possível desenhar uma solução por programação dinâmica que encontra sempre a melhor forma de dar troco em qualquer sistema monetário.
É a confrontação entre estes dois métodos que vamos aqui exibir neste exercício.
Dados a lista ordenada das moedas de base num sistema monetário, a sua tarefa é dar a indicação se neste sistema monetário a solução ao problema dos trocos por algoritmo guloso é coincidente com o algoritmo por programação dinâmica. Se for idêntica a resposta deverá ser YES. Se não for, então deverá devolver a quantidade menor que exibe um comportamento diferenciado nas duas soluções. Por exemplo, no sistema 1,3 e 4, o menor valor em que as duas soluções discordam é precisamente o valor 6 que mencionamos mais acima.

## Entrada
Na primeira linha encontra-se um inteiro n que indica quantas moedas forma o sistema considerado.
Nas n linhas seguintes consta um inteiro que representa o valor numérico de cada uma das moedas do sistema monetário considerado.
Estas moedas são apresentadas por ordem crescente e são todas distintas.

## Saída
Uma linha com:

- a palavra YES caso o sistema monetário em causa permite ao algoritmo guloso concordar com a solução por programação dinâmica;
- ou um inteiro p que é o montante mais baixo para o qual o troco calcula pelos dois algoritmos é discordante.
## Limites
0 < n ≤ 100, e as moedas não ultrapassam o valor 500.
        
