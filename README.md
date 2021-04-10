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


# Problema B
