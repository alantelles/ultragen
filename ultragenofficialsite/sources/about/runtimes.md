# Tempos de execução de UltraGen

Entender os tempos de execução de UltraGen é recomendado para debugar aplicações e entender alguns detalhes da linguagem.

## Listagem

Este é o primeiro estágio de análise de um template. Nele são listadas todas as linhas que um template deverá ter. O único tipo de interpretação feito neste momento é a extensão de templates. No fluxo normal, cada linha de um template é adicionada à lista de linhas do interpretador. Temos duas possibilidades de quebra de fluxo. A primeira é o operador de multilinha ` ++` (espaço, mais, mais). Sempre que esta declaração é encontrada a listagem concatena a próxima linha com a linha em que foi encontrado o operador multilinha. Isto quer dizer que o operador funciona tanto para trechos de texto plano como para trechos de script. Logo, se seu texto plano apresenta esta combinação de caracteres no final da linha você precisa escapar este primeiro espaço de alguma forma.
A segunda quebra do fluxo é quando é encontrado procedimento `extend`. Neste momento, o template referido é buscado e suas linhas são adicionadas à lista de linhas atual. Literalmente, o template estendido é mesclado ao template que o estendeu.

Ilustrando:

**main.ultra**

```ruby
Linha 1
Outra linha
@extend:ext.ultra
Fim template
```

**ext.ultra**

```ruby
Linha estendida
Outra linha estendida
```

O resultado da listagem será:

```ruby
Linha 1
Outra linha
Linha estendida
Outra linha estendida
Fim template
```

Isto pode parecer um `include`, mas não é. Um `include` também tem sua execução "inserida" em um template, mas em tempo de interpretação enquanto o template listado está sendo executado. Diferente do `extend` em que apenas as linhas são mescladas ao template que estendeu. sem que haja execução de nenhum procedimento.

Um detalhe a se notar é que só se pode usar strings literais nesse instante, pois não há interpretação, não se conhece nenhuma variável e nem o parser do interpretador está ativo. Por isso, caso se vá utilizar o `extend` o endereço deve estar sem aspas e não se pode utilizar funções.

#### NOTA:
- Você pode saber mais sobre o `extend` na sua página própria de documentação.

## Avaliação

Este é um tempo depois da listagem onde já temos todas linhas que serão executadas. A partir daí, gens dados como entrada já são avaliados, mas ainda não temos avaliação de funções ou interpretação de variáveis, ou mesmo das linhas. Na avaliação são levantados *filters*, *bypasses*, *sections* e funções e procedimentos. Filtros e bypasses são avaliados nesse momento para que já haja a decisão de se um template deve ou não ser interpretado antes que se inicie a interpretação. A avaliação de filtros e bypass só é feita no modo `persist`. Caso contrário um template é sempre avaliado, mesmo q haja alguma declaração de filter ou bypass.

#### NOTA:
- Para não permitir a exibição de uma página, como quando criando sistemas web o ideal é utilizar *middlewares*.

O segundo item avaliado são as *sections*. Trechos de template que você pode exibir depois através da função `section`. A utilização típica deste procedimento é exibir uma seção do template extensor dentro do template estendido. Lembrando que como não temos interpretação de template, o nome da section deve ser uma literal, sem aspas.

#### NOTA:
- Mais detalhes sobre isto na própria página de documentação do procedimento `section`. 

As sections são listadas e guardadas internamente no interpretador para serem exibidas quando `section` é chamado.

Subentende-se da forma como `section` é listado que você não deve utilizar sections dentro de sections.

## Interpretação

É neste runtime que o template é efetivamente executado. As linhas listadas são interpretadas em ordem e chamadas às funções e procedimentos são executados. Variáveis também são avaliadas neste momento. Enfim, é nesta hora que temos o processamento do que escrevemos. A saída do programa é enviada ao stream principal. Uma coisa a se atentar é que a saída do procedimento `print` não é enviada ao stream principal de saída. Apenas ao shell que chamou o programa. O procedimento para enviar texto ao stream principal é o `live`. Isto é especialmente útil para fazer *debugs* sem afetar a saída principal do template. Você pode ainda enviar para os dois streams utilizando `tee`.
É notável ainda que como print é enviado direto ao shell sua exibição ocorre antes do fim da interpretação.

#### NOTA:
- Mais sobre print, live e tee nas respectivas páginas de documentação.

## Conclusão

Esta documentação procurou explicar como funvionam os processos de interpretação de um template UltraGen. Isto é útil para entender porque algumas coisas podem não sair como esperado.
