# Tipos de dados

UltraGen é uma linguagem de tipagem dinâmica no nível do desenvolvedor. Porém UltraGen tem uma particularidade decorrente do seu principal objetivo. Como é uma linguagem pensada principalmente para processamento de textos todas as variáveis e valores gen são armazenados como string. Isso não quer dizer que não seja possível fazer operações com outros tipos. UltraGen manipula os tipos em tempo de execução. Isso quer dizer que quando você precisa fazer uma operação matemática o número que está armazenado como string é convertido para real e a operação é feita.

#### INSIDE:

- O conjunto de variáveis de um template é implementado como um array de elementos do tipo `TKVPair`. Como o intuitivo nome diz, `TKVPair` é um **record** composto de duas strings, uma chamada **Key** e outra **Value**. Os valores gen também são armazenados numa lista deste tipo.

#### NOTA:

- Não há coerção de tipos em UltraGen. Em caso de mistura de valores numéricos e alfanuméricos na mesma operação você terá **o silêncio como resposta**.

Sendo assim, temos três tipos "expostos" ao desenvolvedor.

## Tipos numéricos

Como já foi dito, mesmo números são armazenados como strings internamente. Porém, a sintaxe para declará-los é similar a qualquer linguagem comum.

```ruby
@aInt=10
@aFloat=3.5
```

Quando necessário em operações matemáticas ou lógicas eles são convertidos para inteiros ou reais apenas no contexto da função executada. Em outras palavras, você não precisa se preocupar com transformações de tipos. O valor que você quer operar sempre terá o tipo ideal para a operação e continuará guardado como string internamente.

## Tipo String

Esta exposição guarda uma string comum. Lembrando que strings são delimitadas em UltraGen unicamente por aspas simples e se você precisar usar aspas simples na sua declaração basta escapá-la com **\**.

São declarações válidas:

```ruby
@aVar='teste'
Para interpolar: {{ 'uma string' }}
```

## Tipo *listable*

Como tudo é representado como string, naturalmente não temos listas como em linguagens tradicionais. O que tipo exposto para esta utilização chama-se **listable**. Como o nome diz, pense neste tipo como uma string com "potencial" para ser uma lista. Na prática, uma string com algum caractere que pode ser escolhido como separador.

Todas as funções e procedimentos da linguagem com características de manipulação de listas possuem essa filosofia implementada. Em outras palavras, quando você utiliza um `explode`, ela tratará o **subject** como um listable.

```ruby
@{
    myListable='value 1,value 2,value 3'
    explode:@myListable,elem
    print:@elem[0]
    print:@elem[1]
    print:@elem[2]
}
```

Imprimirá
```hljs
value 1
value 2
value 3
```

Um laço for também opera sobre um listable. O seguinte script teria a mesma saída do anterior.

```ruby
@{
    myListable='value 1,value 2,value 3'
    for:@myListable,elem
        print:@elem
    endFor
}
```

Outra coisa a ser notada é que você pode usar qualquer caractere como separador e definir nas funções que usam listas qual o separador deseja utilizar.

```ruby
@{
    myListable='value 1|value 2|value 3'
    explode:@myListable,elem,'|'
    print:@elem[0]
    print:@elem[1]
    print:@elem[2]
}
```

Um texto com os itens separados por linha também pode ser manipulado como um listable através da declaração de `LINE` como separador. Você pode carregar um texto através de loadText e iterar pelas linhas do texto através do uso deste separador.

Seja o texto:
```hljs
Este é um texto plano
Esta É A linha 2
LINHA 3
```

O seguinte script:
```ruby
@{
    loadText:'path/to/text.txt',TXT
    for:@txt,t,LINE
		print:lower(@t)
    endFor
}
```

Imprimirá:
```hljs
este é um texto plano
esta é a linha 2
linha 3
```

## Datas

Datas também são armazenadas como strings e convertidas para um formato interno de data apenas em tempo de execução. Para utilizar as funções de data você pode utilizar uma string que atenda no máximo a um formato como no exemplo abaixo.

`2020-01-10 15:34:23.214`

Se você utilizar a string `2010-01-10`, por exemplo, o resto dos caracteres será completado internamente por zeros. Isto quer dizer, que qualquer string de data precisa começar pelo ano e seguir este formato. Se você precisar de outro formato customizado, você pode fazer isso através da função `customDate(format,date)`. Com esta função, a formatação de data é livre e é escaneada pelo formato em "format". Teremos uma explicação mais detalhada sobre formatos de data numa página específica.

## Sobre tipos lógicos

Os valores aceitos como tipos lógicos são **true** e **false**, unicamente.
Outros valores como 0, -1 ou outros típicos *falsy values* não são naturalmente aceitos como falsos. Isto se deve ao fato de UltraGen lidar principalmente com strings. Afinal de contas, true e false são essencialmente strings. Logo, quando definimos uma variável como true ou false, estamos na verdade atribuindo a ela uma string com valor true ou false. Logo, so se fazer comparações deve se ter em mente o contexto no qual se deseja fazer a comparação. Talvez, no contexto de sua aplicação, uma string com valor 'false' não seja necessariamente um *falso booleano*.

Justamente para diminuir esse risco de inconsistências houve a opção de aceitar apenas estes dois valores como valores lógicos. Isto quer dizer que todas as funções lógicas da linguagem retornarão apenas **true** ou **false**. Se houvesse coerção de outras expressões para true ou false a chance de ambiguidade seria maior ainda. De toda forma, existe uma função de cast para valores lógicos. `bool(value)` retorna **false** para o inteiro -1 e **true** para qualquer outro número. E no caso de strings retorna **false** para uma string vazia e **true** para qualquer outro valor.

#### NOTA:

- Por padrão, pela filosofia de não quebrar a renderização do template, quando uma variável não é encontrada ou temos uma função cujo resultado é negativo, como quando se chama `file(path)` e o arquivo não existe, temos **o silêncio como resposta**. Por isso, uma string vazia é entendida como um valor falso quando é feito o cast para boolean.

## Conclusão

Neste doc foram apresentados os tipos utilizados em UltraGen e a forma como tudo é armazenado, com exceção de datas cujas informações estarão em uma página específica.
