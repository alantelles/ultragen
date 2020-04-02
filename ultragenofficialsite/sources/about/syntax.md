# Sintaxe

UltraGen faz parse de seus arquivos-fonte linha a linha. Isto quer dizer que não pode haver quebras de linha numa declaração. Qualquer linha é considerada, incluindo trechos de texto plano. Linhas em branco não são interpretadas, a não ser que `renderBlank` seja declarado.

As declarações básicas em UltraGen são:

- Impressões
- Variáveis
- Procedimentos

## Impressões

Impressões sempre retornam um valor e podem ser embutidas em qualquer lugar de uma linha de texto plano através dos operadores de interpolação.

```ruby
Minha linha de texto plano escrita às {{ date() }}
{{ algumValor }} ou {{ &algumGen.algumValor }}
Linha escrita por {{ @nomeAutor }}
```

Neste trecho de código, vemos os três tipos de fontes que existem na linguagem. Funções - *date()* -, valores **gen** - *algumValor* - e variáveis - *@nomeAutor*. Como já deve ter sido notado, está sintaxe é um padrão para todas as chamadas do mesmo tipo. Temos uma explicação do que são estes dados na página "Fontes de dados em UltraGen".

Você pode trocar o operador de interpolação caso deseje. Isto é extremamente útil se você estiver utilizando como template um texto com grande ocorrência dos operadores padrão. Para isso basta declarar `tokenEnclosers` em seu script. Você pode inclusive alterar mais de uma vez os símbolos.


```html
# numa seção style dentro de um template html

&lt;style>
@tokenEnclosers:&lt;>
.my-class {
    font-size: &lt;&lt; font-size-from-gen >>;
    color: &lt;&lt; font-color-from-gen >>;
}
@tokenEnclosers:{}
&lt;/style>
&lt;p>{{ my-gen-paragraph }}&lt;/p>
```
### NOTA:
- Como pode se notar, você define na mesma declaração os símbolos de abertura e fechamento.
- Você os define uma vez na declaração, mas utiliza dois símbolos no template.
- Você precisa escapar com "\" qualquer ocorrência de um dos operadores de interpolação no seu texto, mesmo que ele ocorra uma vez só.
- Os símbolos de abertura e fechamento precisam ser diferentes.

**Funções** são chamadas pelo nome seguido de parênteses contendo argumentos ou não.

**Valores gen** são obtidos de dicionários **gen**, uma estrutura de dados da linguagem utilizada para armazenar dados de texto. São acessados diretamente pelo nome da chave ou pela referência ao **gen** que fazem parte através da sintaxe mostrada da segunda linha com o uso do caractere `&` seguido do _alias_ do dicionário utilizado.

**Variáveis** são unidades de armazenamento de valores declarados no programa. São acessadas sempre através de `@nomeDaVariavel`.

 ### Notas:
- O estilo de capitalização adotado é o *camelCase*.
- A sintaxe de UltraGen é *case-sensitive*.
- Preferimos espaço como separador e 2 ou 4 espaços como tamanho de indentação, salvo quando processando textos de outras linguagens. Daí sugerimos seguir o guideline da linguagem destino.
- O delimitador de strings é caractere **aspas simples**. Você pode utilizar aspas duplas dentro da string sem problemas, mas deve escapar as aspas simples com **\**.
- Como há documentação detalhada sobre cada função e procedimento em outra seção, esta seção não se preocupará em explicar o que faz nenhuma das funções ou procedimentos apresentados aqui.

## Overrides

**Overrides** são declarações de procedimentos ou variáveis no meio de uma linha de texto plano

É recomendado para quando se deseja fazer pequenas intervenções no processamento do template e não se julga necessário a criação de um bloco de script.

```ruby
@renderBlank
@myVar='algum valor'
Minha linha de texto plano
Uma declaração de procedimento embutida
{{ @myVar }}

A linha acima será renderizada
Por causa da declaração renderBlank
A variável será impressa
```

## Bloco de script

Para codificação de um trecho de código estritamente composto de procedimentos é adequado utilizar o modo de *bloco de script*. Para isto basta declarar numa linha `@{` iniciando o bloco de script e `}` na última linha finalizando o bloco. Nestas linhas você não pode utilizar texto plano, apenas declarações de variáveis e procedimentos. Além disso, não é necessário o uso do operador de *override* **@**.

```ruby
@{
    print:'Este é um trecho de código'
    print:'Não preciso do @.'
    print: 'pois estou em bloco de script'
}
Fim do bloco. Aqui é texto plano e será renderizado.
```

## Procedimentos

Procedimentos não retornam valor e são chamados através do seu nome, seguido de **:** se houver argumentos ou apenas pelo nome, caso não haja.

```ruby
@{
    print:'chamando print com este texto como argumento'
    livePrint:'este comando imprime o texto na saída renderizada'
    explode:'item1|item2',exp,'|'
    renderBlank
    overwrite
}
```

## Funções

Funções são operações de processamento de texto e **sempre** devem retornar algum valor. Por esse entendimento, funções UltraGen nunca têm uma palavra do tipo **get** ou **print** em seu nome. É subentendido que qualquer função sempre retornará alguma string a ser impressa. E não há dúvidas disso pois a sintaxe para funções e procedimentos (que são como funções com retorno void) é diferente.

Para chamar funções basta digitar seu nome e argumentos se houver. Uma função só deve ser chamada num contexto que requeira retorno.

```ruby
Imprimindo a data atual: {{ date() }}
Perceba o operador de interpolação.
print:upper('some string')
Aqui print imprimirá o retorno da função upper
```

### NOTA:
- Algumas funções e procedimentos possuem *aliases* de nomes para diminuição de escrita (*syntax sugar*). Estes aliases geralmente são símbolos. Isto tem por objetivo poluir menos templates que sejam utilizados para geração de texto dinâmico. De toda forma, recomendamos que este recurso seja utilizado apenas em momentos onde se deseja realmente escrever pouco.

Exemplos:

```hljs
    ~ => concat
    ? => ternary
    _ => return
    ^ => arrow

``` 

## Comentários

Você pode fazer comentários de uma linha iniciando esta linha por **#**. Espaços no início da linha são ignorados.

```ruby
# esta linha será desprezada
Esta será impressa
```

Você também pode escrever blocos de comentários delimitando o trecho entre um `###` e outro `###` no fim.

```ruby
Linha que será impressa
###
Este trecho
Inteiro
Não será levado em conta
###
Esta linha será levada em conta
```

Isto funciona da mesma forma em texto plano ou em blocos de script. Não é necessário utilizar **@** mesmo quando escrevendo comentários em texto plano.

## "Todos juntos agora"

Segue um snippet de código ilustrando todos os conceitos vistos aqui. Não se preocupe em não saber o que cada declaração executa.

```ruby
@{
    exportAtRoot
    overwrite
    myVar = 'hello'
    list='value 1,value 2,value 3'
}
@copyTo:'path/to/an_address'
@copyTo:'path/to/another_address'
Aqui temos texto plano
@for:@list,val
    {{ @val }}
@endFor

###
   Agora imprimindo
   A variável que declarei
###

{{ @myVar }}
@print:'end processing'
#fim do script
```



