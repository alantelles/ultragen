# Arquivos GEN

Uma das características da UltraGen é uma estrutura de dados que é o formato básico de fontes de dados entre templates. Esta estrutura se chama **gen**. Este nome é legado da aplicação que originou a UltraGen chamada MetaGen (Metadata Generator). Os arquivos com os dados eram chamados de "Generic Metadata" e tinham a extensão *.gen*. 

## Breve histórico

Esta aplicação surgiu para resolver um problema de processamento de textos em lote no setor de pós-produção dos Estúdios Globo. Uma aplicação de `logging` de material também criada por mim, chamada EasyLog, gerava texto XML pronto, mas esse texto frequentemente tinha que ser alterado. Para isto, era necessário alterar todas as unidades que estavam executando a aplicação. O problema é que são cerca de 20 unidades móveis que não necessariamente gravam dentro do complexo. Uma atualização em massa poderia facilmente levar semanas causando inconsistência na esteira de pós-produção. A solução pensada por mim foi criar um tipo de arquivo o mais básico possível e utilizar os dados deste arquivo como fonte para um template e construir o XML no próprio Centro de Pós-Produção (o CPP). Neste complexo, temos acesso remoto a todos os computadores, além de termos menos computadores executando a aplicação. Assim, qualquer variação no XML a ser gerado, só necessitaria de uma mudança de template. A aplicação que fazia esse processamento era o MetaGen. É por essa origem que os arquivos-fonte de UltraGen são chamados de **templates**.

O MetaGen era uma template engine que foi ganhando funções de acordo com a demanda. O UltraGen iniciou como uma evolução do MetaGen, pois por razões de design o MetaGen se tornou limitado para nossas utilizações. Aos poucos, de acordo com a dinâmica de problemas a serem solucionados foi sendo redirecionado para se tornar uma linguagem de script indo além de uma template engine. Neste contexto, os arquivos gen passaram a ser usados como estruturas de dados dentro da linguagem e não apenas arquivos.

Hoje, UltraGen conta com um sistema embutido de persistência, é uma linguagem de script para páginas web e dispõe de um servidor embutido que propõe um framework básico para aplicações web.

## Voltando aos arquivos GEN

Arquivos gen são arquivos de texto plano com dados estruturados em pares chave-valor separados por um caractere determinado pelo programador. O caractere padrão é o **=**.

Você pode pensar no gen como um típico *record* ou um objeto, porém, sem métodos. Porém veremos a frente que é possível *assinar* métodos em arquivos gen.

Daqui pra frente nos referiremos a dados oriundos de dicionários gen apenas como **valores gen**.

Para a obtenção de um valor gen apenas precisamos referenciar a sua chave. Existe ainda a função **genValue**, mas sua utilização está explicada na sua página de documentação.

São linhas válidas num arquivo gen

`umaChave=Um valor qualquer`

`OUTRA CHAVE=OUTRO VALOR`

Como se pode ver, não há restrição de caracteres como chaves ou valores gen. Outra coisa a se notar é que qualquer espaço em branco é levado em conta. Nem valores nem chaves devem ser delimitados por aspas. Apesar dessa falta de restrição, recomendamos por boa prática de código que as chaves não tenham espaços.

Os valores podem ter mais de uma linha, porém deve-se ter o cuidado de usar caracteres de escape caso o caractere separador apareça em alguma das linhas que compõem um valor

```ruby
chave=texto de uma linha
chave2=este texto
é composto de diversas
linhas.
se eu precisar de um sinal de \=
eu devo escapá-lo
chave3=aqui é outro valor
```

Arquivos gen podem ser passados a uma sessão de execução como parâmetro.

`ultragen myCode.ultra.xml -gens "src1.gen|src2.gen" --persist`

Isto processará o template myCode.ultra.xml utilizando primeiro o arquivo src1.gen e depois o src2.gen como fontes de dados. O arquivo de saída será salvo com a extensão XML. UltraGen por padrão utiliza a extensão dada no template para os arquivos de saída.

Os arquivos de saída, por padrão, são salvos numa pasta com o nome do template e com o nome de cada .gen utilizado.

**myCode/src1.xml**

**myCode/src2.xml**

Este comportamento pode ser alterado através de `exportAtRoot` que faz com que a pasta com o nome do template não seja criada. E o nome do arquivo pode ser alterado através do procedimento `outFileName`. A extensão pode ser alterada através de `extension`.

```ruby
@exportAtRoot
@outFileName:date('hhnnsszzz',NOW)
@extension:'.xhtml'
{{ someValue }}
```
Neste caso os arquivos de saída seriam

**220023210.xhtml**

**220023290.xhtml**

Outra forma é a seguinte:

`ultragen myCode.ultra -gens src1.gen+src2.gen`

Neste modo, UltraGen pode utilizar dados dos dois arquivos gen passados ao mesmo tempo dentro do template. Não é necessário especificar no template de qual gen se deseja utilizar um valor a menos que os arquivos gen tenham chaves com o mesmo nome. Neste caso, se não houver especificação, a chamada retornará a primeira correspondência encontrada.

Sejam:

**src1.gen**
```
chave 1=valor 1
chave N=valor X
```

**src2.gen**
```
chave 2=valor 2
chave N=valor Y
```

**myCode.ultra**
```ruby
Chave 1: {{ chave 1 }}
Chave 2: {{ chave 2 }}
Chave N de src1: {{ &src1.chave N }}
Chave N de src2: {{ &src2.chave N }}
```

Cada arquivo gen é acessível por um *alias*. Por padrão, é utilizado o nome do arquivo sem a extensão.

Diversas funções e procedimentos utilizam os *aliases* dos gens para referenciá-los. O conjunto de gens é uma lista interna chamada de *Gen set*. Sendo uma lista, estes dicionários também podem ser acessados pelo seu índice. Isto é muito útil para quando estamos fazendo processamento em lote e não sabemos os nomes de todos os arquivos gen que serão usados como fontes.

### NOTA:

- Se seu script criar gens em tempo de execução pode se tornar inconsistente acessar seus dicionários gens através do index. Sabendo que o alias padrão de todo arquivo gen utilizado é o próprio nome dele sem a extensão, você pode utilizar `genName()` como `alias` caso deseje fazer alguma operação.
- Gens têm escopo global, por conceito. Isto quer dizer que você pode usar valores carregados em gens em qualquer parte do script com a mesma sintaxe de acesso. Porém, há um detalhe: Para includes, o escopo é global por referência. Para funções e procedimentos o escopo é global por valor. Isto quer dizer que em templates incluídos, alterações em valores gen são refletidas para toda a aplicação, mas em funções as alterações ficam restritas àquela instância da função/procedimento.

Gens podem ser alterados em tempo de execução através da declaração *setValue* ou da *associação implícita*. Os arquivos também podem ser salvos em tempo de execução através de *saveGen*. Observe a utilização destas declarações em conjunto com o acesso ao gen sem que se saiba seu nome, e consequentemente não se saiba seu *alias*. Aproveitaremos este momento para mostrar a chamada do processo com uma pasta com arquivos gen como fonte. Neste formato, a opção `--persist` é habilitada por padrão.

`ultragen myTemp.ultra.xml -genpath variousGens`

Supondo que todos os arquivos na pasta têm as seguintes chaves:

```
key1=algum valor
key2=algum outro valor
```

Em **myTemp.ultra.xml**

```ruby
setValue:0,'key3','adicionando um valor'
saveGen:0
```

Isto consegue adicionar um valor ao primeiro gen carregado por *myTemp* numa execução e salvar o gen com o novo valor adicionado. Note que não é necessário saber o nome e nem um *alias* do arquivo carregado.

## Funções especiais sobre gens

Arquivos gen, por conceito, não devem incluir lógica implícita. Isto quer dizer que nenhuma declaração gen ativa funções em um script ou que exista uma sintaxe especial prevista em um gen para que alguma coisa aconteça. Mas, como em toda regra, há uma exceção. Na verdade, duas.

Valores gen são por padrão referenciados por suas chaves, como vimos, mas há duas funções que contam com declarações especiais para obtenção de um valor de forma diferenciada.

São elas a **match** e a **routeMatch**. Match retorna um valor baseado num padrão correspondente que utilizaum caractere coringa em sua declaração.

De toda forma, você está completamente livre para criar implementações em suas aplicações que utilizem valores em arquivos gen com lógica implícita. Apenas na implementação da linguagem há a opção por não se fazer isto.

Elas serão detalhadas na sua página de documentação.

## Criando gens em tempo de execução

Você pode criar gens em tempo de execução carregando um arquivo .gen que exista ou apenas criando uma estrutura de dados lógica na sua aplicação. O procedimento `createGen` cria um gen com o alias dado e carrega seus valores se for passado um caminho válido.

Você também pode criar gens implicitamente através da sua notação de acesso por alias (*associação implícita*). Mostraremos no exemplo abaixo as duas formas.

Sejam:

**src1.gen**
```
chave1=valor 1
```

**myCode.ultra**
```ruby
@{
createGen:'src','src1.gen'
# carrega o arquivo src1.gen sob o alias 'src'

print:chave1
# imprime "valor 1"

&src.chave1 = 'outro valor'
print:chave1
# imprime "outro valor"

&src.chave2 = 'valor 2'
# cria o par chave2 => valor 2 no gen em src, mas não o salva

print:chave2,' ou ',&src.chave2
# imprime "valor 2 ou valor 2"

&src2.chaveNova = 'um novo valor'
# cria um gen sob o alias 'src2'

print:chaveNova
# imprime "um novo valor"

saveGen:'src'
# salva o gen com as alterações ocorridas em tempo de execução com o mesmo nome anterior
# sobreescreve o arquivo

saveGen:'src2','path/to/src2.gen'
# salva o gen src2 criado no caminho dado
}
```

#### NOTA:
- Se você criar outro gen, seja apenas a estrutura ou carregando utilizando um alias que já está sendo utilizado, isto não funcionará. Você precisa descarregar o gen previamente carregado sob aquele alias através de `unloadGen`.

**src1.gen**
```ini
chave1=valor do gen 1
```

**src2.gen**
```ini
chave1=valor do gen 2
```

**myCode.ultra**
```ruby
@{
createGen:'src','src1.gen'
# carrega o arquivo src1.gen sob o alias 'src'

print:chave1
# imprime "valor do gen 1"

createGen:'src','src2.gen'
# poderia carregar o arquivo src2.gen sob o alias 'src', porém...

print:chave1
# imprime "valor do gen 1"
# um gen não é substituído por razões de consistência
# é difícil garantir que os gens conterão as mesmas chaves e talvez não seja a intenção
# do desenvolvedor destruir o anterior por completo
# por isso, o modo correto é:

unloadGen:'src'
createGen:'src','src2,gen'
print:chave1
# imprime "valor do gen 2"
}
```

#### NOTA:

- Embora você não precise declarar o alias do gen de onde deseja utilizar o valor, é realmente recomendado que você faça isso e só não faça quando só houver um gen envolvido naquele template ou quando o contexto deixe bem explícito de onde está vindo o valor. Numa aplicação com vários arquivos conectados pode ser MUITO fácil se perder ao se perguntar de onde está vindo um determinado valor.


## Conclusão

Gen é uma estrutura de dados similar a um *record*. É a estrutura básica para organização de dados e transferência entre templates. UltraGen tem funções embutidas para persistência destes arquivos que são codificados em pares chaves-valor.
