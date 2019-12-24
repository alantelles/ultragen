# UltraGen - facilitando a web

`print:'Hello, UltraGen'`

UltraGen é uma linguagem interpretada de alto nível desenhada para ser **simples e objetiva**. Inicialmente concebida como uma template engine tomou novos rumos e hoje é uma linguagem para tarefas genéricas e para construção da camada mid-level de sistemas web.

## Camada mid-level?

Sim. Hoje vemos sistemas web compostos de duas partes geralmente:
- Um backend pesado, usando todo poder de uma determinada tecnologia para acesso aos dados.
- Um frontend feito com alguma tecnologia poderosa de construção de views, como o Angular ou React.
- Alguma forma de conectar os dois.

Ou...

- Algum framework cheio de recursos que geralmente tem muito mais do que você precisa. Você não precisa daquilo tudo, mas pra seu sistema acontecer, tudo aquilo tem que vir no pacote.

E todas essas com algum tipo de deploy relativamente complexo.

## UltraGen quer te ajudar nisso!

UltraGen tem 3 diretrizes principais de construção:

1. Simplicidade na escrita
2. Simplicidade no deploy
3. Fácil integração com outras tecnologias

## Features

- Sintaxe simples
- Sistema de persistência de dados embutido (arquivos _.gen_)
- Fácil criação de texto com dados variáveis
- Extremamente simples deploy
- Pré-framework web (incluindo servidor) embutido

# Uso:

`ultragen yourScript.ultra`

## Passando um arquivo gen como fonte

`ultragen yourScript.ultra -gens seuArquivo.gen+seuOutroArquivo.gen`

- Este modo usa os dois arquivos .gen definidos como fontes dentro do script

## Salvando o resultado da compilação do template

`ultragen yourTemplate.ultra -gens "a.gen|b.gen|c.gen" --persist`

- Este modo processa os três arquivos citados sobre o template definido.

## Processando uma pasta de arquivos gen em lote

`ultragen yourTemplate.ultra -genpath "path/to/my/gens"`

- Este modo automaticamente usa o parâmetro `--persist` para salvar o resultado do processamento.

# Alguns exemplos

## Hello, world

`@print:'Hello, World!'`

## Imprimindo datas

```
@print:date()
@print:date('yyyy-mm-dd',NOW)
@print:date('hh:nn:ss.zzz')
```

Imprime:

```
2019-12-24 05:35:22.210
2019-12-24
05:35:22.213
```

## Operações matemáticas

Mostrando o modo de bloco de script

```
@{
  clear
  a = 10
  b = 64
  #uso de @ para acesso às variáveis
  print:'soma: ',sum(@a,@b)
  print:'subtração: ',sub(@a,@b)
  print:'multiplicação: ',mult(@a,@b)
  print:'divisão inteira: ',intDiv(@b,@a)
  print:'divisão: ',div(@b,@a)
  print:'potenciação ao quadrado: ',pow(@a)
  print:'potenciação: ',pow(@a,3)
  print:'raiz quadrada: ',root(@a)
  print:'raiz: ',root(@b,3)
}
```

Imprime:

```
soma: 74
subtração: -54
multiplicação: 640
divisão inteira: 6
divisão: 6.4
potenciação ao quadrado: 100
potenciação: 1000
raiz quadrada: 3.16227766016838
raiz: 1
```

Laços `for` e `loop`

```
@{
  list='ghi,abc,mno,jkl,def'
  print:'Ordem natural separado por vírgula'
  for:@list,'l'
    print:'Elemento ',@l.i,': ',@l
  endFor
  list='ghi|abc|mno|jkl|def'
  print:''
  print:'Iterando pela sequência ordenada separado por "|"'
  for:@list,'l','|','ASC'
    print:'Elemento ',@l.i,': ',@l
  endFor
  print:''
  print:'Executando um loop'
  loop:5,1000,'c'
    print:'No ciclo ',@c,': ',date('hh:nn:ss')
  endLoop
}
```

Imprime:

```
Ordem natural separado por vírgula
Elemento 0: ghi
Elemento 1: abc
Elemento 2: mno
Elemento 3: jkl
Elemento 4: def

Iterando pela sequência ordenada separado por "|"
Elemento 0: abc
Elemento 1: def
Elemento 2: ghi
Elemento 3: jkl
Elemento 4: mno

Executando um loop
No ciclo 0: 06:11:55
No ciclo 1: 06:11:56
No ciclo 2: 06:11:57
No ciclo 3: 06:11:58
No ciclo 4: 06:11:59
```

Embutida em texto estruturado:

```
@aVar = 'Hello'
@otherVar = 'World'
@styleBold='style="font-weight: bold"'
<h1>{{ join(',',@aVar,@otherVar) }}</h1>
{{ inTag('p','Are you fine today?',@styleBold) }}
```

Imprime:

```
<h1>Hello,World</h1>
<p style="font-weight: bold">Are you fine today?</p>
```
