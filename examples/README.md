# UltraGen
## The website machine
### Desktop/Web Template Engine/Scripting Language

UltraGen é uma linguagem de script e template engine feita com o propósito de facilitar a construção de páginas web suportando o uso de texto plano em conjunto com sintaxe de script.

## Features

- Fácil de aprender
- Sintaxe simples e com pouca variação
- Tipagem dinâmica e forte
- Orientada a objetos
- Stream de saída duplo builtin (stdout e "live")
- Facilmente extensível

## Por que UltraGen?

Existem tecnologias de template engine no mercado, mas todas as de mainstream são providas como pacotes e frameworks de alguma linguagem, cada qual com sua curva de aprendizado, a exceção do PHP. Porém, PHP tem as ressalvas de design (tipagem fraca e coreção, variação nos nomes de funções, funções relacionadas ao mesmo tipo implementadas como métodos ou funções), além do stream único de saída (toda saída definida é feita pelo "echo"). UltraGen busca resolver os dois problemas. Um design consistente pensado já observando os paradigmas atuais, a template engine está incluida na linguagem, então não é necessário aprender a linguagem e o framework de extensão, nem instalar outras ferramentas para este fim. Além disso, é possível imprimir na saída web ou no console com funções diferentes.

## Exemplos

Hello, World!
```ruby
print('Hello, World!')
# imprimindo no console
```

Hello, World! (imprimindo na página web)
```ruby
live 'Hello, World!'
```

Variáveis e tipos
```ruby
a = 'texto' # string
a = 10 # int
a = 10.5 # float
a = true # boolean
a = null # null
a = """uma string
com várias linhas"""
```

Chamando métodos
```ruby
a = 'Hello, World!'
print(a.upper())
# HELLO, WORLD!
```

Laços de repetição
```ruby
# for com listas
for (['item1', 'item2', 'item3'], i)
    print(i) # imprime o elemento
    print(_i) # imprime o índice
end

# for com strings
for ('uma string', i)
    print(i) # imprime o caractere
    print(_i) # imprime o índice
end

#for com inteiros
for (10, i)
    print(i) # imprime o número atual
    print(_i) # imprime o mesmo número
end

x = 0
while(x < 10)
    print(x)
    x += 1
end
```

Tipos compostos
```ruby
# listas
a_list = ['item1', 'item2', 'item3']

# dicionarios
a_dict = {
    'key1': 'valor1',
    'key2': 'valor2',
    a_list: 'valor para qualquer elemento em a_list',
    null: 'valor para uma chave que não exista'
}
```

Embutida em texto plano
```html
@header = 'My Website'
@body = 'THIS IS MY WEBSITE'
<h1>{{ header }}</h1>
<p>{{ body.lower() }}</p>
```

Imprimirá
```html
<h1>My Website</h1>
<p>this is my website</p>
```

Passando uma closure como valor
```ruby
myFunc = function()
    print('Hello')
end

function machine(fn)
    fn()
end

myFunc()
machine(function()
    print("it's a closure")
end)
# Hello
# it's a closure
```

Decorators
```ruby
decorator wrapper(fn)
    print("inside decorator")
    fn()
end

wrapped = wrapper(function()
    print('a decorated function')
end)

wrapped()
# inside decorator
# a decorated function

```


## Releases
Atualmente ainda não há releases da linguagem. Siga o projeto para ser notificado e experimente UltraGen!