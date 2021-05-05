# UltraGen
## The website machine
### Desktop/Web Template Engine/Scripting Language

UltraGen is a script langauage and template engine built to turn easier to create a web application supporting plain text as well script execution.

## Features

- Easy to learn
- Simple syntax
- Dynamic and strong typing
- Object oriented
- Aparted builtin web response output stream (*live* output) in addition to *stdout*.
- Easily extensible

## Why UltraGen?

There are many technologies for template engine however all well known mainstream are provided as a package or framework of some language, each one with its own learning curve. Even the response body is treated in a separated method. UltraGen tries to solve this as making the web development **part** of the language. You don't need to learn the script and the language, since they are all UltraGen. It also ships with a web framework embedded that implement many of developers needs, like cookies, sessions or routes handling.
In addition to create web pages UltraGen can be used to process any type of text that has variable data inside a template, like XMLs or config files with some changes.

## Examples

Hello, World!
```ruby
print('Hello, World!')
# print at console(stdout)
```

Hello, World! (adding to http response)
```ruby
live 'Hello, World!'
```

Variables and types
```ruby
a = 'texto' # string
a = 10 # int
a = 10.5 # float
a = true # boolean
a = null # null
a = """uma string
com várias linhas"""
```

Calling methods
```ruby
a = 'Hello, World!'
print(a.upper())
# HELLO, WORLD!
```

Loops
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

Lists and dictionaries
```ruby
# listas
a_list = ['item1', 'item2', 'item3']

# dicionarios
a_dict = {
    'key1': 'valor1',
    'key2': 'valor2',
    a_list: 'value for any element in a_list',
    null: "default value for a key that doesn't exists"
}
```

Embedded in plain text
```html
@header = 'My Website'
@body = 'THIS IS MY WEBSITE'
<h1>{{ header }}</h1>
<p>{{ body.lower() }}</p>
```

Will print
```html
<h1>My Website</h1>
<p>this is my website</p>
```

Passing a closure as value
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
At this time UltraGen is in pre-release phase and it's only available through Docker image with `docker pull alantelles/ultragen`. Pre-releases are using *CalVer*.