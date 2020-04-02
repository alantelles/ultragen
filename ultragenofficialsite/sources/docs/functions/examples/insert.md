Seja o template *caller.ultra*:

```ruby
&main.aKey = 'my value'
&main.otherKey = 'o value'
aVar = 'some variable'
path = 'path/to/inserted.ultra'
live:insert(@path, 'main', @aVar, 'a generic param')
```

E o template *inserted.ultra*:

```ruby
{{ aKey }}
{{ &amp;param0.aKey }}
{{ &amp;param0.otherKey }}
{{ @param[0] }}
{{ @param[1] }}
{{ @param[2] }}
```

O resultado será:

```hljs
my value
my value
o value
main
some variable
a generic param
```
