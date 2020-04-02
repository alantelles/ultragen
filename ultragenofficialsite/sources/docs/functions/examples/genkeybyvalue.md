Seja um gen com alias 'ex' o primeiro template do *genset*

```ini
aKey=Esta chave tem um valor
otherKey=Esta tem outro valor
```

E o template


```ruby
@var='Esta chave tem um valor'
{{ genKeyByValue('ex', @var) }}
{{ genKeyByValue(0, @var) }}
{{ genKeyByValue('ex', 'Esta tem outro valor') }}

```

Teremos

```hljs
aKey
aKey
otherKey
```
