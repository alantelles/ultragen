Seja um gen com alias 'ex' o primeiro template do *genset*

```ini
aKey=Esta chave tem um valor
otherKey=Esta tem outro valor
```

E o template


```ruby
{{ genValueByIndex('ex', 0) }}
{{ genValueByIndex(0, 0) }}
{{ genValueByIndex('ex', 1) }}

```

Teremos

```hljs
aKey
aKey
otherKey
```
