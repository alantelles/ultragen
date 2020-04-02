Seja um gen com alias 'ex' o primeiro template do *genset*

```ini
aKey=Esta chave tem um valor
otherKey=Esta tem outro valor
```

E o template


```ruby
{{ genKeyByIndex('ex', 0) }}
{{ genKeyByIndex(0, 0) }}
{{ genKeyByIndex('ex', 1) }}

```

Teremos

```hljs
aKey
aKey
otherKey
```
