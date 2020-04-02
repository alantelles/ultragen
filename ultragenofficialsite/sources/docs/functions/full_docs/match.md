Seja um gen:
```ini
Key*Index=Value 1
OtherKey*Index=Value 2
```

E um template:

```hljs
{{ Key4Index }}
{{ Key string in the middle Index }}
{{ KeyIndex }}
{{ OtherKey Index }}
{{ OtherKey74829Index }}
{{ Some no match }}
{{ No match, 'return a default' }}
```

Teríamos:

```hljs
Value 1
Value 1
Value 1
Value 2
Value 2

return a default
```

Caso não haja correspondência o valor padrão do gen ou o da chamada será retornado.
