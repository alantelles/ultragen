```ruby
@{
   aVar = false
   print:ternary(@aVar,'esta é verdadeira')
   # imprime uma string vazia
   print:ternary(@aVar,'esta é verdadeira','esta é falsa')
   # imprime "esta é falsa"

   aVar = true
   print:?(@aVar,'esta é verdadeira')
   # imprime "esta é verdadeira"
   print:?(@aVar,'esta é verdadeira','esta é falsa')
   # imprime "esta é verdadeira"
}
```