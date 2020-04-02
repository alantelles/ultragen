```ruby
@{
fn='path/to/my/file.txt'
print:fileName(@fn)
print:fileName(@fn,'true')
print:fileName(@fn,'false')
print:fileName(@fn,'any')
print:fileName(@fn,bool(1))
}
```

```hljs
file
file.txt
file
file
file.txt
```
