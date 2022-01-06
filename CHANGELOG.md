# Changelog

Changes will be described from version 0.8.0

## 0.9.0
Release date: NONE

## Changes

- New optional JS-like style for setting and accessing dictionary values. All other methods still working
```ruby
x = {'myKey': 500}
# access
print(x.myKey) # prints 500, only valid names allowed
print(x['myKey']) # still works, prints 500
print(x[:myKey]) # also still works, prints 500
# set
x.myKey = "I changed value"
print(x.myKey) # prints I changed value
```

## 0.8.0
Release date: *2021/10/14*

## Changes

- New function to check values types `checkTypes`
- Type hint/check for functions parameters
- `include` now returns a the value of the same name of file/module if some exists in included file/module. It can be assigned to an identifier
    - Example: `MyName = include @Core.Request` will assign the class `Request` to `MyName`


### Main current works

- Basic database client for SQLite3 and PostgreSQL. There's an implementation done but it's buggy for prepared statements.
- The `RequestBuilder` class for configure HTTP request with a builder pattern.
