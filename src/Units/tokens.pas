unit Tokens;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, TokenClass;

const
  TYPE_INTEGER = 'INTEGER';
  TYPE_FLOAT = 'FLOAT';      
  TYPE_STRING = 'STRING';
  TYPE_BOOLEAN = 'BOOLEAN';
  TYPE_NULL = 'NULL';
  TYPE_LIST = 'LIST';



  T_LANG_TRUE = 'True';
  T_LANG_FALSE = 'False';
  T_LANG_NULL = 'Null';
  T_PLUS = 'T_PLUS'; T_MINUS = 'T_MINUS'; T_MULT = 'T_MULT'; T_DIV = 'T_DIV';
  T_INT_DIV = 'T_INT_DIV'; T_MODULUS = 'T_MODULUS';
  T_REF_INC = 'T_REFERENTIAL_INC'; T_REF_DEC = 'T_REFERENTIAL_DEC';
  T_GT = 'T_GREATER_THAN'; T_LT = 'T_LESS_THAN'; T_EQ = 'T_EQUAL'; T_NOT = 'T_NOT';
  T_GEQ = 'T_GREATER_OR_EQUAL'; T_LEQ = 'T_LESS_OR_EQUAL'; T_NEQ = 'T_NOT_EQUAL';
  T_AND = 'T_AND'; T_OR  = 'T_OR';

  T_IF_START = 'T_IF_START';
  T_IF_END = 'T_IF_END';
  T_ELSE = 'T_ELSE';
  T_ELSE_IF = 'T_ELSE_IF';
  T_CONTINUE = 'T_CONTINUE';
  T_BREAK = 'T_BREAK';
  T_RETURN = 'T_RETURN';
  T_WHILE_LOOP = 'While loop';
  T_FOR_LOOP = 'For Loop';

  T_LPAREN = 'T_LPAREN';
  T_RPAREN = 'T_RPAREN';
  T_ASSIGN = 'T_ASSIGN';
  T_ESCAPE = 'T_ESCAPE';
  T_DB_QT = 'T_DOUBLE_QUOTES';
  T_SG_QT = 'T_SINGLE_QUOTES';
  T_STRENC_SINGLE = '''';
  T_STRENC_DOUBLE = '"';
  T_STRENC_MULTI = '"""';
  T_LONG_STR = 'T_LONG_STRING';
  T_FUNC_PARAM = 'T_FUNC_PARAM';
  T_FUNC_DEF = 'T_FUNCTION_DEFINITION';
  T_COMMA = 'T_COMMA';
  T_END = 'block end: ';
  T_ID = 'T_ID';
  T_ATTR_ACCESSOR = 'ATTRIBUTE ACCESS';

  ESCAPE_SYMBOL = '\';
  ASSIGN_SYMBOL = '=';
  ATTR_ACCESSOR = '.';
  EOF = 'EOF';
  NONE = '';
  SET_NUMBERS = '0123456789';
  LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz';
  T_NEWLINE = 'T_NEWLINE';
  T_COMMENT = 'T_COMMENT';

  T_LINE_COMMENT = '#';
  T_LIST_START = 'T_LIST_START';
  T_LIST_END = 'T_LIST_END';


var
  ReservedWords: TFPObjectHashTable;

implementation
begin
  ReservedWords := TFPObjectHashTable.Create();
  ReservedWords.Add('function', TToken.Create(T_FUNC_DEF, 'block:function definition'));
  ReservedWords.Add('if', TToken.Create(T_IF_START, 'block:IF START'));
  ReservedWords.Add('elsif', TToken.Create(T_ELSE_IF, 'ELSE IF'));
  ReservedWords.Add('else', TToken.Create(T_ELSE, 'T_ELSE'));
  ReservedWords.Add('while', TToken.Create(T_WHILE_LOOP, 'block:'+T_WHILE_LOOP));
  ReservedWords.Add('for', TToken.Create(T_FOR_LOOP, 'block:'+T_FOR_LOOP));
  ReservedWords.Add('True', TToken.Create(TYPE_BOOLEAN, T_LANG_TRUE));
  ReservedWords.Add('False', TToken.Create(TYPE_BOOLEAN, T_LANG_FALSE));
  ReservedWords.Add('Null', TToken.Create(TYPE_NULL, T_LANG_NULL));
  ReservedWords.Add('continue', TToken.Create(T_CONTINUE, T_CONTINUE));
  ReservedWords.Add('break', TToken.Create(T_BREAK, T_BREAK));
  ReservedWords.Add('return', TToken.Create(T_RETURN, T_RETURN));


end.

