unit Tokens;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, TokenClass;

const
  TYPE_integer = 'integer';
  TYPE_FLOAT = 'FLOAT';      
  TYPE_STRING = 'STRING';
  TYPE_BOOLEAN = 'BOOLEAN';
  TYPE_NULL = 'NULL';
  TYPE_LIST = 'LIST';
  TYPE_BYTE = 'BYTE';



  T_LANG_TRUE = 'true';
  T_LANG_FALSE = 'false';
  T_LANG_NULL = 'null';
  T_PLUS = 'T_PLUS'; T_MINUS = 'T_MINUS'; T_MULT = 'T_MULT'; T_DIV = 'T_DIV';
  T_INT_DIV = 'T_INT_DIV'; T_MODULUS = 'T_MODULUS';
  T_REF_INC = 'T_REFERENTIAL_INC'; T_REF_DEC = 'T_REFERENTIAL_DEC';
  T_GT = 'T_GREATER_THAN'; T_LT = 'T_LESS_THAN'; T_EQ = 'T_EQUAL'; T_NOT = 'T_NOT';
  T_GEQ = 'T_GREATER_OR_EQUAL'; T_LEQ = 'T_LESS_OR_EQUAL'; T_NEQ = 'T_NOT_EQUAL';
  T_AND = 'T_AND'; T_OR  = 'T_OR';

  T_IF_START = 'block:T_IF_START';
  T_IF_END = 'T_IF_END';
  T_ELSE = 'T_ELSE';
  T_ELSE_IF = 'T_ELSE_IF';
  T_CONTINUE = 'T_CONTINUE';
  T_LAMBDA = 'T_LAMBDA';
  T_BREAK = 'T_BREAK';
  T_RETURN = 'T_RETURN';
  T_WHILE_LOOP = 'block:T_WHILE_LOOP';
  T_FOR_LOOP = 'block:T_FOR_LOOP';

  T_SHORT_INC = 'T_SHORT_INC';
  T_SHORT_DEC = 'T_SHORT_DEC';
  T_SHORT_MULT = 'T_SHORT_MULT';
  T_SHORT_DIV = 'T_SHORT_DIV';
  T_SHORT_INT_DIV = 'T_SHORT_INT_DIV';

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
  T_FUNC_DEF = 'block:T_FUNCTION_DEFINITION';
  T_DECOR_DEF = 'block:T_DECORATOR_DEFINITION';
  T_COMMA = 'T_COMMA';
  T_END = 'T_BLOCK_END';
  T_ID = 'T_ID';
  T_ATTR_ACCESSOR = 'ATTRIBUTE ACCESS';
  T_LIVE_OUTPUT = 'T_LIVE_OUTPUT';
  T_LIVE_PRINT = 'T_LIVE_PRINT';
  T_INTERPOLATION_START = 'T_INTERPOLATION_START';
  T_INTERPOLATION_END = 'T_INTERPOLATION_END';
  T_PLAIN_TEXT = 'T_PLAIN_TEXT';
  T_LINE_SCRIPT_EMBED = 'T_LINE_SCRIPT_EMBED';
  T_FROM_NAMESPACE = 'T_FROM_NAMESPACE';
  T_INCLUDE = 'T_INCLUDE';
  T_DICT_ASSIGN = 'T_DICT_ASSIGN';
  T_NEW_OBJECT = 'T_NEW_OBJECT';
  T_CLASS_DEF = 'T_CLASS_DEF';
  T_MODULE_PATH = 'T_MODULE_PATH';
  T_ASSIGNED_TEST = 'T_ASSIGNED_TEST';

  ESCAPE_SYMBOL = '\';
  ASSIGN_SYMBOL = '=';
  ATTR_ACCESSOR = '.';
  EOF = 'EOF';
  NONE = '';
  SET_NUMBERS = '0123456789';
  LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  T_NEWLINE = 'T_NEWLINE';
  T_COMMENT = 'T_COMMENT';

  T_LINE_COMMENT = '#';
  T_LIST_START = 'T_LIST_START';
  T_LIST_END = 'T_LIST_END';
  T_DICT_START = 'T_DICT_START';
  T_DICT_END = 'T_DICT_END';
  T_LOAD_TYPE = 'T_LOAD_TYPE';




var
  //ReservedWords: TFPHashObjectList;
  //InnerAttributes: TFPHashObjectList;
  KW, Inners: TStringList;

implementation
begin
  //ReservedWords := TFPHashObjectList.Create();
  KW := TStringList.Create;
  KW.Add('function=' +  T_FUNC_DEF);
  KW.Add('decorator=' + T_DECOR_DEF);
  KW.Add('assigned=' + T_ASSIGNED_TEST);
  KW.Add('if=' +  T_IF_START);
  KW.Add('elsif=' +  T_ELSE_IF);
  KW.Add('else=' +  T_ELSE);
  KW.Add('while=' +  T_WHILE_LOOP);
  KW.Add('for=' +  T_FOR_LOOP);
  // KW.Add(T_LANG_TRUE + '=' + TYPE_BOOLEAN);
  // KW.Add(T_LANG_FALSE + '=' + TYPE_BOOLEAN);
  KW.Add(T_LANG_NULL + '=' + TYPE_NULL);
  KW.Add('continue=' +  T_CONTINUE);
  KW.Add('load=' + T_LOAD_TYPE);
  KW.Add('break=' +  T_BREAK);
  KW.Add('live=' +  T_LIVE_OUTPUT);
  KW.Add('return=' +  T_RETURN);
  KW.Add('include=' +  T_INCLUDE);
  KW.Add('new=' +  T_NEW_OBJECT);
  KW.Add('class=' + T_CLASS_DEF);
  KW.Add('lambda=' + T_LAMBDA);
  KW.Add('end=' + T_END);

  Inners := TStringList.Create;
  Inners.Add('LIVE=' + T_LIVE_PRINT);


end.

