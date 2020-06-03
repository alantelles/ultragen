unit ExceptionsClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ELexicalError = class (Exception) end;
  EParserError = class (Exception) end;
  ESemanticError = class (Exception) end;

implementation

end.

