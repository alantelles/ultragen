unit ExceptionsClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TokenClass, StrUtils;

const
  E_INVALID_ARGS = 'Invalid number of arguments for this function';
  E_INVALID_ARGS_TYPE = 'Invalid type of argument for this function';

type
  EUltraGenException = class
    public
      constructor Create(AMsg:string; ATrace: TStringList; AToken: TToken; ClientType:string = '');
      constructor Create(AMsg, AFileName: string; LineNo, CharNo:integer);
      constructor Create(AMsg:string);
  end;

  ELexicalError = class (EUltraGenException) end;
  EParserError = class (EUltraGenException) end;
  ESemanticError = class (EUltraGenException) end;
  ERunTimeError = class (EUltraGenException) end;
  EArgumentsError = class(EUltraGenException) end;
  EValueError = class (EUltraGenException) end;
  ETypeError = class (EUltraGenException) end;
  EListError = class(EUltraGenException) end;
  EClientException = class(EUltraGenException) end;


implementation

constructor EUltraGenException.Create(AMsg:string);
begin
  raise Exception.Create(AMsg);
end;

constructor EUltraGenException.Create(AMsg, AFileName: string; LineNo, CharNo:integer);
var
  Exib: string;
begin
  Exib := 'ULTRAGEN found an unhandled ERROR while running "'+AFileName+'"';
  if LineNo <> -1 then
    Exib := Exib +' at <'+
      IntToStr(LineNo)+': '+IntTostr(CharNo)+'>';
  Exib := Exib + sLineBreak + '' + AMsg;
  raise Exception.Create(Exib);
end;

constructor EUltraGenException.Create(AMsg:string; ATrace: TStringList; AToken: TToken; ClientType:string = '');
var
  Exib, astr, AtraceDump: string;
begin
  Exib := 'ULTRAGEN found an unhandled ERROR while running '+
    #13#10 + '' + '    ' + ATrace[0] + #13#10 + ''+ #13#10 + '';
  if ClassNameIs('EClientException') then
    Exib := Exib + 'E' +ClientType+ 'Error' + ': ' + AMsg
  else
    Exib := Exib + ClassName + ': ' + AMsg;
  ATrace.Delete(0);
  if ATrace.Count > 0 then
  begin
    Exib := Exib + #13#10 + '' + 'TRACE (most recent call first): ' + #13#10 + '';
    ATraceDump := '';
    for Astr in ATrace do
      ATraceDump :=  '  + '+ AStr + #13#10 + '' + ATraceDump ;
    Exib := Exib + ATraceDump;
  end;
  Exib := Exib + #13#10 + '' +
    'If you think it''s an interpreter error please open an issue on UltraGen GitHub containing this error information'+ #13#10 + '' +
    'https://github.com/alantelles/ultragen/issues/new'  + #13#10 + '';
  raise Exception.Create(Exib);
end;

end.

