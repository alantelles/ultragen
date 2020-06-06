unit corefunctionsregisterrecords;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

const
  T_NULL = 'TNullInstance';
  T_BOOLEAN = 'TBooleanInstance';
  T_INTEGER = 'TIntegerInstance';
  T_FLOAT = 'TFloatInstance';
  T_STRING = 'TStringInstance';
type

  TSignature = record
    FuncName: string;
    Return: string;
    Params: array of string;
  end;

  TFunctionList = class(TList)

  end;

var
  CoreFunctionList: TFunctionList;
  SPrint, SInline, SStr, STypeof: TSignature;

implementation

begin
  CoreFunctionList := TFunctionList.Create;
  SPrint.FuncName := 'print';
  SPrint.Return := T_NULL;
  SPrint.Params := ['*|*'];
  SPrint.FuncName := 'inline';
  SPrint.Return := T_NULL;
  SPrint.Params := ['*,*'];
  SStr.FuncName := 'str';
  SStr.Return := T_STRING;
  SStr.Params := ['1,*' ];
  STypeof.FuncName := 'typeof';
  STypeof.Return := T_STRING;
  STypeof.Params := ['1,*'];

  CoreFunctionlist.Add(SPrint);


end.

