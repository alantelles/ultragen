unit ListInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass;

type
  TListInstance = class (TInstanceOf)
    private
      FValue: TInstanceList;
      function LenList:integer;
    public
      property Count: integer read LenList;
      property PValue: TInstanceList read FValue write FValue;
      constructor Create(AList: TInstanceList);
      constructor Create;
      function AsString:string;
      function GetItem(AIndex: TIntegerInstance):TInstanceOf;

  end;

implementation
uses
  StrUtils, CoreUtils, Tokens, StringInstanceClass;

constructor TListInstance.Create(AList: TInstanceList);
begin
  FValue := AList;
end;

constructor TListInstance.Create;
begin

end;

function TListInstance.GetItem(AIndex: TIntegerInstance):TInstanceOf;
var
  Ret: TInstanceOf;
begin
  Ret := FValue[AIndex.PValue];
  if Ret <> nil then
    Result := Ret
  else
    raise EListError.Create('Undefined index '+IntToStr(Aindex.PValue)+' at list');
end;

function TListInstance.LenList:integer;
begin
  Result := Length(FValue);
end;

function TListInstance.AsString:string;
var
  AItem: TInstanceOf;
  Ret: TStringList;
begin
  Ret := TStringList.Create;
  Ret.SkipLastLineBreak := True;
  Ret.LineBreak := ', ';
  for AItem in FValue do
  begin
    case AItem.ClassName of
      'TStringInstance' : Ret.Add(TStringInstance(AItem).AsString);
      'TIntegerInstance' : Ret.Add(TIntegerInstance(AItem).PValue.ToString);
      'TFloatInstance' : Ret.Add(FloatToStr(TFloatInstance(AItem).PValue));
      'TBooleanInstance' : Ret.Add(BooleanToStr(TBooleanInstance(AItem).PValue));
      'TNullInstance' : Ret.Add(T_LANG_NULL);
      'TListInstance' : Ret.Add(TListInstance(AItem).AsString);
    end;
  end;
  Result := '[' + Ret.Text + ']';
end;

end.

