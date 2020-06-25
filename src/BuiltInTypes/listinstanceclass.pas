unit ListInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass;

type
  TListInstance = class (TInstanceOf)
    private
      FValue: TInstanceList;
      FMetName:string;
      FArgs: TListInstance;
      function LenList:integer;
    public
      property Count: integer read lenList;
      property PValue: TInstanceList read FValue write FValue;
      constructor Create(AList: TInstanceList);
      constructor Create;
      function AsString:string;
      function GetItem(AIndex: TIntegerInstance):TInstanceOf;
      function GetItem(AIndex: integer):TInstanceOf;

      function Execute: TInstanceOf;

      //functions
      function MapList: TListInstance;


  end;

implementation
uses
  StrUtils, CoreUtils, Tokens, ExceptionsClasses, StringInstanceCLass;

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

function TListInstance.GetItem(AIndex: integer):TInstanceOf;
var
  Ret: TInstanceOf;
begin
  Ret := FValue[AIndex];
  if Ret <> nil then
    Result := Ret
  else
    raise EListError.Create('Undefined index '+IntToStr(Aindex)+' at list');
end;

function TListInstance.LenList:integer;
var
  i: integer;
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

function TListInstance.Execute:TInstanceOf;
begin
  if FMetName = 'each' then
    Result := MapList;
end;

function TListInstance.MapList:TListInstance;
begin

end;

end.

