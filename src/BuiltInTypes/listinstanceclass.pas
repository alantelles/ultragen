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
      function AsString:string; override;
      function GetItem(AIndex: TIntegerInstance):TInstanceOf;
      function GetItem(AIndex: integer):TInstanceOf;
      procedure SetItem(AIndex: integer; AItem: TInstanceOf);
      function PopItem:TInstanceOf;
      procedure Add(AItem: TInstanceOf);

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

function TListInstance.PopItem:TInstanceOf;
var
  Ret: TInstanceOf;
  Last: integer;
begin
  Last := lenList - 1;
  Ret := FValue[last];
  SetLength(FValue, last);
  Result := Ret;
end;

procedure TListInstance.SetItem(Aindex:integer; AItem: TInstanceOf);
begin
  if Aindex > LenList - 1 then
    raise EListError.Create('Undefined index '+IntToStr(Aindex)+' at list');
  FValue[AIndex] := Aitem;
end;

procedure TListInstance.Add(AItem: TInstanceOf);
var
  len: integer;
begin
  len := Length(FValue);
  len := len + 1;
  SetLength(FValue, len);
  FValue[len - 1] := AItem;
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
    if AItem.ClassNameIs('TStringInstance') then
      Ret.Add('''' + AItem.AsString + '''')
    else
      Ret.Add(AItem.AsString);
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

