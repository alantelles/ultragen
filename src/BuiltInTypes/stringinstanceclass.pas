unit StringInstanceClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, InstanceofClass, ListInstanceClass, ExceptionsClasses;

type
  TStringInstance = class (TInstanceOf)
    protected
      FValue:string;
      FMetName:string;
      FArgs: TInstanceList;
    public
      property PMetname:string read FMetname write FMetName;
      property PArgs:TInstanceList read FArgs write FArgs;
      property PValue:string read FValue write FValue;
      constructor Create(AValue: string);
      function GetChar(AnIndex: TIntegerInstance):TStringInstance;
      function AsString: string;


      function Execute:TInstanceOf;

      //functions
      function SplitString:TListInstance;

      //procedures

  end;

implementation
uses
  StrUtils;

constructor TStringInstance.Create(AValue: string);
begin
  FValue := AValue;
end;

function TStringInstance.AsString:string;
begin
  Result := '"'+ FValue + '"';
end;

function TStringInstance.GetChar(AnIndex: TIntegerInstance):TStringInstance;
begin
  if Length(FValue) < AnIndex.PValue + 1 then
    raise ERunTimeError.Create('Index is greater than string size');
  Result := TStringInstance.Create(FValue[AnIndex.PValue + 1]);
end;

function TStringInstance.SplitString:TListInstance;
var
  ASep, s: string;
  AList: TStringList;
  AInsList: TInstanceList;
  len: integer;
begin
  ASep := TStringInstance(FArgs[0]).PValue;
  AList := TStringList.Create;
  AList.SkipLastLineBreak := True;
  AList.LineBreak := ASep;
  AList.Text := FValue;
  SetLength(AInsList, 0);
  len := 0;
  for s in AList do
  begin
    len := len + 1;
    SetLength(AInsList, len);
    AInsList[len - 1] := TStringInstance.Create(s);
  end;
  Result := TListInstance.Create(AInsList);
end;

function TStringInstance.Execute: TInstanceOf;
begin
  if FMetName = 'split' then
    Result := SplitString
  else if FMetName = 'upper' then
  begin
    FValue := AnsiUpperCase(FValue);
    Result := Self;
	end
  else if FMetName = 'replace' then
  begin
    FValue := ReplaceStr(FValue, TStringInstance(FArgs[0]).PValue, TStringInstance(FArgs[1]).PValue);
    Result := Self;
	end;
end;


end.

