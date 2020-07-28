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
      function AsString: string;  override;
      //procedures

  end;

implementation
uses
  StrUtils, LazUTF8;

constructor TStringInstance.Create(AValue: string);
begin
  FValue := AValue;
  FStrValue := AValue;
  FCoreType := True;
end;

function TStringInstance.AsString:string;
begin
  Result := FValue;
end;

function TStringInstance.GetChar(AnIndex: TIntegerInstance):TStringInstance;
begin
  if UTF8Length(FValue) < AnIndex.PValue + 1 then
    raise ERunTimeError.Create('Index is greater than string size', '', 1, 1);
  Result := TStringInstance.Create(FValue[AnIndex.PValue + 1]);
end;




end.

