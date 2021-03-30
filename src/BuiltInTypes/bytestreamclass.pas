unit ByteStreamClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, ARClass, StringInstanceclass, ListInstanceClass;

  type TByteStreamInput = array of byte;

  type TByteStreamInstance = class (TInstanceOf)
    protected
      FValue: TByteStreamInput;
    public
      property PValue: TByteStreamInput read FValue write FValue;
      constructor Create(ByteArray: TByteStreamInput);
      constructor Create(ByteArray: string);
      function SaveStream(OutPath: TStringInstance): TBooleanInstance;
      function ReadStream(count: integer): TByteStreamInstance;
      function ReadStream: TByteStreamInstance;
      procedure WriteStream(ByteArray: TByteStreamInput);
      function AsString: string;  override;
  end;




implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
   Dos, UltraGenInterfaceClass, ResponseHandlerClass, Math;

constructor TByteStreamInstance.Create(ByteArray: TByteStreamInput);
begin
  inherited Create;
  FValue := ByteArray;
  FMembers.Add('position', TIntegerInstance.Create(0));
  //FValue.LoadFromStream(TStringStream.Create(TStringInstance(AInstance).PValue));
end;
constructor TByteStreamInstance.Create(ByteArray: string);
var
  Temp: TByteStreamInput;
  len, i: integer;
begin
  inherited Create;
  len := Length(Bytearray);
  SetLength(Temp, 0);
  if len > 0 then
  begin
    SetLength(Temp, len);
    for i:=1 to len do
      temp[i - 1] := ord(ByteArray[i]);
  end;
  FValue := Temp;
  FMembers.Add('position', TIntegerInstance.Create(0));
end;

procedure TByteStreamInstance.WriteStream(ByteArray: TByteStreamInput);
var

  position, i, lenContent, totallen: integer;
begin
  position := TIntegerInstance(FMembers.Find('position')).PValue;
  lencontent := Length(ByteArray);
  totalLen := Length(FValue);
  if (position + lencontent) > totallen then
    SetLength(FValue, position + lenContent);
  for i:=0 to (lenContent-1) do
    FValue[i + position] := ByteArray[i];
  TIntegerInstance(FMembers.Find('position')).PValue := position + lenContent;
end;

function TByteStreamInstance.ReadStream: TByteStreamInstance;
var
  Temp: TByteStreamInput;
  len, lenOut, i, start: integer;
begin

  start := TIntegerInstance(Pmembers.Find('position')).PValue;
  len := Length(FValue);

  SetLength(Temp, 0);
  lenout := 0;
  for i:=start to (len-1) do
  begin
    lenOut := lenOut + 1;
    SetLength(Temp, lenout);
    Temp[lenOut - 1] := FValue[i];
  end;
  TIntegerInstance(Pmembers.Find('position')).PValue := len;
  Result := TByteStreamInstance.Create(Temp);
end;

function TByteStreamInstance.ReadStream(count: integer): TByteStreamInstance;
var
  Temp: TByteStreamInput;
  len, lenOut, i, start, limit: integer;
begin

  start := TIntegerInstance(Pmembers.Find('position')).PValue;
  limit := start + count;
  len := Length(FValue);
  if limit > len then
    limit := len;
  SetLength(Temp, 0);
  lenout := 0;
  for i:=start to (limit - 1) do
  begin
    lenOut := lenOut + 1;
    SetLength(Temp, lenout);
    Temp[lenOut - 1] := FValue[i];
  end;
  TIntegerInstance(Pmembers.Find('position')).PValue := limit;
  Result := TByteStreamInstance.Create(Temp);
end;

function TByteStreamInstance.AsString: string;
var
  i: byte;
  ret: string = '';
begin
  for i in FValue do
    ret := ret + IntTostr(i) + ' ';

  result := ret;
end;

function TByteStreamInstance.SaveStream(OutPath: TStringInstance): TBooleanInstance;
var
  strSize, i: integer;
  AStream: TFileStream;
  Buffer:longint;
begin
  Buffer := 0;
  strSize := Length(FValue);
  AStream := TFileStream.Create(OutPath.PStrValue, fmCreate);
  for i:=0 to StrSize - 1 do
    AStream.WriteByte(FValue[i]);
  AStream.Free;
  Result := TBooleanInstance.create(True);
end;

end.

