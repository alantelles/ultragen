unit bytestreamclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, ARClass, StringInstanceclass, ListInstanceClass;

  type TByteStreamInput = array of byte;

  type TByteStreamInstance = class (TInstanceOf)
    protected
      FValue: TByteStreamInput;
    public

      constructor Create(ByteArray: TByteStreamInput);
      function SaveStream(OutPath: TStringInstance): TBooleanInstance;
      function AsString: string;  override;
  end;




implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
   Dos, UltraGenInterfaceClass, ResponseHandlerClass;

constructor TByteStreamInstance.Create(ByteArray: TByteStreamInput);
var
  i:TInstanceOf;
begin
  inherited Create;
  FValue := ByteArray;
  //FValue.LoadFromStream(TStringStream.Create(TStringInstance(AInstance).PValue));
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
  strSize: integer;
begin
  Result := TBooleanInstance.create(True);
end;

end.

