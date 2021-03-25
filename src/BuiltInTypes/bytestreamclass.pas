unit bytestreamclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, ARClass, StringInstanceclass;

  type TByteStreamInstance = class (TInstanceOf)
    protected
      FValue: TMemoryStream;
    public
      property PStream: TMemoryStream read FValue write FValue;
      constructor Create(AInstance: TInstanceOf);
      function SaveStream(OutPath: TStringInstance): TBooleanInstance;
  end;




implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
   Dos, UltraGenInterfaceClass, ResponseHandlerClass;

constructor TByteStreamInstance.Create(AInstance: TInstanceOf);
begin
  inherited Create;
  FValue := TMemoryStream.Create;
  FValue.LoadFromStream(TStringStream.Create(TStringInstance(AInstance).PValue));
end;

function TByteStreamInstance.SaveStream(OutPath: TStringInstance): TBooleanInstance;
begin
  try
    FValue.SaveToFile(OutPath.PValue);
    Result := TBooleanInstance.Create(True);
  except
    Result := TBooleanInstance.Create(False);
  end;

end;

end.

