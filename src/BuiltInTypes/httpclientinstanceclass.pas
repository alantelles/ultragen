unit HttpClientInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, httpprotocol, InstanceOfClass, ARCLass;

type THttpClientInstance = class (TInstanceOf)
  private
    FValue: TFPHttpClient;
    FResponse: string;

    FUrl: string;
  public
    property PUrl: string read FUrl;
    property PValue: TFPHttpClient read FValue;
    property PResponse: string read FResponse;
    constructor Create(AClient: TFPHttpClient; AUrl, AResponse: string);
    class function RequestGet(AUrl: string; AHeaders: TDictionaryInstance=nil): THttpClientInstance;
    class function RequestPost(AUrl: string; APayload: TInstanceOf; AHeaders: TDictionaryInstance=nil): THttpClientInstance;
  end;

implementation
uses CoreUtils;

constructor THttpClientInstance.Create(AClient: TFPHttpClient; AUrl, AResponse: string);
begin
  FUrl := AUrl;
  FValue := AClient;
  FResponse := AResponse;
end;

class function THttpClientInstance.RequestGet(AUrl: string; AHeaders: TDictionaryInstance=nil): THttpClientInstance;
var
  Requirer: TFPHttpClient;
  Return: string = '';
  i:integer;
begin
  InitSSLInterface;
  Requirer := TFPHttpClient.Create(nil);
  Requirer.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');

  if AHeaders <> nil then
  begin
    if AHeaders.PValue.PMembers.Count > 0 then
    begin
      for i:=0 to AHeaders.PValue.PMembers.Count-1 do
        Requirer.AddHeader(
          AHeaders.PValue.PMembers.NameOfIndex(i),
          TInstanceOf(AHeaders.PValue.PMembers[i]).AsString);
    end;
  end;
  try
    Requirer.AllowRedirect := True;
    try
      Return := Requirer.Get(AUrl);
    except
      On E:Exception do
      begin
        WriteLn(E.Message);
      end;
    end;
  finally
  end;
  Result := THttpClientInstance.Create(Requirer, AUrl, Return);
end;

class function THttpClientInstance.RequestPost(AUrl: string; APayload: TInstanceOf; AHeaders: TDictionaryInstance=nil): THttpClientInstance;
var
  Requirer: TFPHttpClient;
  Return: string = '';
  i:integer;
  SS: TStringStream;
begin
  InitSSLInterface;
  Requirer := TFPHttpClient.Create(nil);
  Requirer.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');

  if AHeaders <> nil then
  begin
    if AHeaders.PValue.PMembers.Count > 0 then
    begin
      for i:=0 to AHeaders.PValue.PMembers.Count-1 do
        Requirer.AddHeader(
          AHeaders.PValue.PMembers.NameOfIndex(i),
          TInstanceOf(AHeaders.PValue.PMembers[i]).AsString);
    end;
  end;
  try
    Requirer.AllowRedirect := True;
    SS := TStringStream.Create(APayload.AsString);
    SS.Position := 0;
    Requirer.RequestBody := SS;
    try
      Return := Requirer.Post(AUrl);
    except
      On E:Exception do
      begin
        WriteLn(E.Message);
      end;
    end;
  finally
    SS.Free;
    Requirer.Free;
  end;
  Result := THttpClientInstance.Create(Requirer, AUrl, Return);
end;

end.

