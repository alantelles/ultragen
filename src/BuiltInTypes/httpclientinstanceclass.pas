unit HttpClientInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, httpprotocol, InstanceOfClass, ARCLass, contnrs;

type THttpClientInstance = class (TInstanceOf)
  private
    FValue: TFPHttpClient;
  public
    property PValue: TFPHttpClient read FValue;

    constructor Create(AClient: TFPHttpClient; AUrl, AResponse: string);
    class function RequestGet(AUrl: string; AHeaders: TDictionaryInstance=nil): THttpClientInstance;
    class function RequestPost(AUrl: string; APayload: TInstanceOf; AHeaders: TDictionaryInstance=nil): THttpClientInstance;
  end;

implementation
uses CoreUtils, StringInstanceCLass;

constructor THttpClientInstance.Create(AClient: TFPHttpClient; AUrl, AResponse: string);
var
  AHeaders: TDictionaryInstance;
  i : integer;
  head, val: string;
begin
  FValue := AClient;
  inherited Create;
  AHeaders := TDictionaryInstance.Create(TActivationRecord.Create('headers', AR_DICT, 1));
  if AClient.ResponseHeaders.Count > 0  then
  begin
    for i:=0 to AClient.ResponseHeaders.Count-1 do
    begin
      head := Copy(AClient.ResponseHeaders[i], 1, Pos(':', AClient.ResponseHeaders[i])-1);
      val := Copy(AClient.ResponseHeaders[i], Pos(':', AClient.ResponseHeaders[i])+2, Length(AClient.ResponseHeaders[i]));
      if lowercase(head) = 'content-type' then
        FMembers.Add('content_type', TStringInstance.Create(Copy(val, 1, pos(';', val)-1)));
      AHeaders.PValue.AddMember(head, TStringInstance.Create(val));
		end;
	end;
	//len := AHeaders.PValue.AddMember(Ahead);
  FMembers.Add('status_text', TStringInstance.Create(AClient.ResponseStatusText));
  FMembers.Add('status', TIntegerInstance.Create(AClient.ResponseStatusCode));
  FMembers.Add('headers', AHeaders);
  FMembers.Add('response', TStringInstance.Create(AResponse));
  FMembers.Add('url', TStringInstance.Create(AUrl));
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
        Return := E.Message;
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
        Return := E.Message;
      end;
    end;
  finally
    SS.Free;
  end;
  Result := THttpClientInstance.Create(Requirer, AUrl, Return);
end;

end.

