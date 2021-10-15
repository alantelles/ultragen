unit HttpClientInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, opensslsockets, httpprotocol, InstanceOfClass, ARCLass, contnrs;

type THttpResponseInstance = class (TInstanceOf)
  private
    FValue: TFPHttpClient;

  public

    property PValue: TFPHttpClient read FValue;
    function AsString: string;  override;
    constructor Create(AClient: TFPHttpClient; AUrl, AResponse: string; error: boolean=False);
end;

type THttpClientInstance = class (TInstanceOf)
  public
    class function RequestGet(AMethod, AUrl: string; AHeaders: TDictionaryInstance=nil): THttpResponseInstance;
    class function RequestPost(AMethod, AUrl: string; APayload: TInstanceOf; AHeaders: TDictionaryInstance=nil): THttpResponseInstance;
    constructor Create(BaseUrl: string);
    function DoRequest(Method, Endpoint: string): THttpResponseInstance;
  end;

implementation
uses CoreUtils, StringInstanceClass, byteStreamClass;

constructor THttpClientInstance.Create(BaseUrl:string);
begin
  inherited Create;
  FMembers.Add('$baseUrl', TStringInstance.Create(baseUrl));
  FMembers.Add('headers', TNullInstance.Create);
  FMembers.Add('timeout', TIntegerInstance.Create(10));
  FMembers.Add('allowRedirects', TBooleanInstance.Create(True));
  FMembers.Add('body', TStringInstance.Create(''));
end;

function THttpClientInstance.DoRequest(Method, Endpoint: string): THttpResponseInstance;
var
  AClient: TFPHttpClient;
  i, len: integer;
  AHeaders: TDictionaryInstance;
  Ret: THttpResponseInstance;
  AUrl, AResponse: string;
begin                                                                  
  InitSSLInterface;
  AClient := TFPHttpClient.Create(nil);
  AClient.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb/UltraGen)');
  AUrl := TStringInstance(FMembers.Find('$baseUrl')).PValue + '/' + Endpoint;
  Method := lowercase(Method);
  AClient.AllowRedirect := TInstanceOf(FMembers.Find('allowRedirects')).PBoolValue;
  AClient.RequestBody := TStringStream.Create(TInstanceOf(FMembers.Find('body')).AsString);
  if FMembers.Find('headers').ClassName = 'TDictionaryInstance' then
  begin
    AHeaders := TDictionaryInstance(FMembers.Find('headers'));
    len := AHeaders.PValue.PMembers.Count;
    if len > 0 then
    begin
      for i:=0 to len - 1 do
        AClient.AddHeader(AHeaders.PValue.PMembers.NameOfIndex(i), TInstanceOf(AHeaders.PValue.PMembers[i]).AsString);

    end;

  end;
  AClient.ConnectTimeout := TIntegerInstance(FMembers.Find('timeout')).PValue * 1000;
  // TODO: handle form files send

  try
    if Method = 'get' then
      AResponse := AClient.Get(AUrl)
    else if Method = 'post' then
      AResponse := AClient.Post(AUrl)
    else if Method = 'put' then
      AResponse := AClient.Put(AUrl)
    else if MEthod = 'delete' then
      AResponse := AClient.Delete(AUrl);
    Ret := THttpResponseInstance.Create(AClient, AUrl, AResponse);
  except on E: Exception do
    Ret := THttpResponseInstance.Create(AClient, Aurl, E.Message, True);
  end;

  Result := Ret;
end;

constructor THttpResponseInstance.Create(AClient: TFPHttpClient; AUrl, AResponse: string; error: boolean= False);
var
  AHeaders: TDictionaryInstance;
  i : integer;
  head, val: string;
begin
  FValue := AClient;
  Ferror := error;
  if Ferror then
    Ferrormsg := AREsponse;
  inherited Create;
  AHeaders := TDictionaryInstance.Create(TActivationRecord.Create('headers', AR_DICT, 1));
  if AClient.ResponseHeaders.Count > 0  then
  begin
    for i:=0 to AClient.ResponseHeaders.Count-1 do
    begin
      head := Copy(AClient.ResponseHeaders[i], 1, Pos(':', AClient.ResponseHeaders[i])-1);
      val := Copy(AClient.ResponseHeaders[i], Pos(':', AClient.ResponseHeaders[i])+2, Length(AClient.ResponseHeaders[i]));
      if lowercase(head) = 'content-type' then
        FMembers.Add('contentType', TStringInstance.Create(Copy(val, 1, pos(';', val)-1)));
      AHeaders.PValue.AddMember(head, TStringInstance.Create(val));
		end;
	end;
	//len := AHeaders.PValue.AddMember(Ahead);
  FMembers.Add('statusText', TStringInstance.Create(AClient.ResponseStatusText));
  FMembers.Add('status', TIntegerInstance.Create(AClient.ResponseStatusCode));
  FMembers.Add('headers', AHeaders);
  FMembers.Add('text', TStringInstance.Create(AResponse));
  FMembers.Add('content', TByteStreamInstance.Create(AResponse));
  FMembers.Add('url', TStringInstance.Create(AUrl));
end;

function THttpResponseInstance.AsString: string;
begin
  Result := '<Response from endpoint "'+TInstanceOf(FMembers.Find('url')).AsString+'">';
end;

class function THttpClientInstance.RequestGet(AMethod, AUrl: string; AHeaders: TDictionaryInstance=nil): THttpResponseInstance;
var
  Requirer: TFPHttpClient;
  Return: string = '';
  i:integer;
begin
  InitSSLInterface;
  Requirer := TFPHttpClient.Create(nil);
  Requirer.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb/UltraGen)');
  Requirer.IOTimeout := 60000;
  Requirer.ConnectTimeout := 60000;
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
      if AMethod = 'delete' then
        Return := Requirer.Delete(AUrl)
      else
        Return := Requirer.Get(AUrl);
    except
      On E:Exception do
      begin
        Return := E.Message;
      end;
    end;
  finally
  end;
  Result := THttpResponseInstance.Create(Requirer, AUrl, Return);
end;

class function THttpClientInstance.RequestPost(AMethod, AUrl: string; APayload: TInstanceOf; AHeaders: TDictionaryInstance=nil): THttpResponseInstance;
var
  Requirer: TFPHttpClient;
  Return: string = '';
  i:integer;
  SS: TStringStream;
begin
  InitSSLInterface;
  Requirer := TFPHttpClient.Create(nil);
  Requirer.IOTimeout := 15000;
  Requirer.ConnectTimeout := 15000;
  Requirer.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb/UltraGen)');

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
      if AMethod = 'put' then
        Return := Requirer.Put(AUrl)

      else
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
  Result := THttpResponseInstance.Create(Requirer, AUrl, Return);
end;

end.

