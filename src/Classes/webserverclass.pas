unit WebServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, httproute, fphttpapp, fphttpserver, fpwebfile,

  { Classes }
  GenFileSetClass, GenFileClass,

  { Globals }
  ConstantsGlobals, TypesGlobals;

type
  TUltraGenServer = class
  private
    FDontServe:boolean;
    FPort: word;
    FLoader: string;
    FRoutesAge: int64;
    FConfig, FLocations: TGenFile;
    FHandler: TFPCustomFileModule;
    FServer: TFPHTTPServer;
    FSessionsPath, FSessionFile:string;
    FSessionDuration:integer;
  public
    property SessionsPath: string read FSessionsPath write FSessionsPath;
    property Port: word read FPort write FPort;
    property Server: TFPHTTPServer read FServer write FServer;
    property Handler: TFPCustomFileModule read FHandler write FHandler;
    constructor Create(APort: word; AnApp: string; Mode:string);
    procedure ExecuteAction(ARequest: TRequest; AResponse: TResponse);
    function RunServer:boolean;
  end;

implementation

uses
  TemplateClass,
  FileHandlingUtils, VariablesGlobals,
  StringsFunctions, DateUtils,
  Character;

constructor TUltraGenServer.Create(APort: word; AnApp: string; Mode:string);
var
  Aux:string;
  APair: TKVPair;
begin
  FDontServe := False;
  FPort := APort;
  FLocations := TGenFile.Create;
  FConfig := TGenFile.Create;
  FConfig.Load(GetFileName(AnApp,False) + '.gen');
  Aux := FConfig.GetValue('_appLoader').Value;
  if Trim(Aux) <> '' then
  begin
    if FileExists(Aux) then
      FLoader := Aux
    else
    begin
      WriteLn('App Loader doesn''t exist. Exitting');
      FDontServe := True;
      Exit;
    end;
  end
  else
  begin
    WriteLn('App Loader not defined.');
    WriteLn('See docs for details. Exitting.');
    FDontServe := True;
    Exit;
  end;

  Aux := FConfig.GetValue('_locations').Value;
  if Trim(Aux) <> '' then
  begin
    if FileExists(Aux) then
    begin
      FLocations.Load(Aux);
      for APair in FLocations.Pairs do
      begin
        CreateDirTree(APair.Value,False);
        RegisterFileLocation(APair.Key, APair.Value);
      end;
    end
    else
    begin
      WriteLn('Locations gen doesn''t exist. Exitting.');
      FDontServe := True;
      Exit;
    end;
  end
  else
  begin
    WriteLn('Locations gen not defined. No static file will be served.');
    WriteLn('See docs for details.');
  end;

  Aux := FConfig.GetValue('_sessionsPath').Value;
  if (Trim(Aux) <> '') then
    FSessionsPath := Aux
  else
  begin
    WriteLn('Session path not defined.');
    WriteLn('Using "./sessions"');
    FSessionsPath := 'sessions';
  end;
  CreateDirTree(FSessionsPath+DirectorySeparator,False);

  Aux := FConfig.GetValue('_sessionDuration').Value;
  if (Trim(Aux) <> '') then
  begin
    try
      FSessionDuration := StrToInt(Aux);
    except
      WriteLn('Session duration is not a number. Using 120 minutes as value.');
      FSessionDuration := 120;
    end;
  end
  else
  begin
    WriteLn('Session duration not defined.');
    WriteLn('Using 120 minutes.');
    FSessionDuration := 120;
  end;

  Aux := FConfig.GetValue('_mimeTypesFile').Value;
  if Trim(Aux) <> '' then
  begin
    if FileExists(Aux) then
      MimeTypesFile := Aux
    else
    begin
      WriteLn('MimeTypesFile doesn''t exist. Using default static load.');
    end;
  end
  else
  begin
    WriteLn('Mime Types File not defined. Using default static load.');
  end;
  HTTPRouter.RegisterRoute('*', @ExecuteAction);
end;

function TUltraGenServer.RunServer:boolean;
begin
  if not FDontServe then
  begin
    WriteLn('Running server at port: ', FPort);
    Application.Title := 'UltraGen server 1.0';
    Application.Port := FPort;
    Application.Threaded := True;
    Application.Initialize;
    Application.Run;
    Result := True;
  end
  else
    Result := False;
end;


procedure TUltraGenServer.ExecuteAction(ARequest: TRequest;AResponse: TResponse);
var
  C: TCookie;
  ATemplate: TTemplate;
  AGenSet: TGenFileSet;
  AGenReq, AConfig, ASession, AFileGen: TGenFile;
  D: TGenFileRecord;
  DumpTemplate, Route, s: string;
  i: integer;
  SessionID, ExpireTime, SessionFile:string;
  AnAlias:string;
begin
  if FDontServe then
    Exit;
  WriteLn('Requesting: ', ARequest.URI, '. Return code: ', AResponse.Code,
    ' ', AResponse.CodeText);
  AGenSet := TGenFileSet.Create;
  AGenReq := TGenFile.Create;

  AConfig := TGenFile.Create;
  for i := 0 to Length(FConfig.Pairs) - 1 do
    AConfig.SetValue(FConfig.Pairs[i].Key, FConfig.Pairs[i].Value);

  if ARequest.QueryFields.Count > 0 then
  begin
    for i := 0 to ARequest.QueryFields.Count - 1 do
      AGenReq.SetValue('_get:' + ARequest.QueryFields.Names[i],
        ARequest.QueryFields.ValueFromIndex[i]);
  end;

  if ARequest.Files.Count > 0 then
  begin
    //alias: _files:[fieldName]:0
    for i:=0 to ARequest.Files.Count - 1 do
    begin
      AFileGen := TGenFile.Create;
      AFileGen.SetValue('fileName',ARequest.Files[i].FileName);
      AFileGen.SetValue('size',IntToStr(ARequest.Files[i].Size));
      AFileGen.SetValue('serverName',ARequest.Files[i].LocalFileName);
      AnAlias := '_files:'+ARequest.Files[i].FieldName+':'+IntToStr(i);
      AGenSet.Add(AFileGen,AnAlias);
    end;
    {
    Property FieldName: String Read FFieldName Write FFieldName;
  Property FileName: String Read FFileName Write FFileName;
  Property Stream: TStream Read GetStream;
  Property Size: Int64 Read FSize Write FSize;
  Property ContentType: String Read FContentType Write FContentType;
  Property Disposition: String Read FDisposition Write FDisposition;
  Property LocalFileName: String Read FLocalFileName Write FLocalFileName;
  Property Description: String Read FDescription Write FDescription;
    }
  end;

  if ARequest.ContentFields.Count > 0 then
  begin
    for i := 0 to ARequest.ContentFields.Count - 1 do
      AGenReq.SetValue('_post:' + ARequest.ContentFields.Names[i],
        ARequest.ContentFields.ValueFromIndex[i]);
  end;

  if ARequest.CookieFields.Count > 0 then
  begin
    for i := 0 to ARequest.CookieFields.Count - 1 do
      AGenReq.SetValue('_cookie:' + ARequest.CookieFields.Names[i],
        ARequest.CookieFields.ValueFromIndex[i]);
  end;

  i := Pos('?',ARequest.URI);
  if i > 0 then
    Route := Copy(ARequest.URI,1,i-1)
  else
    Route := ARequest.URI;

  AGenReq.SetValue('_route', Route);
  AGenReq.SetValue('_method', ARequest.Method);
  AGenSet.Add(AConfig, 'app');

  ASession := TGenFile.Create;

  if ARequest.CookieFields.IndexOfName('sessionID') > -1 then
  begin
    SessionId := ARequest.CookieFields.Values['sessionID'];
    SessionFile := FSessionsPath+DirectorySeparator+SessionId+'.gen';
    if not FileExists(SessionFile) then
    begin
      ASession.SetValue('_session:sessionID',SessionId);
      ASession.SetValue('_session:expiresAt',FormatDateTime(
          DATE_INTERCHANGE_FORMAT,IncMinute(Now,FSessionDuration)
      ));
      ASession.Save(FSessionsPath+DirectorySeparator+SessionID+'.gen');
    end
    else
    begin
      ASession.Load(SessionFile);
    end;

  end
  else
  begin
    ASession.ClearValues;
    SessionFile := '';
    SessionId := '';
  end;

  AGenReq.SetValue('_sessionID',SessionID);
  AGenReq.SetValue('_sessionFile',SessionFile);
  AGenSet.Add(ASession, 'session');
  AGenSet.Add(AGenReq, 'request');
  ATemplate := TTemplate.Create(FLoader);
  ATemplate.SetWebVars(SessionId, FSessionsPath, FSessionDuration);
  ATemplate.ParseTemplate(AGenSet);
  ATemplate.ParsedLines.SaveToFile('zika.txt');
  DumpTemplate := ATemplate.ParsedLines.Text;
  AGenSet.Free;
  ATemplate.Free;

  AResponse.Content := DumpTemplate;
end;



end.
