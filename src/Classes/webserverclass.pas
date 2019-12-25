unit WebServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, httproute, fphttpapp, fphttpserver, fpwebfile,

  { Classes }
  GenFileSetClass, GenFileClass,

  { Globals }
  ConstantsGlobals, VariablesGlobals, TypesGlobals;

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
    function CreateSessionID:string;
    function RunServer:boolean;
  end;

implementation

uses
  TemplateClass,
  FileHandlingUtils,
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
  Aux := FConfig.GetValue('_appLoader').Value+'.ultra';
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

  Aux := FConfig.GetValue('_sessionDuration').Value;
  if (Trim(Aux) <> '') then
  begin
    try
      FSessionDuration := StrToInt(Aux);

    finally
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

  Aux := FConfig.GetValue('_sessionsPath').Value;
  if (Trim(Aux) <> '') then
    FSessionsPath := Aux
  else
  begin
    WriteLn('Session path not defined.');
    WriteLn('Using "./sessions"');
    FSessionsPath := 'sessions';
  end;
  CreateDirTree(FSessionsPath);

  Aux := FConfig.GetValue('_locations').Value;
  if Trim(Aux) <> '' then
  begin
    if FileExists(Aux) then
    begin
      FLocations.Load(Aux);
      for APair in FLocations.Pairs do
      begin
        if DirectoryExists(APair.Value+DirectorySeparator) then
          RegisterFileLocation(APair.Key, APair.Value)
        else
          Writeln('Location ',APair.Value,' doesn''t exist. Skipping creation');
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

function TUltraGenServer.CreateSessionID:string;
var
  ASessionName:string;
  FullPath: string;
begin
  ASessionName :=  StrToMd5(
    FormatDateTime('yyyymmddhhnnsszzz',now)+
    IntToStr(Random(Int64(HourOf(now))))+
    IntToStr(MilliSecondOf(now))+
    IntToStr(Random(Int64(MinuteOf(now)+SecondOf(Now))))
  );
  ASessionName := ASessionName + StrToMd5(
    FormatDateTime('yyyymmddhhnnsszzz',now)+
    IntToStr(Random(Int64(HourOf(now))))+
    IntToStr(MilliSecondOf(now))+
    IntToStr(Random(Int64(MinuteOf(now)+SecondOf(Now))))
  );
  Result := ASessionName;

end;
procedure TUltraGenServer.ExecuteAction(ARequest: TRequest;AResponse: TResponse);
var
  C: TCookie;
  ATemplate: TTemplate;
  AGenSet: TGenFileSet;
  AGenReq, AConfig, ASession: TGenFile;
  DumpTemplate, Route: string;
  i: integer;
  SessionID, ExpireTime, SessionFile:string;
begin
  if FDontServe then
    Exit;
  WriteLn('Requesting: ', ARequest.URI, '. Return code: ', AResponse.Code,
    ' ', AResponse.CodeText);
  AGenSet := TGenFileSet.Create;
  AGenReq := TGenFile.Create;

  AConfig := TGenFile.Create;
  try
    for i := 0 to Length(FConfig.Pairs) - 1 do
    begin
      AConfig.SetValue(FConfig.Pairs[i].Key, FConfig.Pairs[i].Value);
	  end;

  finally
  end;


  if ARequest.QueryFields.Count > 0 then
  begin
    for i := 0 to ARequest.QueryFields.Count - 1 do
      AGenReq.SetValue('_get:' + ARequest.QueryFields.Names[i],
        ARequest.QueryFields.ValueFromIndex[i]);
  end;

  if ARequest.ContentFields.Count > 0 then
  begin
    for i := 0 to ARequest.ContentFields.Count - 1 do
      AGenReq.SetValue('_post:' + ARequest.ContentFields.Names[i],
        ARequest.ContentFields.ValueFromIndex[i]);
  end;

  i := Pos('?',ARequest.URI);
  if i > 0 then
    Route := Copy(ARequest.URI,1,i-1)
  else
    Route := ARequest.URI;

  AGenReq.SetValue('_route', Route);
  AGenReq.SetValue('_method', ARequest.Method);
  try
    FSessionsPath := AConfig.GetValue('_sessionsPath','core/sessions').Value;

  except
    FSessionsPath := 'core/sessions';
  end;
  CreateDirTree(FSessionsPath);
  if ARequest.CookieFields.IndexOfName('sessionID') > 0 then
  begin
    ASession := TGenFile.Create;
    ASession.SetValue('expiresAt',FormatDateTime(DATE_INTERCHANGE_FORMAT,IncMinute(Now,StrToInt(AConfig.GetValue('_sessionDuration').Value))));
    ASession.Save(FSessionsPath+DirectorySeparator+SessionID+'.gen');
    ASession.Free;
  end;

   {
  if ARequest.CookieFields.IndexOfName('sessionID') < 0 then
  begin
    SessionID := CreateSessionID;
    C := AResponse.Cookies.Add;
    C.Name := 'sessionID';
    C.Value := SessionID;
    ASession := TGenFile.Create;
    ASession.SetValue('sessionID',SessionID);
    ASession.SetValue('expiresAt',FormatDateTime(DATE_INTERCHANGE_FORMAT,IncMinute(Now,StrToInt(AConfig.GetValue('_sessionDuration').Value))));
    ASession.SetValue('valid','true');
    SessionFile := FSessionsPath+DirectorySeparator+SessionID+'.gen';
    ASession.FullName := SessionFile;
    ASession.Save;
    ASession.Free;
  end
  else
  begin
    SessionFile := FSessionsPath+DirectorySeparator+SessionID+'.gen';
    if not FileExists (SessionFile) then
    begin
      SessionID := ARequest.CookieFields.Values['sessionID'];
      ASession := TGenFile.Create;
      ASession.SetValue('sessionID',SessionID);
      ASession.SetValue('expiresAt',FormatDateTime(DATE_INTERCHANGE_FORMAT,IncMinute(Now,StrToInt(AConfig.GetValue('_sessionDuration').Value))));
      ASession.SetValue('valid','true');
      SessionFile := FSessionsPath+DirectorySeparator+SessionID+'.gen';
      ASession.FullName := SessionFile;
      ASession.Save;
      ASession.Free;
		end;
		SessionID := ARequest.CookieFields.Values['sessionID'];
	end;
   }
  AGenReq.SetValue('_sessionID',SessionID);
  AGenReq.SetValue('_sessionFile',SessionFile);
  try
    AGenSet.Add(AConfig, 'appGen');

  finally
  end;
  AGenSet.Add(AGenReq, 'requestGen');
  ATemplate := TTemplate.Create(FLoader);
  ATemplate.SetWebVars(SessionFile,SessionId, FSessionsPath);
  ATemplate.ParseTemplate(AGenSet);
  DumpTemplate := ATemplate.ParsedLines.Text;
  AGenSet.Free;
  ATemplate.Free;

  AResponse.Content := DumpTemplate;
end;



end.
