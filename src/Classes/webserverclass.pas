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
    FPort: word;
    FLoader: string;
    FRoutesAge: int64;
    FConfig, FLocations: TGenFile;
    FRouterParam: string;
    FRedir: string;
    FHandler: TFPCustomFileModule;
    FServer: TFPHTTPServer;
    FSessionsPath:string;
  public
    property SessionsPath: string read FSessionsPath write FSessionsPath;
    property Port: word read FPort write FPort;
    property Server: TFPHTTPServer read FServer write FServer;
    property Handler: TFPCustomFileModule read FHandler write FHandler;
    property Redir:string read FRedir write FRedir;
    constructor Create(APort: word; AnApp: string = 'index.ultra');
    procedure ExecuteAction(ARequest: TRequest; AResponse: TResponse);
    function CreateSessionID:string;
    procedure RunServer;
  end;

implementation

uses
  TemplateClass,
  FileHandlingUtils,
  StringsFunctions, DateUtils;

constructor TUltraGenServer.Create(APort: word; AnApp: string = 'index.ultra');
var
  APair: TKVPair;
begin
  FPort := APort;
  if AnApp = 'index.ultra' then
    FLoader := AnApp
  else
  begin

    FHandler := TFPCustomFileModule.CreateNew(nil);
    FLocations := TGenFile.Create;
    FConfig := TGenFile.Create;
    FConfig.Load(AnApp + '.gen');
    FLoader := FConfig.GetValue('appLoader').Value + '.ultra';
    FLocations.Load(FConfig.GetValue('locations').Value);
    FRouterParam := FConfig.GetValue('routerParam', 'route').Value;
    for APair in FLocations.Pairs do
    begin
      RegisterFileLocation(APair.Key, APair.Value);
    end;
  end;
  HTTPRouter.RegisterRoute('*', @ExecuteAction);
end;

procedure TUltraGenServer.RunServer;
begin
  WriteLn('Running server at port: ', FPort);
  Application.Title := 'UltraGen server';
  Application.Port := FPort;
  Application.Threaded := True;
  Application.Initialize;
  Application.Run;

end;

function TUltraGenServer.CreateSessionID:string;
var
  ASessionName:string;
  FullPath: string;
  i:integer;
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
  writeln('Requesting: ', ARequest.URI, '. Return code: ', AResponse.Code,
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
  FSessionsPath := AConfig.GetValue('sessionsPath','sessions').Value;
  CreateDirTree(FSessionsPath);

  AGenSet.Add(AConfig, 'appGen');
  if ARequest.CookieFields.IndexOfName('sessionID') < 0 then
  begin
    SessionID := CreateSessionID;
    C := AResponse.Cookies.Add;
    C.Name := 'sessionID';
    C.Value := SessionID;
    ASession := TGenFile.Create;
    ASession.SetValue('sessionID',SessionID);
    ASession.SetValue('expiresAt',FormatDateTime(DATE_INTERCHANGE_FORMAT,IncMinute(Now,StrToInt(FConfig.GetValue('sessionDuration').Value))));
    ASession.SetValue('valid','true');
    SessionFile := FSessionsPath+DirectorySeparator+SessionID+'.gen';
    ASession.FullName := SessionFile;
    ASession.Save;
    ASession.Free;
  end
  else
    SessionID := ARequest.CookieFields.Values['sessionID'];
  SessionFile := FSessionsPath+DirectorySeparator+SessionID+'.gen';
  AGenReq.SetValue('_sessionID',SessionID);
  AGenReq.SetValue('_sessionFile',SessionFile);
  AGenSet.Add(AGenReq, 'requestGen');
  ATemplate := TTemplate.Create(FLoader);
  ATemplate.ParseTemplate(AGenSet);
  DumpTemplate := ATemplate.ParsedLines.Text;
  AGenSet.Free;
  ATemplate.Free;

  AResponse.Content := DumpTemplate;
end;



end.
