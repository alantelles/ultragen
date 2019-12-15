unit WebServerClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, httpdefs, httproute, fphttpapp, fpwebfile,

      { Classes }
      TemplateClass, ParserClass, GenFileSetClass, GenFileClass,

      { Globals }
      ConstantsGlobals, VariablesGlobals, TypesGlobals
      ;

type
  TUltraGenServer = class
  private
    FPort: word;
    FLoader: string;
    FConfig: TGenFile;
    FRouterParam: string;
  property
    Port: word read FPort write FPort;
  public
    constructor Create(APort:word; AnApp:string='index.ultra');
    procedure ExecuteAction(ARequest: TRequest; AResponse: TResponse);
    procedure RunServer;
    procedure RegisterRoutes;
  end;

implementation
  constructor TUltraGenServer.Create(APort:word; AnApp:string='index.ultra');
  begin
    FPort := APort;
    if AnApp = 'index.ultra' then
      FLoader := AnApp
    else
    begin
      FConfig := TGenFile.Create;
      FConfig.Load(AnApp+'.gen');
      FLoader := FConfig.GetValue('appLoader').Value+'.ultra';
      FRouterParam := FConfig.GetValue('routerParam','route').Value;
		end;
    RegisterFileLocation('static','/home/alantelles/development/ultragen/webpocmvc/static');
    HTTPRouter.RegisterRoute('*', @ExecuteAction);
	end;

  procedure TUltraGenServer.RegisterRoutes;
  begin
  //TODO
  end;
  procedure TUltraGenServer.RunServer;
  begin
    WriteLn('Running server at port: ',FPort);
    Application.Title := 'UltraGen server';
    Application.Port := FPort;
    Application.Initialize;
    Application.Run;
	end;

  procedure TUltraGenServer.ExecuteAction(ARequest: TRequest; AResponse: TResponse);
  var
    ATemplate: TTemplate;
    AGenSet: TGenFileSet;
    AGenReq, AConfig: TGenFile;
    RouterParam: string='r';
    DumpTemplate: string;
    i: integer;
  begin
    AGenSet := TGenFileSet.Create;
    AGenReq := TGenFile.Create;
    AConfig := TGenFile.Create;
    for i:=0 to Length(FConfig.Pairs)-1 do
      AConfig.SetValue(FConfig.Pairs[i].Key, FConfig.Pairs[i].Value);
    AGenReq.SetValue('_route',ARequest.QueryFields.Values[FRouterParam]);
    AGenSet.Add(AGenReq,'requestGen');
    AGenSet.Add(AConfig,'appGen');
    ATemplate := TTemplate.Create(FLoader);
    ATemplate.ParseTemplate(AGenSet);
    DumpTemplate := ATemplate.ParsedLines.Text;
    AGenSet.Free;
    ATemplate.Free;
    AResponse.Content := DumpTemplate;
  end;



end.

