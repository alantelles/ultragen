unit WebServerClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, httpdefs, httproute, fphttpapp,

      { Classes }
      TemplateClass, ParserClass, GenFileSetClass,

      { Globals }
      ConstantsGlobals, VariablesGlobals, TypesGlobals
      ;

type
  TUltraGenServer = class
  private
    FPort: word;
    FLoader: string;
  property
    Port: word read FPort write FPort;
  public
    constructor Create(APort:word; ALoader:string);
    procedure ExecuteAction(ARequest: TRequest; AResponse: TResponse);
    procedure RunServer;
    procedure RegisterRoutes;
  end;

implementation
  constructor TUltraGenServer.Create(APort:word; ALoader:string);
  begin
    FPort := APort;
    FLoader := ALoader;
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
    DumpTemplate: string;
  begin
    AGenSet := TGenFileSet.Create;

    ATemplate := TTemplate.Create(FLoader);
    ATemplate.ParseTemplate(AGenSet);
    DumpTemplate := ATemplate.ParsedLines.Text;
    AGenSet.Free;
    ATemplate.Free;
    AResponse.Content := DumpTemplate;
  end;



end.

