unit ServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass,
  httpdefs, httproute, fphttpapp, fphttpserver, fpwebfile;

  type TServerInstance = class (TInstanceOf)
    protected
      FTitle: string;
      FPort: integer;
      FRootFile: string;
      FStopRoute: string;
    public
      property PStopRoute: string read FStopRoute write FStopRoute;
      property PRootFile: string read FRootFile write FRootFile;
      property PTitle: string read FTitle write FTitle;
      property PPort: integer read FPort write FPort;
      procedure ExecuteAction(ARequest: TRequest;AResponse: TResponse);
      procedure StopServer(ARequest: TRequest;AResponse: TResponse);

      procedure SetServerStopRoute(ARoute:string);
      procedure RunServer;
      constructor Create;
  end;

implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass;

constructor TServerInstance.Create;
begin
  FRootFile := 'index.ultra';
  FStopRoute := '';
end;

procedure TServerInstance.SetServerStopRoute(ARoute:string);
begin
  FStopRoute := ARoute;
end;

procedure TServerInstance.StopServer(ARequest: TRequest;AResponse: TResponse);
begin
  AResponse.Content := 'Server stopped';
  WriteLn('Stopping server');
  Application.Terminate;

  WriteLn('Server stopped');
end;

procedure TServerInstance.ExecuteAction(ARequest: TRequest;AResponse: TResponse);
var
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  AprogTree: TAST;
  AInter: TInterpreter;
  Output: string;
  AToken, BToken: TToken;
begin

  ALexer := TLexer.Create(FRootFile);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  AToken := TToken.Create(T_ID, '_SESSION');
  BToken := TToken.Create(TYPE_STRING, 'Uma variável de sessão');
  TProgram(ATree).AddPrelude(TVarAssign.Create(AToken, TString.Create(BToken)));
  AInter := TInterpreter.Create(ATree);
  AInter.Interpret;
  Output := AInter.PLive;     
  AInter.Free;
  AResponse.Content := Output;
  WriteLn('['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
    ARequest.Method + ': '+
    ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
    ' ' + AResponse.CodeText +
    ', ' + IntToStr(AResponse.ContentLength) + ' B');
end;

procedure TServerInstance.RunServer;
begin
  if FStopRoute <> '' then
    HTTPRouter.RegisterRoute(FStopRoute, @StopServer);
  HTTPRouter.RegisterRoute('*', @ExecuteAction);
  WriteLn('Running '+FTitle+' in '+'UltraGen Builtin Development Server at port '+IntToStr(FPort));
  Application.Title := 'UltraGen Builtin Development Server';
  Application.Port := FPort;
  Application.Threaded := True;
  Application.Initialize;
  Application.Run;
end;

end.

