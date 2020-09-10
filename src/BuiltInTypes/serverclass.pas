unit ServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, ARClass,
  httpdefs, httproute, fphttpapp, fphttpserver, fpwebfile{,
    blcksock, sockets, Synautil};

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
      procedure ExecuteAction(ARequest: TRequest; AResponse: TResponse);
      procedure StopServer(ARequest: TRequest;AResponse: TResponse);
      procedure SetStaticPath(AALias, APath: string);
      procedure SetStaticPaths(AStatic: TActivationRecord);
      procedure SetMimeTypesFile(AFile:string);

      //procedure AttendConnection(ASocket: TTCPBlockSocket);

      procedure SetServerStopRoute(ARoute:string);
      procedure RunServer;
      constructor Create(APort: integer);
  end;


procedure ShowException(AResponse: TResponse; AException: Exception; var Switch: boolean);

implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
  StringInstanceClass, Dos, UltraGenInterfaceClass;

constructor TServerInstance.Create(APort: integer);
begin
  inherited Create;
  MimeTypesFile := GetEnv('ULTRAGEN_HOME') + DirectorySeparator + 'assets' + DirectorySeparator + 'mime-types.txt';
  FMembers.Add('title', TStringInstance.Create('Untitled application'));
  FPort := Aport;
  FRootFile := 'index.ultra';
  FStopRoute := '';
end;

procedure TServerInstance.SetStaticPath(AALias, APath: string);
begin
  ForceDirectories(Apath);
  RegisterFileLocation(AAlias, APath);
end;

procedure TServerInstance.SetStaticPaths(AStatic: TActivationRecord);
var
  i: integer;
begin
  if AStatic.PMembers.Count > 0 then
  begin
    for i:=0 to Astatic.PMembers.Count-1 do
    begin
      ForceDirectories(TStringInstance(AStatic.PMembers[i]).PValue);
      RegisterFileLocation(AStatic.PMembers.NameOfIndex(i), TStringInstance(AStatic.PMembers[i]).PValue);
    end;
  end;
end;

procedure TServerInstance.SetServerStopRoute(ARoute:string);
begin
  FStopRoute := ARoute;
end;

procedure TServerInstance.SetMimeTypesFile(AFile:string);
begin
  MimeTypesFile := AFile;
end;

procedure TServerInstance.StopServer(ARequest: TRequest;AResponse: TResponse);
begin
  AResponse.Content := 'Server stopped';
  WriteLn('Stopping server');
  Application.Terminate;

  WriteLn('Server stopped');
end;

procedure ShowException(AResponse: TResponse; AException: Exception; var Switch: boolean);
begin

end;

procedure TServerInstance.ExecuteAction(ARequest: TRequest;AResponse: TResponse);
var
  BTree: TAST;
begin
  BTree := TUltraInterface.ParseWebRequest(ARequest);
  try
    AResponse.Content := TUltraInterface.InterpretScript(FRootFile, TProgram(BTree));
    WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
      ' ' + AResponse.CodeText +
      ', ' + IntToStr(AResponse.ContentLength) + ' B', #13);
  except
    on E: Exception do
    begin
       AResponse.Code := 500;
       AResponse.CodeText := 'Internal server error';
       AResponse.Content := '<h1>UltraGen ERROR!</h1><pre style="white-space: pre-wrap; font-size: 12pt">'+ReplaceStr(E.Message, '<', '&lt') +'</pre>';
       WriteLn('['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
        ARequest.Method + ': '+
        ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
        ' ' + AResponse.CodeText +
        ', ' + IntToStr(AResponse.ContentLength) + ' B', #13);
       WriteLn(E.Message);
    end;
  end;
end;


procedure TServerInstance.RunServer;
begin
  begin
    if FStopRoute <> '' then
      HTTPRouter.RegisterRoute(FStopRoute, @StopServer);
    HTTPRouter.RegisterRoute('*', @ExecuteAction);
    WriteLn('Running '+TInstanceOf(FMembers.Find('title')).AsString+' in '+'UltraGen Builtin Development Server at port '+IntToStr(FPort), #13);
    Application.Title := 'UltraGen Builtin Development Server';
    Application.Port := FPort;
    Application.Threaded := True;
    Application.OnShowRequestException := @ShowException;
    Application.Initialize;
    Application.Run;

  end;
end;

end.

