unit ServerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass,
  httpdefs, httproute, fphttpapp, fphttpserver, fpwebfile,
    blcksock, sockets, Synautil;

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

      procedure AttendConnection(ASocket: TTCPBlockSocket);

      procedure SetServerStopRoute(ARoute:string);
      procedure RunServer;
      constructor Create;
  end;


procedure ShowException(AResponse: TResponse; AException: Exception; var Switch: boolean);

implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass, StrUtils;

constructor TServerInstance.Create;
begin
  FPort := 2020;
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

procedure ShowException(AResponse: TResponse; AException: Exception; var Switch: boolean);
begin

end;

procedure TServerInstance.AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout: integer;
  s, resp: string;


  method, uri, protocol: string;
  OutputDataString: string;
  ResultCode: integer;
begin
  timeout := 120000;

  WriteLn('Received headers+document from browser:');

  //read request line
  s := ASocket.RecvString(timeout);
  resp := s;
  WriteLn(s);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');

  //read request headers
  repeat
    s := ASocket.RecvString(Timeout);
    resp := resp + s;
    WriteLn(s);
  until s = '';

  // Now write the document to the output stream

  //if uri = '/' then
  //begin

    // Write the output document to the stream
    OutputDataString :=
      '<!DOCTYPE html>' + CRLF
      + '<html><h1>Teste</h1><p>'+uri+'</p></html>' + CRLF;

    // Write the headers back to the client
    ASocket.SendString('HTTP/1.1 200' + CRLF);
    ASocket.SendString('Content-type: Text/Html' + CRLF);
    ASocket.SendString('Content-length: ' + IntTostr(Length(OutputDataString)) + CRLF);
    ASocket.SendString('Connection: close' + CRLF);
    ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    ASocket.SendString('Server: Servidor do Felipe usando Synapse' + CRLF);
    ASocket.SendString('' + CRLF);

  //  if ASocket.lasterror <> 0 then HandleError;

    // Write the document back to the browser
    ASocket.SendString(OutputDataString);
  //end
  //else
    //ASocket.SendString('HTTP/1.0 404' + CRLF);
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
  // commented to show concept
  //AToken := TToken.Create(T_ID, '_SESSION');
  //BToken := TToken.Create(TYPE_STRING, 'Uma variável de sessão');
  //TProgram(ATree).AddPrelude(TVarAssign.Create(AToken, TString.Create(BToken)));
  AInter := TInterpreter.Create(ATree);
  try
    AInter.Interpret;
    Output := AInter.PLive;
    //AInter.FreeInstances;
    AInter.Free;
    AParser.Free;
    ALexer.Free;
    AResponse.Content := Output;
    AResponse.SendResponse;
    AResponse.CleanupInstance;
    WriteLn('['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
      ' ' + AResponse.CodeText +
      ', ' + IntToStr(AResponse.ContentLength) + ' B', #13);

  except
    on E: Exception do
    begin
       AInter.FreeInstances;
       AInter.Free;
       AResponse.Code := 500;
       AResponse.CodeText := 'Internal server error';
       AResponse.Content := '<pre style="font-size: 12pt">'+ReplaceStr(E.Message, '<', '&lt') +'</pre>';
       WriteLn('['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
        ARequest.Method + ': '+
        ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
        ' ' + AResponse.CodeText +
        ', ' + IntToStr(AResponse.ContentLength) + ' B', #13);
       writeln(E.Message);
    end;
  end;
end;

procedure TServerInstance.RunServer;
{}
  {var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;}

begin
  begin
    if FStopRoute <> '' then
      HTTPRouter.RegisterRoute(FStopRoute, @StopServer);
    HTTPRouter.RegisterRoute('*', @ExecuteAction);
    WriteLn('Running '+FTitle+' in '+'UltraGen Builtin Development Server at port '+IntToStr(FPort), #13);
    Application.Title := 'UltraGen Builtin Development Server';
    Application.Port := FPort;
    Application.Threaded := True;
    Application.OnShowRequestException := @ShowException;
    Application.Initialize;
    Application.Run;

  end;
  {ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind('0.0.0.0',IntToStr(FPort));
  WriteLn('Listening connections at port ', FPort);
  ListenerSocket.listen;

  repeat
    if ListenerSocket.canread(1000) then
    begin
      ConnectionSocket.Socket := ListenerSocket.accept;
      WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
      AttendConnection(ConnectionSocket);
      ConnectionSocket.CloseSocket;
    end;
  until false;

  ListenerSocket.Free;
  ConnectionSocket.Free;}
end;

end.

