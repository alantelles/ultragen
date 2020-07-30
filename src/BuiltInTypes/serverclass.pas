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
  StringInstanceClass;

constructor TServerInstance.Create(APort: integer);
begin
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

{procedure TServerInstance.AttendConnection(ASocket: TTCPBlockSocket);
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
end;}

procedure TServerInstance.ExecuteAction(ARequest: TRequest;AResponse: TResponse);
var
  AParser: TTParser;
  ALexer, BLexer: TLexer;
  ATree, BTree: TAST;
  AInter: TInterpreter;
  Output, comma, K, V: string;
  WebVars:TStringList;
  len, i: integer;
begin
  WebVars := TStringList.Create;
  WebVars.SkipLastLineBreak := True;
  WebVars.Add('$_request = {');
  Webvars.Add('"_route": "'+ARequest.URI+'", ');
  Webvars.Add('"_method": "'+ARequest.Method+'", ');
  Webvars.Add('"_querystring": "'+ARequest.QueryString+'", ');
  WebVars.Add('"_query": {');
  len := ARequest.QueryFields.Count;
  if len > 0 then
  begin
    comma := ', ';
    for i:=0 to len - 1 do
    begin
      ARequest.QueryFields.GetNameValue(i, K, V);
      if i = len - 1 then
        comma := '';
      WebVars.Add('"'+K+'": "'+V+'"'+comma);
    end;
  end;
  WebVars.Add('}, ');
  WebVars.Add('"_content_type": "'+ARequest.ContentType+'", ');
  WebVars.Add('"_body_content": "'+ReplaceStr(ARequest.Content, '"', '\"')+'", ');
  WebVars.Add('"_body": {');
  len := ARequest.ContentFields.Count;
  if len > 0 then
  begin
    comma := ', ';
    for i:=0 to len - 1 do
    begin
      ARequest.ContentFields.GetNameValue(i, K, V);
      writeln(k, ': ', v);
      if i = len - 1 then
        comma := '';
      WebVars.Add('"'+K+'": "'+V+'"'+comma);
    end;
  end;
  WebVars.Add('}');
  WebVars.Add('}');
  WebVars.Add('$_request.lock()');
  BLexer := TLexer.Create(WebVars.Text, False);
  AParser := TTParser.Create(BLexer);
  BTree := AParser.ParseCode();
  AParser.Free;

  ALexer := TLexer.Create(FRootFile);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();

  len := Length(TProgram(BTree).PChildren);
  if len > 0 then
  begin
    for i:=0 to len-1 do
      TProgram(ATree).AddPrelude(TProgram(BTree).PChildren[i]);
  end;
  AInter := TInterpreter.Create(ATree);
  try
    AInter.Interpret;
    Output := AInter.PLive;
    //AInter.FreeInstances;
    AInter.Free;
    AParser.Free;
    AResponse.Content := Output;
    WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
      ' ' + AResponse.CodeText +
      ', ' + IntToStr(AResponse.ContentLength) + ' B', #13);
  except
    on E: Exception do
    begin
       AInter.Free;
       AResponse.Code := 500;
       AResponse.CodeText := 'Internal server error';
       AResponse.Content := '<pre style="font-size: 12pt">'+ReplaceStr(E.Message, '<', '&lt') +'</pre>';
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

