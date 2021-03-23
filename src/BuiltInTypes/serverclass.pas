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
      FExceptionHandler: string;
      FStopRoute: string;
      FDebug: boolean;
      FRegisteredMimeTypes: TStringList;
    public
      property PStopRoute: string read FStopRoute write FStopRoute;
      property PRootFile: string read FRootFile write FRootFile;
      property PExceptionHandler: string read FExceptionHandler write FExceptionHandler;
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
      constructor Create(APort: integer; ADebug: boolean);
  end;


procedure ShowException(AResponse: TResponse; AException: Exception; var Switch: boolean);

implementation

uses
  ASTClass, TokenClass, Tokens, LexerClass, ImpParserClass, InterpreterClass, StrUtils,
  StringInstanceClass, Dos, UltraGenInterfaceClass, ResponseHandlerClass;

constructor TServerInstance.Create(APort: integer; ADebug: boolean);
begin
  inherited Create;
  MimeTypesFile := GetEnv('ULTRAGEN_HOME') + DirectorySeparator + 'assets' + DirectorySeparator + 'mime-types.txt';
  FMembers.Add('title', TStringInstance.Create('Untitled application'));
  FPort := Aport;
  FRootFile := 'index.ultra';
  FExceptionHandler := 'exception.ultra';
  FDebug := ADebug;
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
  FRegisteredMimeTypes.Free;
  WriteLn('Stopping server');
  Application.Terminate;

  WriteLn('Server stopped');
end;

procedure ShowException(AResponse: TResponse; AException: Exception; var Switch: boolean);
begin

end;

procedure TServerInstance.ExecuteAction(ARequest: TRequest; AResponse: TResponse);
var
  BTree: TAST;
  InsertActRec: TActivationRecord;
  ResponseContent: string = '';
  //AStream: TStringStream;
begin

  try
  begin

    BTree := TUltraInterface.ParseWebRequest(ARequest, AResponse, '');
    //InsertActRec := TActivationrecord.Create('HTTPRESPONSE', 'ANY', 1);
    //InsertActRec.AddMember('response', TResponseHandlerInstance.Create(AResponse));
    ResponseContent := TUltraInterface.InterpretScript(FRootFile, TProgram(BTree), nil, '', AResponse, ARequest, FRegisteredMimeTypes);
    //AStream := TSTringStream.Create(ResponseContent);
    if AResponse.ContentStream = nil then
      AResponse.ContentStream :=  TSTringStream.Create(ResponseContent);
    WriteLn(#13+'['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
      ARequest.Method + ': '+
      ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
      ' ' + AResponse.CodeText +
      ', ' + IntToStr(AResponse.ContentLength) + ' B, Content-Type: ' + AResponse.ContentType, #13);
    AResponse.SendContent;
    AResponse.ContentStream.Free;
  end;
  except on E: Exception do
    begin

       AResponse.Code := 500;
       AResponse.CodeText := 'Internal server error';
       WriteLn('['+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+'] ' +
          ARequest.Method + ': '+
          ARequest.URI+' -- '+ IntToStr(AResponse.Code)+
          ' ' + AResponse.CodeText +
          ', ' + IntToStr(AResponse.ContentLength) + ' B', #13);
       WriteLn(E.Message);

       if FDebug then

         AResponse.Content := '<h1>UltraGen ERROR!</h1><pre style="white-space: pre-wrap; font-size: 12pt"><h3>Error while fetching content at "' + ARequest.URI + '"</h3><br>'+ReplaceStr(E.Message, '<', '&lt') +'</pre>'
       else
       begin
         // InsertActRec := TActivationrecord.Create('HTTPRESPONSE', 'ANY', 1);
         // InsertActRec.AddMember('response', TResponseHandlerInstance.Create(AResponse));
         BTree := TUltraInterface.ParseWebRequest(ARequest, AResponse, E.Message);
         try
           ResponseContent := TUltraInterface.InterpretScript(FExceptionHandler, TProgram(BTree), nil, '', AResponse, ARequest, FRegisteredMimeTypes);
         except on F: Exception do
           WriteLn('While running application exception handler, another exception occurred:', #13, #10, #13, #10, F.Message);
         end;
         if ResponseContent = '' then
           ResponseContent := IntToStr(AResponse.Code) + ' ' + AResponse.CodeText;
         AResponse.Content := ResponseContent;
       end;
    end;
  end;
end;


procedure TServerInstance.RunServer;
begin

    FRegisteredMimeTypes := TStringList.Create;
    FRegisteredMimeTypes.LoadFromFile(MimeTypesFile);

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

end.

