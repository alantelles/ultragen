unit UltragenInterfaceClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils,
      ASTClass, LexerClass, ImpParserClass, InterpreterClass,
      StrUtils, LoggingClass, httpdefs, httpprotocol, ARCLass, UltraWebHandlersClass;

type
  TUltraResult = record
    LiveOutput: string;
    ActRec: TActivationRecord;
    Redirected: boolean;
  end;

  TUltraAdapter = class
    private
      FActRec: TActivationRecord;
    public
      property ActRec: TActivationRecord read FActRec;
      constructor Create(AName:string);
      function AddMember(AName: string; AValue:string):boolean;
      function AddMember(AName: string; AValue:integer):boolean;
  end;
  TUltraInterface = class
    public

      class function ParseString(AStringCode:string): TAST;
      class function ParseStringList(var AList: TStringList): TAST;
      class function ParseWebRequest(var ARequest: TRequest; var AResponse: TResponse; TraceLog: string): TAST;

      class function InterpretScript(AFilePath: string; APreludes: TProgram; InsertActRec: TActivationRecord; InsertName: string; AResponse: TResponse; ARequest: TRequest; AMimeFile: TStringList): string;
      class function InterpretScript(
        AFilePath: string;
        APreludes: TProgram;
        InsertActRec: TActivationRecord): string;
      class function InterpretScript(
        AFilePath: string;
        APreludeList: TStringList;
        AnAdapter: TUltraAdapter): string;
      class function InterpretScriptWithResult(
        AFilePath: string;
        APreludeList: TStringList;
        AnAdapter: TUltraAdapter;
        UltraHome: string;
        WebHandlers: TUltraWebHandlers = nil): TUltraResult;
    end;

implementation
uses Dos, ResponseHandlerClass, StringInstanceClass, InstanceOfClass;


constructor TUltraAdapter.Create(AName:string);
begin
  FActRec := TActivationRecord.Create(AName, 'EXTERNAL', 0);
end;

function TUltraAdapter.AddMember(AName: string; AValue: string):boolean;
begin
  Result := FActRec.AddMember(AName, TStringInstance.Create(AValue));
end;
function TUltraAdapter.AddMember(AName: string; AValue: integer):boolean;
begin
  Result := FActRec.AddMember(AName, TIntegerInstance.Create(AValue));
end;

class function TUltraInterface.ParseString(AStringCode: string): TAST;
var
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
begin
  ALexer := TLexer.Create(AStringCode, False);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  AParser.Free;
  Result := ATree;
end;

class function TUltraInterface.InterpretScriptWithResult(
  AFilePath: string;
  APreludeList: TStringList;
  AnAdapter: TUltraAdapter;
  UltraHome: string;
  WebHandlers: TUltraWebHandlers = nil): TUltraResult;
var
  len, i: integer;
  AParser: TTParser;
  ALexer: TLexer;
  APreludes: TProgram;
  ATree: TAST;
  AInter: TInterpreter;
  LiveOut: string;
begin
  APreludes := TProgram(TUltraInterface.ParseStringList(APreludeList));
  ALexer := TLexer.Create(AFilePath);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  if APreludes <> nil then
  begin
    len := Length(APreludes.PChildren);
    if len > 0 then
    begin
      for i:=0 to len-1 do
	TProgram(ATree).AddPrelude(APreludes.PChildren[i]);
    end;
  end;
  AParser.Free;
  AInter := TInterpreter.Create(ATree);
  AInter.PWebHandlers := WebHandlers;
  AInter.PUltraHome := UltraHome;
  AInter.Interpret(AnAdapter.ActRec, AnAdapter.ActRec.PName, True);
  LiveOut := AInter.PLive;
  Result.Redirected := AInter.PRedirected;
  AInter.Free;
  Result.LiveOutput := LiveOut;
  Result.ActRec := AInter.PCallStack.Peek;
end;



class function TUltraInterface.InterpretScript(
  AFilePath: string;
  APreludes: TProgram;
  InsertActRec: TActivationRecord): string;
begin
  Result :=  InterpretScript(
    AFilePath,
    APreludes,
    InsertActRec,
    InsertActRec.PName,
    nil,
    nil,
    nil);
end;

class function TUltraInterface.InterpretScript(
  AFilePath: string;
  APreludeList: TStringList;
  AnAdapter: TUltraAdapter):string;
var
  APreludes: TProgram;
begin

  APreludes := TProgram(TUltraInterface.ParseStringList(APreludeList));
  Result :=  InterpretScript(
    AFilePath,
    APreludes,
    AnAdapter.ActRec,
    AnAdapter.ActRec.PName,
    nil,
    nil,
    nil);
end;

class function TUltraInterface.InterpretScript(
  AFilePath: string;
  APreludes: TProgram;
  InsertActRec: TActivationRecord;
  InsertName:string; AResponse:
  TResponse;
  ARequest: TRequest;
  AMimeFile: TStringList): string;
var
  len, i: integer;
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  AInter: TInterpreter;
  LiveOut: string;
begin
  ALexer := TLexer.Create(AFilePath);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  if APreludes <> nil then
  begin
    len := Length(APreludes.PChildren);
    if len > 0 then
    begin
      for i:=0 to len-1 do
	TProgram(ATree).AddPrelude(APreludes.PChildren[i]);
    end;
  end;
  AParser.Free;
  AInter := TInterpreter.Create(ATree);
  if InsertActRec <> nil then
    AInter.Interpret(InsertActRec, InsertName)
  else
    AInter.Interpret();
  LiveOut := AInter.PLive;
  AInter.Free;
  Result := LiveOut;
end;

class function TUltraInterface.ParseStringList(var AList: TStringList): TAST;
begin
  AList.LineBreak := #10;
  Result := ParseString(AList.Text);
end;

class function TUltraInterface.ParseWebRequest(var ARequest: TRequest; var AResponse: TResponse; TraceLog: string): TAST;
var
  WebVars: TStringList;
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  len, i: integer;
  comma, K, V, UHome, AuxStr: string;
begin

  WebVars := TStringList.Create;

  UHome := GetEnv('ULTRAGEN_HOME');
  UHome := ReplaceStr(UHome, DirectorySeparator, '\' + DirectorySeparator);
	if Trim(UHome) <> '' then
	begin
	  WebVars.Add('addModulePath(["'+UHome + '", "modules"].path())');
	  WebVars.Add('include @Core');
	end;
  if FileExists('./_INCLUDE.ultra') then
	  WebVars.Add('include "./_INCLUDE.ultra"');

  WebVars.Add('$stacktrace = """'+TraceLog+'""".split("\n")');
  WebVars.Add('$request = {');
  i := Pos('?', ARequest.URI);
  if i > 0 then
    Webvars.Add('"route": "'+Copy(ARequest.URI, 1, i-1)+'", ')
  else
    Webvars.Add('"route": "'+ARequest.URI+'", ');

  Webvars.Add('"method": "'+ARequest.Method+'", ');

  Webvars.Add('"querystring": "'+ARequest.QueryString+'", ');

  WebVars.Add('"query": {');
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

  // WebVars.Add('"content_type": "'+ARequest.ContentType+'", ');


  WebVars.Add('"body_content": new ByteStream(["""'+ReplaceStr(ARequest.Content, '\', '\\')+'"""]), ');
  WebVars.Add('"cookies": {');
  len := ARequest.CookieFields.Count;
  if len > 0 then
  begin
    comma := ', ';
    for i:=0 to len - 1 do
    begin
      ARequest.CookieFields.GetNameValue(i, K, V);
      if i = len - 1 then
        comma := '';
      WebVars.Add('"'+K+'": "'+V+'"'+comma);
    end;
  end;
  WebVars.Add('}, ');

  // HTTP headers
  WebVars.Add('"headers": {');
    if ARequest.Accept <> '' then
      WebVars.Add('"Accept": "' + ARequest.Accept + '", ');
    if ARequest.AcceptCharset <> '' then
      WebVars.Add('"Accept-Charset": "' + ARequest.AcceptCharset + '", ');
    if ARequest.AcceptEncoding <> '' then
      WebVars.Add('"Accept-Encoding": "' + ARequest.AcceptEncoding + '", ');
    if ARequest.AcceptLanguage <> '' then
      WebVars.Add('"Accept-Language": "' + ARequest.AcceptLanguage + '", ');
    if ARequest.Authorization <> '' then
      WebVars.Add('"Authorization": "' + ARequest.Authorization + '", ');
    if ARequest.Connection <> '' then
      WebVars.Add('"Connection": "' + ARequest.Connection + '", ');
    if ARequest.ContentEncoding <> '' then
      WebVars.Add('"Content-Encoding": "' + ARequest.ContentEncoding + '", ');
    if ARequest.ContentLanguage <> '' then
      WebVars.Add('"Content-Language": "' + ARequest.ContentLanguage + '", ');
    if ARequest.ContentLength <> 0 then
      WebVars.Add('"Content-Length": "' + IntToStr(ARequest.ContentLength) + '", ');
    if ARequest.ContentType <> '' then
      WebVars.Add('"Content-Type": "' + ARequest.ContentType + '", ');
    if ARequest.Cookie <> '' then
      WebVars.Add('"Cookie": "' + ARequest.Cookie + '", ');
    if ARequest.Date <> '' then
      WebVars.Add('"Date": "' + ARequest.Date + '", ');
    if ARequest.Expires <> '' then
      WebVars.Add('"Expires": "' + ARequest.Expires + '", ');
    if ARequest.From <> '' then
      WebVars.Add('"From": "' + ARequest.From + '", ');
    if ARequest.IfModifiedSince <> '' then
      WebVars.Add('"If-Modified-Since": "' + ARequest.IfModifiedSince + '", ');
    if ARequest.LastModified <> '' then
      WebVars.Add('"Last-Modified": "' + ARequest.LastModified + '", ');
    if ARequest.Location <> '' then
      WebVars.Add('"Location": "' + ARequest.Location + '", ');
    if ARequest.Pragma <> '' then
      WebVars.Add('"Pragma": "' + ARequest.Pragma + '", ');
    if ARequest.Referer <> '' then
      WebVars.Add('"Referer": "' + ARequest.Referer + '", ');
    if ARequest.RetryAfter <> '' then
      WebVars.Add('"Retry-After": "' + ARequest.RetryAfter + '", ');
    if ARequest.Server <> '' then
      WebVars.Add('"Server": "' + ARequest.Server + '", ');
    if ARequest.SetCookie <> '' then
      WebVars.Add('"Set-Cookie": "' + ARequest.SetCookie + '", ');
    if ARequest.UserAgent <> '' then
      WebVars.Add('"User-Agent": "' + ARequest.UserAgent + '", ');
    if ARequest.WWWAuthenticate <> '' then
      WebVars.Add('"WWW-Authenticate": "' + ARequest.WWWAuthenticate + '", ');


    for V in ARequest.CustomHeaders do
    begin
      i := Pos('=', V);
      K := Copy(V, 1, i-1);
      WebVars.Add('"' + K + '": """' + replaceStr(ARequest.CustomHeaders.Values[K], '/', '//') + '""", ');
    end;
    WebVars.Add('"host": "'+ARequest.Host+'"');
  WebVars.Add('  } ');
  WebVars.Add('}');

  if (pos('application/x-www-form-urlencoded', ARequest.ContentType) > 0) or
     (pos('multipart/form-data', ARequest.ContentType) > 0) then
  begin
    WebVars.Add('$request["body"] = {}');
    len := ARequest.ContentFields.Count;
    if len > 0 then
    begin
      for i:=0 to len - 1 do
      begin
        ARequest.ContentFields.GetNameValue(i, K, V);
        WebVars.Add('if (($request[:body].hasKey("' +k+ '")))');
        WebVars.Add('    if (typeof($request[:body]["' + k + '"]) == "TStringInstance")');
        WebVars.Add('        $request[:body]["' + k + '"] = [$request[:body]["' + k + '"]]');
        Webvars.add('        $request[:body]["' + k + '"].append("""' + v + '""")');
        Webvars.add('    else');

        Webvars.add('        $request[:body]["' + k + '"].append("""' + v + '""")');
        Webvars.add('    end');
        Webvars.add('else');
        WebVars.Add('    $request[:body]["'+K+'"] = """'+V+'"""');
        Webvars.add('end');
      end;
    end;

	end
  else if pos(ARequest.ContentType, 'application/json') > 0 then
  begin
    WebVars.Add('$request["body"] = JSON.parse("""' + ARequest.Content + '""")');
  end;
  WebVars.Add('$request["files"] = {}');
  len := ARequest.Files.Count;
  if len > 0 then
  begin
    for i := 0 to len-1 do
    begin
      AuxStr := ARequest.Files[i].LocalFileName;
      {$IFDEF Windows}
        AuxStr := ReplaceStr(ARequest.Files[i].LocalFileName, '\', '\\');
      {$ENDIF}

      WebVars.Add('if (($request["files"].hasKey("' + ARequest.Files[i].FieldName + '")))');
      WebVars.Add('    if (typeof($request["files"]["' + ARequest.Files[i].FieldName + '"]) == "TDictionaryInstance")');
      WebVars.Add('        $request["files"]["' + ARequest.Files[i].FieldName + '"] = [$request["files"]["' + ARequest.Files[i].FieldName + '"]]');
      WebVars.Add('        $request["files"]["' + ARequest.Files[i].FieldName + '"].append({"serverName": """'+AuxStr+'""", "fileName": """'+ARequest.Files[i].FileName+'"""})');
      WebVars.Add('    else');
      WebVars.Add('        $request["files"]["' + ARequest.Files[i].FieldName + '"].append({"serverName": """'+AuxStr+'""", "fileName": """'+ARequest.Files[i].FileName+'"""})');
      WebVars.Add('    end');
      WebVars.Add('else');
      WebVars.Add('    $request["files"]["'+ARequest.Files[i].FieldName+'"] = {"serverName": """'+AuxStr+'""", "fileName": """'+ARequest.Files[i].FileName+'"""}');
      WebVars.Add('end');
    end;
  end;
  WebVars.Add('$request.lock()');
  ALexer := TLexer.Create(WebVars.Text, False);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  AParser.Free;
  WebVars.Free;
  Result := ATree;
end;

begin
  randomize;
end.

