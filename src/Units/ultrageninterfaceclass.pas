unit UltragenInterfaceClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils,
      ASTClass, LexerClass, ImpParserClass, InterpreterClass,
      StrUtils, LoggingClass, httpdefs, httpprotocol, ARCLass;

type
  TUltraInterface = class
    public
      class function ParseString(AStringCode:string): TAST;
      class function ParseStringList(var AList: TStringList): TAST;
      class function ParseWebRequest(var ARequest: TRequest; var AResponse: TResponse; TraceLog: string): TAST;
      class function InterpretScript(AFilePath: string; APreludes: TProgram; InsertActRec: TActivationRecord; InsertName: string; AResponse: TResponse; ARequest: TRequest; AMimeFile: TStringList): string;
    end;

implementation
uses Dos, ResponseHandlerClass, StringInstanceClass;

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

class function TUltraInterface.InterpretScript(AFilePath: string; APreludes: TProgram; InsertActRec: TActivationRecord; InsertName:string; AResponse: TResponse; ARequest: TRequest; AMimeFile: TStringList): string;
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
  AInter.PResponse := AResponse;
  AInter.PRequest := ARequest;
  Ainter.PMimeFile := AMimeFile;
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
  comma, K, V, UHome: string;
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


  WebVars.Add('"body_content": """'+ReplaceStr(ARequest.Content, '"', '\"')+'""", ');

  if pos(ARequest.ContentType, 'application/x-www-form-urlencoded') > 0 then
  begin
    WebVars.Add('"body": {');
    len := ARequest.ContentFields.Count;
    if len > 0 then
    begin
      comma := ', ';
      for i:=0 to len - 1 do
      begin
        ARequest.ContentFields.GetNameValue(i, K, V);
        if i = len - 1 then
          comma := '';
        WebVars.Add('"'+K+'": "'+V+'"'+comma);
      end;
    end;
	  WebVars.Add('}, ');
	end
  else if pos(ARequest.ContentType, 'application/json') > 0 then
  begin
    WebVars.Add('"body": JSON.parse(''' + ARequest.Content + '''), ');
  end;



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
      WebVars.Add('"accept": "' + ARequest.Accept + '", ');
    if ARequest.AcceptCharset <> '' then
      WebVars.Add('"accept_charset": "' + ARequest.AcceptCharset + '", ');
    if ARequest.AcceptEncoding <> '' then
      WebVars.Add('"accept_encoding": "' + ARequest.AcceptEncoding + '", ');
    if ARequest.AcceptLanguage <> '' then
      WebVars.Add('"accept_language": "' + ARequest.AcceptLanguage + '", ');
    if ARequest.Authorization <> '' then
      WebVars.Add('"authorization": "' + ARequest.Authorization + '", ');
    if ARequest.Connection <> '' then
      WebVars.Add('"connection": "' + ARequest.Connection + '", ');
    if ARequest.ContentEncoding <> '' then
      WebVars.Add('"content_encoding": "' + ARequest.ContentEncoding + '", ');
    if ARequest.ContentLanguage <> '' then
      WebVars.Add('"content_language": "' + ARequest.ContentLanguage + '", ');
    if ARequest.ContentLength <> 0 then
      WebVars.Add('"content_length": "' + IntToStr(ARequest.ContentLength) + '", ');
    if ARequest.ContentType <> '' then
      WebVars.Add('"content_type": "' + ARequest.ContentType + '", ');
    if ARequest.Cookie <> '' then
      WebVars.Add('"cookie": "' + ARequest.Cookie + '", ');
    if ARequest.Date <> '' then
      WebVars.Add('"date": "' + ARequest.Date + '", ');
    if ARequest.Expires <> '' then
      WebVars.Add('"expires": "' + ARequest.Expires + '", ');
    if ARequest.From <> '' then
      WebVars.Add('"from": "' + ARequest.From + '", ');
    if ARequest.IfModifiedSince <> '' then
      WebVars.Add('"if_modified_since": "' + ARequest.IfModifiedSince + '", ');
    if ARequest.LastModified <> '' then
      WebVars.Add('"last_modified": "' + ARequest.LastModified + '", ');
    if ARequest.Location <> '' then
      WebVars.Add('"location": "' + ARequest.Location + '", ');
    if ARequest.Pragma <> '' then
      WebVars.Add('"pragma": "' + ARequest.Pragma + '", ');
    if ARequest.Referer <> '' then
      WebVars.Add('"referer": "' + ARequest.Referer + '", ');
    if ARequest.RetryAfter <> '' then
      WebVars.Add('"retry_after": "' + ARequest.RetryAfter + '", ');
    if ARequest.Server <> '' then
      WebVars.Add('"server": "' + ARequest.Server + '", ');
    if ARequest.SetCookie <> '' then
      WebVars.Add('"set_cookie": "' + ARequest.SetCookie + '", ');
    if ARequest.UserAgent <> '' then
      WebVars.Add('"user_agent": "' + ARequest.UserAgent + '", ');
    if ARequest.WWWAuthenticate <> '' then
      WebVars.Add('"www_authenticate": "' + ARequest.WWWAuthenticate + '", ');


    for V in ARequest.CustomHeaders do
    begin
      i := Pos('=', V);
      K := Copy(V, 1, i-1);
      WebVars.Add('"' + Replacestr(K, '-', '_') + '": """' + ARequest.CustomHeaders.Values[K] + '""", ');
    end;
    WebVars.Add('"host": "'+ARequest.Host+'"');
  WebVars.Add('} ');

  WebVars.Add('}');
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

