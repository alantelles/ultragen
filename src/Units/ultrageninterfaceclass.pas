unit UltragenInterfaceClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils,
      ASTClass, LexerClass, ImpParserClass, InterpreterClass,
      StrUtils, LoggingClass, httpdefs, httpprotocol;

type
  TUltraInterface = class
    public
      class function ParseString(AStringCode:string): TAST;
      class function ParseStringList(var AList: TStringList): TAST;
      class function ParseWebRequest(var ARequest: TRequest): TAST;
      class function InterpretScript(AFilePath: string; APreludes: TProgram = nil): string;
	end;

implementation
uses Dos;

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

class function TUltraInterface.InterpretScript(AFilePath: string; APreludes: TProgram = nil): string;
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
	AInter.Interpret;
	LiveOut := AInter.PLive;
	AInter.Free;
  Result := LiveOut;
end;

class function TUltraInterface.ParseStringList(var AList: TStringList): TAST;
begin
  AList.LineBreak := #10;
  Result := ParseString(AList.Text);
end;

class function TUltraInterface.ParseWebRequest(var ARequest: TRequest): TAST;
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
	if Trim(UHome) <> '' then
	begin
	  WebVars.Add('addModulePath("'+UHome + DirectorySeparator + 'modules' +'")');
	  WebVars.Add('include @Core');
	end;
  if FileExists('./_INCLUDE.ultra') then
	  WebVars.Add('include "./_INCLUDE.ultra"');
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

  WebVars.Add('"content_type": "'+ARequest.ContentType+'", ');

  WebVars.Add('"body_content": "'+ReplaceStr(ARequest.Content, '"', '\"')+'", ');

  writeln(ARequest.ContentType);

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

  WebVars.Add('"host": "'+ARequest.Host+'", ');
  WebVars.Add('');


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

