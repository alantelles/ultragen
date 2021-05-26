unit TypeLoaderClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, InterpreterClass, ARClass, ByteStreamClass;

type
  TTypeLoader = class
  public
    class procedure LoadType(AName: string; var AInter: Tinterpreter;
      var AActRec: TActivationRecord);
    class procedure LoadRequest(var AActRec: TActivationRecord);
    class procedure LoadFileSystem(var AActRec: TActivationRecord);
    class procedure LoadOS(var AActRec: TActivationRecord);
    class procedure LoadDateTime(var AActRec: TActivationRecord);
    class procedure LoadResponseHandler(var AActRec: TActivationRecord);
    class procedure LoadBrookResponseHandler(var AActRec: TActivationRecord);
    class procedure LoadServerInstance(var AActRec: TActivationRecord);
    class procedure LoadByteStream(var AActRec: TActivationRecord);
    class procedure LoadMarkdownParser(var AActRec: TActivationrecord);
    class procedure LoadBrookserver(var AActRec: TActivationRecord);
    class procedure LoadHelpers(var AActRec: TActivationRecord);
    class procedure LoadUploaded(var AActRec: TActivationRecord);
  end;

implementation

class procedure TTypeLoader.LoadHelpers(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('THelpersInstance', 'Helpers');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'THelpersInstance',
    True, False, False);
  Atype.PMembers.Add('urlencode', AFunc);
  Atype.PMembers.Add('urldecode', AFunc);
  AActRec.AddMember('Helpers', AType);
end;

class procedure TTypeloader.LoadUploaded(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TUploadedInstance', 'Uploaded');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TUploadedInstance' ,
    True, False, False);
  AType.PMembers.Add('save', AFunc);
  AActRec.AddMember('Uploaded', AType);
end;

class procedure TTypeLoader.LoadBrookserver(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('TBrookServerInstance', 'Server');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBrookServerInstance',
    True, False, False);
  Atype.PMembers.Add('run', AFunc);
  AActrec.AddMember('Server', AType);
end;

class procedure TTypeLoader.LoadMarkdownParser(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('TMarkdownParserInstance', 'Markdown');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TMarkdownParserInstance',
    True, False, False);
  AType.PMembers.Add('parse', AFunc);
  AType.PMembers.Add('parseFile', AFunc);
  AActRec.AddMember('Markdown', AType);
end;

class procedure TTypeLoader.LoadOS(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TOSInstance', 'OS');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance',
    True, False, False);
  AType.PMembers.Add('getEnv', AFunc);
  AActRec.AddMember('OS', AType);
end;

class procedure TTypeLoader.LoadByteStream(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TByteStreamInstance', 'ByteStream');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TByteStreamInstance',
    True, False, False);
  AType.PMembers.Add('save', AFunc);
  AType.PMembers.Add('read', AFunc);
  AType.PMembers.Add('write', AFunc);
  AType.PMembers.Add('length', AFunc);
  AActRec.AddMember('ByteStream', AType);
end;

{class procedure TTypeLoader.LoadCookiesHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TCookiesHandlerInstance', 'Cookies');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TCookiesHandlerInstance', True, False, False);
  ADataType.PMembers.Add('set', AFunc);
  ADataType.PMembers.Add('get', AFunc);
  ADataType.PMembers.Add('unset', AFunc);
  AActRec.AddMember('Cookies', ADataType);
end;}

class procedure TTypeLoader.LoadBrookResponseHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TBrookResponseInstance', 'AppResponse');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBrookResponseInstance',
    True, False, False);
  AdataType.PMembers.Add('$headers', TDictionaryInstance.Create(
    TActivationRecord.Create('AppResponseHeaders', AR_DICT, -1)));
  AdataType.PMembers.Add('$cookies', TDictionaryInstance.Create(
    TActivationRecord.Create('AppResponseCookies', AR_DICT, -1)));
  ADataType.PMembers.Add('redirect', AFunc);
  ADataType.PMembers.Add('clientRedirect', AFunc);
  {ADataType.PMembers.Add('setStatusCode', AFunc);
  ADataType.PMembers.Add('setStatusText', AFunc);
  ADataType.PMembers.Add('setContentType', AFunc);
  ADataType.PMembers.Add('setHeader', AFunc);}
  ADataType.PMembers.Add('static', AFunc);
  AActRec.AddMember('AppResponse', ADataType);
end;

class procedure TTypeLoader.LoadResponseHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TAppResponseInstance', 'AppResponse');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TAppResponseInstance',
    True, False, False);
  AdataType.PMembers.Add('$headers', TDictionaryInstance.Create(
    TActivationRecord.Create('AppResponseHeaders', AR_DICT, -1)));
  AdataType.PMembers.Add('$cookies', TDictionaryInstance.Create(
    TActivationRecord.Create('AppResponseCookies', AR_DICT, -1)));
  ADataType.PMembers.Add('redirect', AFunc);
  ADataType.PMembers.Add('clientRedirect', AFunc);
  ADataType.PMembers.Add('static', AFunc);
  AActRec.AddMember('AppResponse', ADataType);
end;

class procedure TTypeLoader.LoadServerInstance(var AActRec: TActivationRecord);
var
  AServerType: TDataType;
  AServerFunc: TFunctionInstance;
begin
  AServerType := TDataType.Create('TServerInstance', 'Server');
  AServerFunc := TFunctionInstance.Create('BuiltIn', nil, nil,
    'TServerInstance', True, False, False);
  AServerType.PMembers.Add('run', AServerFunc);
  AServerType.PMembers.Add('setStaticPath', AServerFunc);
  AServerType.PMembers.Add('setStaticPaths', AServerFunc);
  AActRec.AddMember('Server', AServerType);
end;

class procedure TTypeLoader.LoadDateTime(var AActRec: TActivationRecord);
var
  ADateTimeFunc: TFunctionInstance;
  ADateTimeType: TDataType;
begin
  ADateTimeFunc := TFunctionInstance.Create('BuiltIn', nil, nil,
    'TDateTimeInstance', True, False, False);
  ADateTimeType := TDataType.Create('TDateTimeInstance', 'DateTime');
  ADateTimeType.PMembers.Add('now', ADateTimeFunc);
  ADateTimeType.PMembers.Add('year', ADateTimeFunc);
  ADateTimeType.PMembers.Add('month', ADateTimeFunc);
  ADateTimeType.PMembers.Add('day', ADateTimeFunc);
  ADateTimeType.PMembers.Add('hour', ADateTimeFunc);
  ADateTimeType.PMembers.Add('minute', ADateTimeFunc);
  ADateTimeType.PMembers.Add('second', ADateTimeFunc);
  ADateTimeType.PMembers.Add('milli', ADateTimeFunc);
  ADateTimeType.PMembers.Add('weekDay', ADateTimeFunc);
  ADateTimeType.PMembers.Add('weekDayAbbr', ADateTimeFunc);
  ADateTimeType.PMembers.Add('weekDayName', ADateTimeFunc);
  ADateTimeType.PMembers.Add('parse', ADateTimeFunc);
  ADateTimeType.PMembers.Add('format', ADateTimeFunc);
  ADateTimeType.PMembers.Add('unixTime', ADateTimeFunc);
  ADateTimeType.PMembers.Add('compare', ADateTimeFunc);
  ADateTimeType.PMembers.Add('compareTime', ADateTimeFunc);
  ADateTimeType.PMembers.Add('compareDate', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addDays', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addMonts', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addYears', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addHours', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addMinutes', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addSeconds', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addMillis', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addWeeks', ADateTimeFunc);
  ADateTimeType.PMembers.Add('isBetween', ADateTimeFunc);

  AActRec.AddMember('DateTime', ADateTimeType);
end;

class procedure TTypeLoader.LoadRequest(var AActRec: TActivationRecord);
var
  AHttpClientFunc: TFunctionInstance;
  AHttpClientType, AHttpResponseType: TDataType;
begin
  AHttpClientFunc := TFunctionInstance.Create('BuiltIn', nil, nil,
    'THttpClientInstance', True, False, False);
  AHttpClientType := TDataType.Create('THttpClientInstance', 'Request');
  AHttpClientType.PMembers.Add('get', AHttpClientFunc);
  AHttpClientType.PMembers.Add('post', AHttpClientFunc);
  AHttpClientType.PMembers.Add('put', AHttpClientFunc);
  AHttpClientType.PMembers.Add('delete', AHttpClientFunc);
  AHttpClientType.PMembers.Add('run', AHttpClientFunc);
  AActrec.AddMember('Request', AHttpClientType);

  AHttpResponseType := TDataType.Create('THttpResponseInstance', 'Response');
  AActrec.AddMember('Response', AHttpResponseType);
end;

class procedure TTypeLoader.LoadFileSystem(var AActRec: TActivationRecord);
var
  AFSType: TDataType;
  AFSFunc: TFunctionInstance;
begin
  AFSType := TDataType.Create('TFileSystemInstance', 'FileSystem');
  AFSFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance',
    True, False, False);
  AFSType.PMembers.Add('loadText', AFSFunc);
  AFSType.PMembers.Add('mkdir', AFSFunc);
  AFSType.PMembers.Add('isFile', AFSFunc);
  AFSType.PMembers.Add('isDir', AFSFunc);
  AFSType.PMembers.Add('getAllFiles', AFSFunc);
  AFSType.PMembers.Add('copy', AFSFunc);
  AFSType.PMembers.Add('delete', AFSFunc);
  AActRec.AddMember('FileSystem', AFSType);
end;

class procedure TTypeLoader.LoadType(AName: string; var AInter: Tinterpreter;
  var AActRec: TActivationRecord);
begin
  if AName = 'Request' then
    TTypeLoader.LoadRequest(AActRec)
  else if AName = 'FileSystem' then
    TTypeLoader.LoadFileSystem(AActRec)
  {else if AName = 'Cookies' then
    TTypeLoader.LoadCookiesHandler(AActRec)}
  else if AName = 'OS' then
    TTypeLoader.LoadOS(AActRec)
  else if AName = 'ByteStream' then
    TTypeLoader.LoadByteStream(AActRec)
  else if AName = 'Helpers' then
    TTypeLoader.LoadHelpers(AActRec)
  else if AName = 'DateTime' then
    TTypeLoader.LoadDateTime(AActRec)
  else if AName = 'Server' then
    TTypeLoader.LoadServerInstance(AActRec)
  else if AName = 'AppResponse' then
    TTypeLoader.LoadResponseHandler(AActRec)
  else if AName = 'BrookServer' then
    TTypeLoader.LoadBrookserver(AActRec)
  else if AName = 'BrookAppResponse' then
    TTypeLoader.LoadBrookResponseHandler(AActRec)
  else if AName = 'Markdown' then
    TTypeLoader.LoadMarkdownParser(AActRec)
  else if AName = 'Uploaded' then
    TTypeLoader.LoadUploaded(AActRec)
  else
    AInter.RaiseException('Type "' + AName + '" does not exist and can''t be loaded',
      'RunTime');
end;

end.
