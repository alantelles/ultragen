ADictFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDictionaryInstance', True);
ADictType := TDataType.Create('TDictionaryInstance', 'Dict');
ADictType.PMembers.Add( 'set', ADictFunc);
ADictType.PMembers.Add( 'get', ADictFunc);
ADictType.PMembers.Add( 'keys', ADictFunc);
ADictType.PMembers.Add( 'drop', ADictFunc);
ADictType.PMembers.Add( 'hasKey', ADictFunc);
ADictType.PMembers.Add( 'addLock', ADictFunc);
ADictType.PMembers.Add( 'changeLock', ADictFunc);
ADictType.PMembers.Add( 'lock', ADictFunc);
AActRec.AddMember('Dict', ADictType);
