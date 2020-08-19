AOSType := TDataType.Create('TOSInstance', 'OS');
AOSFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance', True);
AOSType.PMembers.Add('getEnv', AOSFunc);
AActRec.AddMember('OS', AOSType);
