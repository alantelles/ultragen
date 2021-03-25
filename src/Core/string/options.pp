else if AType = 'TStringInstance' then
begin
      if FName = 'split' then
		    Ret := SplitString(TStringInstance(FObj))
      else if FName = 'init' then
        InitString(TStringInstance(FObj))
		  else if FName = 'upper' then
		    Ret := TStringInstance.Create(UTF8UpperCase(TStringInstance(FObj).PValue))
		  else if FName = 'lower' then
		    Ret := TStringInstance.Create(UTF8LowerCase(TStringInstance(FObj).PValue))
		  else if FName = 'join' then
		    Ret := JoinString(TStringInstance(FObj))
		  else if FName = 'capital' then
		    Ret := CapitalString(TStringInstance(FObj))
      else if FName = 'slice' then
        Ret := SliceString (TStringInstance(FObj))
      else if FName = 'save' then
        SaveStringStream(TStringInstance(FObj))
      else if FName = 'pos' then
        Ret := TIntegerInstance.Create(UTF8Pos(
            TStringInstance(FParams[0]).PValue,
            TStringInstance(FObj).PValue
        ) - 1)

		  else if FName = 'replace' then
		    Ret := TStringInstance.Create(
		      ReplaceStr(
		        TStringInstance(FObj).PValue,
		        TStringInstance(FParams[0]).PValue,
		        TStringInstance(FParams[1]).PValue
		        )
		    )
		  else if FName = 'length' then
		    Ret := TIntegerInstance.Create(UTF8Length(TStringInstance(FObj).PValue))
      else if FName = 'fileName' then
        Ret := GetFileName(TStringInstance(FObj))
      else if FName = 'indexOf' then
        Ret := TIntegerInstance.Create(UTF8Pos(TStringInstance(FParams[0]).PValue, TStringInstance(FObj).PValue) - 1)
      else if FName = 'isInt' then
      begin
        try
        begin
          StrToInt(TStringInstance(FObj).PValue);
          Ret := TBooleanInstance.Create(True)

        end;
        except
          Ret := TBooleanInstance.Create(False);
        end;
      end
      else
        raise ERunTimeError.Create('Referenced function "' + FName + '" does not exist.');
end
