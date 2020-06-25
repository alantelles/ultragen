else if AType = 'TStringInstance' then
begin
      if FName = 'split' then
		    Ret := SplitString(TStringInstance(FObj))
		  else if FName = 'upper' then
		    Ret := TStringInstance.Create(UTF8UpperCase(TStringInstance(FObj).PValue))
		  else if FName = 'lower' then
		    Ret := TStringInstance.Create(UTF8LowerCase(TStringInstance(FObj).PValue))
		  else if FName = 'join' then
		    Ret := JoinString(TStringInstance(FObj))
		  else if FName = 'capital' then
		    Ret := CapitalString(TStringInstance(FObj))
		  else if FName = 'replace' then
		    Ret := TStringInstance.Create(
		      ReplaceStr(
		        TStringInstance(FObj).PValue,
		        TStringInstance(FParams[0]).PValue,
		        TStringInstance(FParams[1]).PValue
		        )
		    )
		  else if FName = 'length' then
		    Ret := TIntegerInstance.Create(UTF8Length(TStringInstance(FObj).PValue));
end
