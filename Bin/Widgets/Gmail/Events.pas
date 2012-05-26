function OnBeforeNavigate(url: String): Boolean;
begin
	Result:= false;
end;

function OnNewWindow(url: String): Boolean;
begin
	Result:= true;
end;

function OnDocumentComplete(url: String): Boolean;
begin
	Result:= true;
end;

begin

end.