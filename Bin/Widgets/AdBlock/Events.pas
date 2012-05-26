function OnBeforeNavigate(url: String): Boolean;
begin
	Result:= false;	
		
	if Pos('googleads.g.doubleclick.net', url) <> 0 then Result:= true;
	if Pos('google_ads_iframe_Homepage', url) <> 0 then Result:= true;
	
	if Pos('tas-rs.toboads.com/ads.php', url) <> 0 then Result:= true;
	if Pos('rs.search.etargetnet.com', url) <> 0 then Result:= true;
	if Pos('lakodoposla.com/ads.php', url) <> 0 then Result:= true;
	if Pos('ldpo_ads_frame', url) <> 0 then Result:= true;
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