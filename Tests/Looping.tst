code Basic For Loop  var tot%:=0  var x%  for x:=1 to 10 do    tot:=tot+xendcodevarvalue tot 55code With block  var tot%:=0  var x%  for x:=1 to 10 do  begin    tot:=tot+x  endendcodecompile noerrorvarvalue tot 55code Empty statement  var tot%:=0  var x%  for x:=1 to 10 do    ;endcodecompile noerrorcode Inline  var tot%:=0  var x%  for x:=1 to 10 do tot:=tot+xendcodecompile noerrorvarvalue tot 55code Inline  var tot%:=0  var x%  for x:=1 to 10 do tot:=tot+xendcodecompile noerrorvarvalue tot 55code Optional DO  var tot%:=0  var x%  for x:=1 to 10    tot:=tot+xendcodecompile noerrorvarvalue tot 55code Optional DO Block  var tot%:=0  var x%  for x:=1 to 10  begin    tot:=tot+x  endendcodecompile noerrorvarvalue tot 55code Optional DO empty statement  var tot%:=0  var x%  for x:=1 to 10    ;endcodecompile noerror;============VAR DECLARATIONScode Inline loop var decl  var tot%:=0  for var x%:=1 to 10 do    tot:=tot+xendcodecompile noerrorvarvalue tot 55code Inline loop var is local  var tot%:=0  for var x%:=1 to 10 do    tot:=tot+x  x:=x+1endcodecompile errorvarvalue tot 55code VAR block scope  var tot%:=0  for x%:=1 to 10 do    var y:Integer=100  y:=y+1endcodecompile errorcode VAR block scope  var tot%:=0  for x%:=1 to 10 do    var y:Integer=100  y:=y+1endcodecompile errorcode VAR block scope  var tot%:=0  for x%:=1 to 10 do  begin    var y:Integer=100  end  y:=y+1endcodecompile error;===================LINE BREAKScode Break FOR  var tot%:=0  for     var x%:=1 to 10 do    tot := tot+xendcodecompile noerrorcode Break VAR  var tot%:=0  for var     x%:=1 to 10 do    tot := tot+xendcodecompile noerrorcode Break loopvar  var tot%:=0  for var x%    :=1 to 10 do    tot := tot+xendcodecompile errorcode Break assignment  var tot%:=0  for var x%:=    1 to 10 do    tot := tot+xendcodecompile noerrorcode Break initial  var tot%:=0  for var x%:=1     to 10 do    tot := tot+xendcodecompile errorcode Break DO  var tot%:=0  for var x%:=1 to     10 do    tot := tot+xendcodecompile noerrorcode Break final  var tot%:=0  for var x%:=1 to 10     do    tot := tot+xendcodecompile errorcode Break typed before  var tot%:=0  for var x:    Byte = 1 to 10 do    tot := tot+xendcodecompile noerrorcode Break typed after type  var tot%:=0  for var x:Byte     = 1 to 10 do    tot := tot+xendcodecompile errorcode Break typed after assign  var tot%:=0  for var x:Byte =    1 to 10 do    tot := tot+xendcodecompile noerror