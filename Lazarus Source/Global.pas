unit Global;

{$MODE objfpc}{$H+}

interface

uses Registry;

procedure OpenReg(key: String);
function DeleteKey(key: String): Boolean;
function GetRegValS(V: String;D: String): String;
procedure GetRegValA(V: String;var D: array of Byte);
function GetRegValI(V: String;D: Cardinal): Cardinal;
function GetRegValB(V: String;D: Boolean): Boolean;
procedure SetRegValS(V: String;D: String);
procedure SetRegValA(V: String;var D: array of Byte);
procedure SetRegValI(V: String;D: Cardinal);
procedure SetRegValB(V: String;D: Boolean);
function ExtractKey(var V: String):String;
var
 SpriteReg : TRegistry;
const
 //Registry Key to use
 RegKey = '\Software\GJH Software\Sprite Converter';

implementation

{-------------------------------------------------------------------------------
Open the registry key
-------------------------------------------------------------------------------}
procedure OpenReg(key: String);
begin
 SpriteReg:=TRegistry.Create;
 if key<>'' then key:='\'+key;
 SpriteReg.OpenKey(RegKey+key,true);
end;

{-------------------------------------------------------------------------------
Function to delete a key from the registry
-------------------------------------------------------------------------------}
function DeleteKey(key: String): Boolean;
var
 x: Boolean;
begin
 x:=True;
 OpenReg(ExtractKey(key));
 if SpriteReg.ValueExists(key) then x:=SpriteReg.DeleteValue(key);
 SpriteReg.Free;
 Result:=x;
end;

{-------------------------------------------------------------------------------
Function to read a string from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValS(V: String;D: String): String;
var
 X: String;
begin
 OpenReg(ExtractKey(V));
 If SpriteReg.ValueExists(V)then X:=SpriteReg.ReadString(V)
 else begin X:=D;SpriteReg.WriteString(V,X);end;
 SpriteReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read an array from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
procedure GetRegValA(V: String;var D: array of Byte);
var
 s: Integer;
begin
 OpenReg(ExtractKey(V));
 If SpriteReg.ValueExists(V)then
 begin
  s:=SpriteReg.GetDataSize(V);
  SpriteReg.ReadBinaryData(V,D,s);
 end
 else
 begin
  SpriteReg.WriteBinaryData(V,D,SizeOf(D));
 end;
 SpriteReg.Free;
end;

{-------------------------------------------------------------------------------
Function to read an integer from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValI(V: String;D: Cardinal): Cardinal;
var
 X: Cardinal;
begin
 OpenReg(ExtractKey(V));
 If SpriteReg.ValueExists(V)then X:=SpriteReg.ReadInteger(V)
 else begin X:=D;SpriteReg.WriteInteger(V,X);end;
 SpriteReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read a boolean from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValB(V: String;D: Boolean): Boolean;
var
 X: Boolean;
begin
 OpenReg(ExtractKey(V));
 If SpriteReg.ValueExists(V)then X:=SpriteReg.ReadBool(V)
 else begin X:=D;SpriteReg.WriteBool(V,X);end;
 SpriteReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to save a string to the registry
-------------------------------------------------------------------------------}
procedure SetRegValS(V: String;D: String);
begin
 OpenReg(ExtractKey(V));
 SpriteReg.WriteString(V,D);
 SpriteReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save an array to the registry
-------------------------------------------------------------------------------}
procedure SetRegValA(V: String;var D: array of Byte);
begin
 OpenReg(ExtractKey(V));
 SpriteReg.WriteBinaryData(V,D,SizeOf(D));
 SpriteReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save an integer to the registry
-------------------------------------------------------------------------------}
procedure SetRegValI(V: String;D: Cardinal);
begin
 OpenReg(ExtractKey(V));
 SpriteReg.WriteInteger(V,D);
 SpriteReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save a boolean to the registry
-------------------------------------------------------------------------------}
procedure SetRegValB(V: String;D: Boolean);
begin
 OpenReg(ExtractKey(V));
 SpriteReg.WriteBool(V,D);
 SpriteReg.Free;
end;

{-------------------------------------------------------------------------------
Function to extract key part of string
-------------------------------------------------------------------------------}
function ExtractKey(var V: String):String;
begin
 Result:='';
 if Pos('\',V)>0 then
 begin
  Result:=Copy(V,1,Pos('\',V)-1);
  V:=Copy(V,Pos('\',V)+1);
 end;
end;

end.
