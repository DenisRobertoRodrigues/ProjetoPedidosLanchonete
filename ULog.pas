unit ULog;

interface

uses
  System.SysUtils, System.IOUtils, System.Classes, System.Threading;

type
  TLog = class
  private
    CaminhoAplicacao: String;
    UserLog: String;
    CaminhoArqLogUser: String;
    ArqLog: TextFile;
  public
    procedure SetaCaminho(const ACaminho: String);
    procedure SetaUserLog(const AUser: String);
    procedure SetaCaminhoArqLogUser(const ACaminho: String);
    procedure VerificaDirLog;
    procedure VerificaArqLog;
    procedure RegistraLog(const ALog: String; AAssincrono: boolean); overload;
    procedure RegistraLog(const ALog: String); overload;
    function RegistraLogEx(const ALog: String): String;
    procedure GravaLogArq(const ALog: String);
  end;

var
  AppLog: TLog;

implementation

procedure TLog.GravaLogArq(const ALog: String);
begin
  AssignFile(ArqLog, CaminhoArqLogUser );
  Append(ArqLog);
  Writeln(ArqLog, Format('%s - %s',[FormatDateTime('dd/mm/yyyy - hh:MM:ss', now), ALog]));
  CloseFile(ArqLog);
end;

procedure TLog.RegistraLog(const ALog: String; AAssincrono: boolean);
var
 aTask: ITask;
begin
 aTask := TTask.Create(
   procedure
   begin
     GravaLogArq(ALog);
   end);
 aTask.Start;
end;

procedure TLog.RegistraLog(const ALog: String);
begin
  GravaLogArq(ALog);
end;

function TLog.RegistraLogEx(const ALog: String): String;
begin
  GravaLogArq(ALog);
  Result := ALog;
end;

procedure TLog.SetaCaminho(const ACaminho: String);
begin
  CaminhoAplicacao := ACaminho;
  VerificaDirLog;
end;

procedure TLog.SetaCaminhoArqLogUser(const ACaminho: String);
begin
  CaminhoArqLogUser := ACaminho;
end;

procedure TLog.SetaUserLog(const AUser: String);
begin
  UserLog := AUser;
  VerificaArqLog;
end;

procedure TLog.VerificaArqLog;
var
  LDia, LMes, LAno: Word;
  LDir: String;
begin
  DecodeDate(Date, LAno, LMes, LDia);
  LDir := CaminhoAplicacao+'\'+ 'logs'+'\'+IntToStr(LAno)+'\'+IntToStr(LMes)+'\'+IntToStr(LDia);
  SetaCaminhoArqLogUser( TPath.Combine(LDir, UserLog+'.log') );
  if not FileExists( CaminhoArqLogUser ) then
  begin
    AssignFile(ArqLog, CaminhoArqLogUser );
    Rewrite(ArqLog);
    Append(ArqLog);
    Writeln(ArqLog ,'');
    CloseFile(ArqLog);
  end;

end;

procedure TLog.VerificaDirLog;
var
  LDia, LMes, LAno: Word;
  LDir: String;
begin
  DecodeDate(Date, LAno, LMes, LDia);
  LDir := CaminhoAplicacao+'\'+ 'logs'+'\'+IntToStr(LAno)+'\'+IntToStr(LMes)+'\'+IntToStr(LDia);
  if not DirectoryExists(LDir)then begin
    if not CreateDir(LDir) then begin
      ForceDirectories(LDir);
    end;
  end;
end;

end.
