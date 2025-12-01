unit UDMDTO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, System.IniFiles,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet;

type
  TDMDTO = class(TDataModule)
    FDConexao: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDQuery1: TFDQuery;
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    procedure carregarConfBD(const AArqConf: String);
    function ExecutarSQL(const ASQL: string): Boolean;
  end;

  TFDConexaoHelper = class helper for TFDConnection
  public
    function ExecutarSQL(const ASQL: String; ALogAssincrono: boolean = False): Boolean;
  end;

var
  DMDTO: TDMDTO;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses ULog;

{$R *.dfm}

{ TDMDTO }

procedure TDMDTO.carregarConfBD(const AArqConf: String);
var
  Ini: TIniFile;
begin

  Ini := TIniFile.Create(AArqConf);
  try
    with FDConexao do
    begin
      DriverName := 'SQLite';
      Params.Clear;
      Params.Add('DriverID=SQLite');
      Params.Add('Database=' + Ini.ReadString('SQLITE', 'Database', ''));
      Params.Add('VendorLib=' + Ini.ReadString('SQLITE', 'VendorLib', ''));
      Params.Add('LockingMode=Normal');
      Params.Add('Synchronous=Normal');
      Params.Add('JournalMode=WAL');
      Params.Add('ForeignKeys=On');
      Params.Add('CacheSize=10000');
      LoginPrompt := False;
    end;
  finally
    Ini.Free;
  end;
end;



procedure TDMDTO.DataModuleDestroy(Sender: TObject);
begin
  AppLog.SetaUserLog('Sys');
  AppLog.RegistraLog('### Aplicação encerrada ###', True);
end;

function TDMDTO.ExecutarSQL(const ASQL: string): Boolean;
begin
  Result := False;
  try
    FDConexao.ExecSQL(ASQL);
  except
    on E: Exception do
    begin
      AppLog.RegistraLog(E.Message);
      Result := False;
    end;
  end;
end;

{ TFDConexaoHelper }

function TFDConexaoHelper.ExecutarSQL(const ASQL: String; ALogAssincrono: boolean = False): Boolean;
begin
  Result := False;
  try
    ExecSQL(ASQL);
  except
    on E: Exception do
    begin
      if ALogAssincrono then
        AppLog.RegistraLog(Format('Erro ao executar comando SQL: %s - %s',[ASQL, E.Message]), ALogAssincrono)
      else AppLog.RegistraLog(Format('Erro ao executar comando SQL: %s - %s',[ASQL, E.Message]));
      Result := False;
    end;
  end;
end;

end.
