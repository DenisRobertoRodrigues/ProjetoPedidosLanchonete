unit SQLiteInstaller;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Net.HttpClient,
  System.Net.URLClient, Winapi.Windows, Winapi.ShellAPI;

type
  TSQLiteInstaller = class
  private
    FInstallPath: string;
    FLogCallback: TProc<string>;
    procedure Log(const AMessage: string);
    function DownloadFile(const AURL, ADestFile: string): Boolean;
    function ExtractDLL(const AZipFile, ADestPath: string): Boolean;
  public
    constructor Create(const AInstallPath: string = '');
    function InstallSQLite: Boolean;
    function ConfigureSQLite: Boolean;
    function TestSQLiteConnection: Boolean;
    property OnLog: TProc<string> write FLogCallback;
  end;

implementation

uses
  System.Zip;

const
  SQLITE_DLL_URL = 'https://www.sqlite.org/2025/sqlite-dll-win-x86-3510000.zip';
  SQLITE_DLL_NAME = 'sqlite3.dll';

{ TSQLiteInstaller }

constructor TSQLiteInstaller.Create(const AInstallPath: string);
begin
  inherited Create;
  
  if AInstallPath <> '' then
    FInstallPath := AInstallPath
  else
    FInstallPath := TPath.Combine(TPath.GetPublicPath, 'SQLite');
    
  if not TDirectory.Exists(FInstallPath) then
    TDirectory.CreateDirectory(FInstallPath);
end;

procedure TSQLiteInstaller.Log(const AMessage: string);
begin
  if Assigned(FLogCallback) then
    FLogCallback(AMessage);
end;

function TSQLiteInstaller.DownloadFile(const AURL, ADestFile: string): Boolean;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
begin
  HttpClient := THTTPClient.Create;
  try
    try
      Log('Baixando SQLite DLL de: ' + AURL);
      
      FileStream := TFileStream.Create(ADestFile, fmCreate);
      try
        Response := HttpClient.Get(AURL, FileStream);
        Result := Response.StatusCode = 200;
        
        if Result then
          Log('Download concluído com sucesso')
        else
          Log('Erro no download. Status: ' + Response.StatusCode.ToString);
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
      begin
        Log('Erro ao baixar arquivo: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

function TSQLiteInstaller.ExtractDLL(const AZipFile, ADestPath: string): Boolean;
begin
  try
    Log('Extraindo arquivo ZIP...');
    TZipFile.ExtractZipFile(AZipFile, ADestPath);
    Result := TFile.Exists(TPath.Combine(ADestPath, SQLITE_DLL_NAME));
    
    if Result then
      Log('Extração concluída com sucesso')
    else
      Log('Erro: DLL não encontrada após extração');
  except
    on E: Exception do
    begin
      Log('Erro ao extrair ZIP: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSQLiteInstaller.InstallSQLite: Boolean;
var
  ZipFile: string;
  {DllSource, }DllDest: string;
begin
  Result := False;
  
  try
    Log('=== Iniciando Instalação do SQLite ===');
    
    // Verifica se já existe
    DllDest := TPath.Combine(FInstallPath, SQLITE_DLL_NAME);
    if TFile.Exists(DllDest) then
    begin
      Log('SQLite DLL já existe em: ' + DllDest);
      Result := True;
      Exit;
    end;
    
    // Download da DLL
    ZipFile := TPath.Combine(TPath.GetTempPath, 'sqlite.zip');
    
    if not DownloadFile(SQLITE_DLL_URL, ZipFile) then
    begin
      Log('ERRO: Não foi possível baixar o SQLite');
      Exit;
    end;
    
    // Extrai a DLL
    if not ExtractDLL(ZipFile, FInstallPath) then
    begin
      Log('ERRO: Não foi possível extrair a DLL');
      Exit;
    end;


    // Limpa arquivo temporário
    if TFile.Exists(ZipFile) then
      TFile.Delete(ZipFile);
    
    Log('=== Instalação Concluída com Sucesso ===');
    Result := True;
    
  except
    on E: Exception do
    begin
      Log('ERRO FATAL: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSQLiteInstaller.ConfigureSQLite: Boolean;
var
  ConfigFile: string;
  ConfigLines: TStringList;
begin

  try
    Log('=== Configurando SQLite ===');
    
    ConfigFile := TPath.Combine(FInstallPath, 'sqlite.conf');
    ConfigLines := TStringList.Create;
    try
      // Configurações recomendadas
      ConfigLines.Add('# Configuração SQLite');
      ConfigLines.Add('# Gerado automaticamente');
      ConfigLines.Add('');
      ConfigLines.Add('# Performance');
      ConfigLines.Add('PRAGMA journal_mode=WAL;');
      ConfigLines.Add('PRAGMA synchronous=NORMAL;');
      ConfigLines.Add('PRAGMA cache_size=10000;');
      ConfigLines.Add('PRAGMA temp_store=MEMORY;');
      ConfigLines.Add('');
      ConfigLines.Add('# Segurança');
      ConfigLines.Add('PRAGMA foreign_keys=ON;');
      ConfigLines.Add('');
      ConfigLines.Add('# Localização da DLL');
      ConfigLines.Add('DLL_PATH=' + TPath.Combine(FInstallPath, SQLITE_DLL_NAME));
      
      ConfigLines.SaveToFile(ConfigFile);
      Log('Arquivo de configuração criado: ' + ConfigFile);
      
      Result := True;
    finally
      ConfigLines.Free;
    end;
    
    Log('=== Configuração Concluída ===');
    
  except
    on E: Exception do
    begin
      Log('Erro ao configurar: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSQLiteInstaller.TestSQLiteConnection: Boolean;
var
  DllPath: string;
  Handle: THandle;
begin
  Result := False;
  
  try
    Log('=== Testando SQLite ===');
    
    DllPath := TPath.Combine(FInstallPath, SQLITE_DLL_NAME);
    
    if not TFile.Exists(DllPath) then
    begin
      Log('ERRO: DLL não encontrada em: ' + DllPath);
      Exit;
    end;
    
    // Tenta carregar a DLL
    Handle := LoadLibrary(PChar(DllPath));
    if Handle <> 0 then
    begin
      Log('SQLite DLL carregada com sucesso!');
      Log('Handle: ' + IntToHex(Handle, 8));
      FreeLibrary(Handle);
      Result := True;
    end
    else
      Log('ERRO: Não foi possível carregar a DLL. Código: ' + GetLastError.ToString);
    
    Log('=== Teste Concluído ===');
    
  except
    on E: Exception do
    begin
      Log('Erro ao testar: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
