unit uUsuario;

interface

uses
  System.SysUtils;

type
  TTipoUsuario = (tuNormal, tuAdministrador);

  TUsuario = class
  private
    FID: Integer;
    FNome: string;
    FEmail: string;
    FSenha: string;
    FTipo: TTipoUsuario;
    FDataCadastro: TDateTime;
    FAtivo: Boolean;
    
    function GetTipoStr: string;
    procedure SetTipoStr(const Value: string);
  public
    constructor Create;
    
    property ID: Integer read FID write FID;
    property Nome: string read FNome write FNome;
    property Email: string read FEmail write FEmail;
    property Senha: string read FSenha write FSenha;
    property Tipo: TTipoUsuario read FTipo write FTipo;
    property TipoStr: string read GetTipoStr write SetTipoStr;
    property DataCadastro: TDateTime read FDataCadastro write FDataCadastro;
    property Ativo: Boolean read FAtivo write FAtivo;
    
    procedure Clear;
    function IsValid(out MsgErro: string): Boolean;
  end;

implementation

{ TUsuario }

constructor TUsuario.Create;
begin
  inherited Create;
  Clear;
end;

procedure TUsuario.Clear;
begin
  FID := 0;
  FNome := '';
  FEmail := '';
  FSenha := '';
  FTipo := tuNormal;
  FDataCadastro := Now;
  FAtivo := True;
end;

function TUsuario.GetTipoStr: string;
begin
  case FTipo of
    tuNormal: Result := 'N';
    tuAdministrador: Result := 'A';
  else
    Result := 'N';
  end;
end;

procedure TUsuario.SetTipoStr(const Value: string);
begin
  if UpperCase(Value) = 'A' then
    FTipo := tuAdministrador
  else
    FTipo := tuNormal;
end;

function TUsuario.IsValid(out MsgErro: string): Boolean;
begin
  Result := True;
  MsgErro := '';
  
  if Trim(FNome) = '' then
  begin
    MsgErro := 'O nome é obrigatório!';
    Result := False;
    Exit;
  end;
  
  if Trim(FEmail) = '' then
  begin
    MsgErro := 'O e-mail é obrigatório!';
    Result := False;
    Exit;
  end;
  
  if Pos('@', FEmail) = 0 then
  begin
    MsgErro := 'E-mail inválido!';
    Result := False;
    Exit;
  end;
  
  if (FID = 0) and (Trim(FSenha) = '') then
  begin
    MsgErro := 'A senha é obrigatória!';
    Result := False;
    Exit;
  end;
  
  if (Trim(FSenha) <> '') and (Length(FSenha) < 6) then
  begin
    MsgErro := 'A senha deve ter no mínimo 6 caracteres!';
    Result := False;
    Exit;
  end;
end;

end.
