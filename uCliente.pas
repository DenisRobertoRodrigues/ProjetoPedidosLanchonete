unit uCliente;

interface

uses
  System.SysUtils;

type
  TCliente = class
  private
    FID: Integer;
    FNome: string;
    FContato: string;
    FDocumento: string;
    FDataCadastro: TDateTime;
    FAtivo: Boolean;
  public
    constructor Create;
    
    property ID: Integer read FID write FID;
    property Nome: string read FNome write FNome;
    property Contato: string read FContato write FContato;
    property Documento: string read FDocumento write FDocumento;
    property DataCadastro: TDateTime read FDataCadastro write FDataCadastro;
    property Ativo: Boolean read FAtivo write FAtivo;
    
    procedure Clear;
    function IsValid(out MsgErro: string): Boolean;
  end;

implementation

{ TCliente }

constructor TCliente.Create;
begin
  inherited Create;
  Clear;
end;

procedure TCliente.Clear;
begin
  FID := 0;
  FNome := '';
  FContato := '';
  FDocumento := '';
  FDataCadastro := Now;
  FAtivo := True;
end;

function TCliente.IsValid(out MsgErro: string): Boolean;
begin
  Result := True;
  MsgErro := '';
  
  if Trim(FNome) = '' then
  begin
    MsgErro := 'O nome é obrigatório!';
    Result := False;
    Exit;
  end;
  
  if Trim(FContato) = '' then
  begin
    MsgErro := 'O contato é obrigatório!';
    Result := False;
    Exit;
  end;
  
  if Trim(FDocumento) = '' then
  begin
    MsgErro := 'O documento é obrigatório!';
    Result := False;
    Exit;
  end;
end;

end.
