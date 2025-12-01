unit uIngrediente;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics;

type
  TIngrediente = class
  private
    FID: Integer;
    FNome: string;
    FImagem: TMemoryStream;
    FValor: Double;
    FDataCadastro: TDateTime;
    FAtivo: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ID: Integer read FID write FID;
    property Nome: string read FNome write FNome;
    property Imagem: TMemoryStream read FImagem write FImagem;
    property Valor: Double read FValor write FValor;
    property DataCadastro: TDateTime read FDataCadastro write FDataCadastro;
    property Ativo: Boolean read FAtivo write FAtivo;
    
    procedure Clear;
    function IsValid(out MsgErro: string): Boolean;
    procedure CarregarImagemDeArquivo(const CaminhoArquivo: string);
    function TemImagem: Boolean;
  end;

implementation

{ TIngrediente }

constructor TIngrediente.Create;
begin
  inherited Create;
  FImagem := TMemoryStream.Create;
  Clear;
end;

destructor TIngrediente.Destroy;
begin
  FImagem.Free;
  inherited;
end;

procedure TIngrediente.Clear;
begin
  FID := 0;
  FNome := '';
  FImagem.Clear;
  FValor := 0;
  FDataCadastro := Now;
  FAtivo := True;
end;

function TIngrediente.IsValid(out MsgErro: string): Boolean;
begin
  Result := True;
  MsgErro := '';
  
  if Trim(FNome) = '' then
  begin
    MsgErro := 'O nome do ingrediente é obrigatório!';
    Result := False;
    Exit;
  end;
  
  if FValor < 0 then
  begin
    MsgErro := 'O valor não pode ser negativo!';
    Result := False;
    Exit;
  end;
end;

procedure TIngrediente.CarregarImagemDeArquivo(const CaminhoArquivo: string);
begin
  if FileExists(CaminhoArquivo) then
  begin
    FImagem.Clear;
    FImagem.LoadFromFile(CaminhoArquivo);
  end
  else
    raise Exception.Create('Arquivo de imagem não encontrado!');
end;

function TIngrediente.TemImagem: Boolean;
begin
  Result := (FImagem <> nil) and (FImagem.Size > 0);
end;

end.
