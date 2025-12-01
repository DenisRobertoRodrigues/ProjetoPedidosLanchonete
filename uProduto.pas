unit uProduto;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TProdutoIngrediente = class
  private
    FID: Integer;
    FIDProduto: Integer;
    FIDIngrediente: Integer;
    FNomeIngrediente: string;
    FDataCadastro: TDateTime;
    FAtivo: Boolean;
  public
    property ID: Integer read FID write FID;
    property IDProduto: Integer read FIDProduto write FIDProduto;
    property IDIngrediente: Integer read FIDIngrediente write FIDIngrediente;
    property NomeIngrediente: string read FNomeIngrediente write FNomeIngrediente;
    property DataCadastro: TDateTime read FDataCadastro write FDataCadastro;
    property Ativo: Boolean read FAtivo write FAtivo;
  end;

  TProduto = class
  private
    FID: Integer;
    FNome: string;
    FImagem: TMemoryStream;
    FValor: Double;
    FDataCadastro: TDateTime;
    FAtivo: Boolean;
    FIngredientes: TObjectList<TProdutoIngrediente>;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ID: Integer read FID write FID;
    property Nome: string read FNome write FNome;
    property Imagem: TMemoryStream read FImagem write FImagem;
    property Valor: Double read FValor write FValor;
    property DataCadastro: TDateTime read FDataCadastro write FDataCadastro;
    property Ativo: Boolean read FAtivo write FAtivo;
    property Ingredientes: TObjectList<TProdutoIngrediente> read FIngredientes;
    
    procedure Clear;
    function IsValid(out MsgErro: string): Boolean;
    procedure CarregarImagemDeArquivo(const CaminhoArquivo: string);
    function TemImagem: Boolean;
    procedure AdicionarIngrediente(IDIngrediente: Integer; const NomeIngrediente: string);
    procedure RemoverIngrediente(IDIngrediente: Integer);
    function TemIngrediente(IDIngrediente: Integer): Boolean;
  end;

implementation

{ TProduto }

constructor TProduto.Create;
begin
  inherited Create;
  FImagem := TMemoryStream.Create;
  FIngredientes := TObjectList<TProdutoIngrediente>.Create;
  Clear;
end;

destructor TProduto.Destroy;
begin
  FIngredientes.Free;
  FImagem.Free;
  inherited;
end;

procedure TProduto.Clear;
begin
  FID := 0;
  FNome := '';
  FImagem.Clear;
  FValor := 0;
  FDataCadastro := Now;
  FAtivo := True;
  FIngredientes.Clear;
end;

function TProduto.IsValid(out MsgErro: string): Boolean;
begin
  Result := True;
  MsgErro := '';
  
  if Trim(FNome) = '' then
  begin
    MsgErro := 'O nome do produto é obrigatório!';
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

procedure TProduto.CarregarImagemDeArquivo(const CaminhoArquivo: string);
begin
  if FileExists(CaminhoArquivo) then
  begin
    FImagem.Clear;
    FImagem.LoadFromFile(CaminhoArquivo);
  end
  else
    raise Exception.Create('Arquivo de imagem não encontrado!');
end;

function TProduto.TemImagem: Boolean;
begin
  Result := (FImagem <> nil) and (FImagem.Size > 0);
end;

procedure TProduto.AdicionarIngrediente(IDIngrediente: Integer; const NomeIngrediente: string);
var
  ProdutoIngrediente: TProdutoIngrediente;
begin
  if not TemIngrediente(IDIngrediente) then
  begin
    ProdutoIngrediente := TProdutoIngrediente.Create;
    ProdutoIngrediente.IDProduto := FID;
    ProdutoIngrediente.IDIngrediente := IDIngrediente;
    ProdutoIngrediente.NomeIngrediente := NomeIngrediente;
    ProdutoIngrediente.Ativo := True;
    ProdutoIngrediente.DataCadastro := Now;
    
    FIngredientes.Add(ProdutoIngrediente);
  end;
end;

procedure TProduto.RemoverIngrediente(IDIngrediente: Integer);
var
  I: Integer;
begin
  for I := FIngredientes.Count - 1 downto 0 do
  begin
    if FIngredientes[I].IDIngrediente = IDIngrediente then
    begin
      FIngredientes.Delete(I);
      Break;
    end;
  end;
end;

function TProduto.TemIngrediente(IDIngrediente: Integer): Boolean;
var
  ProdutoIngrediente: TProdutoIngrediente;
begin
  Result := False;
  for ProdutoIngrediente in FIngredientes do
  begin
    if ProdutoIngrediente.IDIngrediente = IDIngrediente then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
