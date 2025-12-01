unit uPedido;

interface

uses
  System.SysUtils, System.Generics.Collections, uProduto;

type
  TItemPedidoIngrediente = class
  private
    FIDIngrediente: Integer;
    FNomeIngrediente: string;
    FValorIngrediente: Double;
    FRemovido: Boolean;
  public
    property IDIngrediente: Integer read FIDIngrediente write FIDIngrediente;
    property NomeIngrediente: string read FNomeIngrediente write FNomeIngrediente;
    property ValorIngrediente: Double read FValorIngrediente write FValorIngrediente;
    property Removido: Boolean read FRemovido write FRemovido;
  end;

  TItemPedido = class
  private
    FID: Integer;
    FIDProduto: Integer;
    FNomeProduto: string;
    FValorUnitarioOriginal: Double;
    FValorUnitarioFinal: Double;
    FQuantidade: Integer;
    FObservacao: string;
    FIngredientesPersonalizados: TObjectList<TItemPedidoIngrediente>;
    
    function CalcularValorFinal: Double;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ID: Integer read FID write FID;
    property IDProduto: Integer read FIDProduto write FIDProduto;
    property NomeProduto: string read FNomeProduto write FNomeProduto;
    property ValorUnitarioOriginal: Double read FValorUnitarioOriginal write FValorUnitarioOriginal;
    property ValorUnitarioFinal: Double read FValorUnitarioFinal;
    property Quantidade: Integer read FQuantidade write FQuantidade;
    property Observacao: string read FObservacao write FObservacao;
    property IngredientesPersonalizados: TObjectList<TItemPedidoIngrediente> read FIngredientesPersonalizados;
    
    procedure AdicionarIngrediente(IDIngrediente: Integer; const Nome: string; Valor: Double);
    procedure RemoverIngrediente(IDIngrediente: Integer; const Nome: string; Valor: Double);
    function GetValorTotal: Double;
    function GetDescricaoPersonalizacao: string;
    procedure AtualizarValorFinal;
  end;

  TPedido = class
  private
    FID: Integer;
    FNumero: Integer;
    FIDCliente: Integer;
    FNomeCliente: string;
    FDataPedido: TDateTime;
    FStatus: string;
    FItens: TObjectList<TItemPedido>;
    FCotacaoDolar: Double;
    FFormaPagamento: string;
    FEnderecoEntrega: string;
    FObservacao: string;
    
    function GetValorTotal: Double;
    function GetValorTotalDolar: Double;
    function GetTotalItens: Double;
    function GetTotalAdicional: Double;
    function GetTotalDesconto: Double;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ID: Integer read FID write FID;
    property Numero: Integer read FNumero write FNumero;
    property IDCliente: Integer read FIDCliente write FIDCliente;
    property NomeCliente: string read FNomeCliente write FNomeCliente;
    property DataPedido: TDateTime read FDataPedido write FDataPedido;
    property Status: string read FStatus write FStatus;
    property Itens: TObjectList<TItemPedido> read FItens;
    property CotacaoDolar: Double read FCotacaoDolar write FCotacaoDolar;
    property FormaPagamento: string read FFormaPagamento write FFormaPagamento;
    property EnderecoEntrega: string read FEnderecoEntrega write FEnderecoEntrega;
    property Observacao: string read FObservacao write FObservacao;
    
    property ValorTotal: Double read GetValorTotal;
    property ValorTotalDolar: Double read GetValorTotalDolar;
    property TotalItens: Double read GetTotalItens;
    property TotalAdicional: Double read GetTotalAdicional;
    property TotalDesconto: Double read GetTotalDesconto;
    
    procedure AdicionarItem(Item: TItemPedido);
    procedure RemoverItem(Index: Integer);
    procedure LimparItens;
  end;

implementation

{ TItemPedidoIngrediente }

{ TItemPedido }

constructor TItemPedido.Create;
begin
  inherited Create;
  FIngredientesPersonalizados := TObjectList<TItemPedidoIngrediente>.Create;
  FQuantidade := 1;
  FValorUnitarioOriginal := 0;
  FValorUnitarioFinal := 0;
end;

destructor TItemPedido.Destroy;
begin
  FIngredientesPersonalizados.Free;
  inherited;
end;

procedure TItemPedido.AdicionarIngrediente(IDIngrediente: Integer; const Nome: string; Valor: Double);
var
  Ingrediente: TItemPedidoIngrediente;
begin
  Ingrediente := TItemPedidoIngrediente.Create;
  Ingrediente.IDIngrediente := IDIngrediente;
  Ingrediente.NomeIngrediente := Nome;
  Ingrediente.ValorIngrediente := Valor;
  Ingrediente.Removido := False; // Adicionado
  
  FIngredientesPersonalizados.Add(Ingrediente);
  AtualizarValorFinal;
end;

procedure TItemPedido.RemoverIngrediente(IDIngrediente: Integer; const Nome: string; Valor: Double);
var
  Ingrediente: TItemPedidoIngrediente;
begin
  Ingrediente := TItemPedidoIngrediente.Create;
  Ingrediente.IDIngrediente := IDIngrediente;
  Ingrediente.NomeIngrediente := Nome;
  Ingrediente.ValorIngrediente := Valor;
  Ingrediente.Removido := True; // Removido
  
  FIngredientesPersonalizados.Add(Ingrediente);
  AtualizarValorFinal;
end;

function TItemPedido.CalcularValorFinal: Double;
var
  Ingrediente: TItemPedidoIngrediente;
begin
  Result := FValorUnitarioOriginal;

  for Ingrediente in FIngredientesPersonalizados do
  begin
    if Ingrediente.Removido then
      Result := Result - Ingrediente.ValorIngrediente // Subtrai ingrediente removido
    else
      Result := Result + Ingrediente.ValorIngrediente; // Soma ingrediente adicionado
  end;

  if Result < 0 then
    Result := 0;
end;

procedure TItemPedido.AtualizarValorFinal;
begin
  FValorUnitarioFinal := CalcularValorFinal;
end;

function TItemPedido.GetValorTotal: Double;
begin
  Result := FValorUnitarioFinal * FQuantidade;
end;

function TItemPedido.GetDescricaoPersonalizacao: string;
var
  Ingrediente: TItemPedidoIngrediente;
  Adicionados, Removidos: string;
begin
  Result := '';
  Adicionados := '';
  Removidos := '';
  
  for Ingrediente in FIngredientesPersonalizados do
  begin
    if Ingrediente.Removido then
    begin
      if Removidos <> '' then
        Removidos := Removidos + ', ';
      Removidos := Removidos + Ingrediente.NomeIngrediente;
    end
    else
    begin
      if Adicionados <> '' then
        Adicionados := Adicionados + ', ';
      Adicionados := Adicionados + Ingrediente.NomeIngrediente;
    end;
  end;
  
  if Removidos <> '' then
    Result := 'Sem: ' + Removidos;
    
  if Adicionados <> '' then
  begin
    if Result <> '' then
      Result := Result + ' | ';
    Result := Result + 'Adicionar: ' + Adicionados;
  end;
end;

{ TPedido }

constructor TPedido.Create;
begin
  inherited Create;
  FItens := TObjectList<TItemPedido>.Create;
  FDataPedido := Now;
  FStatus := 'Novo';
  FCotacaoDolar := 5.50;
end;

destructor TPedido.Destroy;
begin
  FItens.Free;
  inherited;
end;

procedure TPedido.AdicionarItem(Item: TItemPedido);
begin
  FItens.Add(Item);
end;

procedure TPedido.RemoverItem(Index: Integer);
begin
  if (Index >= 0) and (Index < FItens.Count) then
    FItens.Delete(Index);
end;

procedure TPedido.LimparItens;
begin
  FItens.Clear;
end;

function TPedido.GetValorTotal: Double;
var
  Item: TItemPedido;
begin
  Result := 0;
  for Item in FItens do
    Result := Result + Item.GetValorTotal;
end;

function TPedido.GetValorTotalDolar: Double;
begin
  if FCotacaoDolar > 0 then
    Result := GetValorTotal / FCotacaoDolar
  else
    Result := 0;
end;

function TPedido.GetTotalItens: Double;
var
  Item: TItemPedido;
begin
  Result := 0;
  for Item in FItens do
    Result := Result + (Item.ValorUnitarioOriginal * Item.Quantidade);
end;

function TPedido.GetTotalAdicional: Double;
var
  Item: TItemPedido;
  Ingrediente: TItemPedidoIngrediente;
begin
  Result := 0;
  for Item in FItens do
  begin
    for Ingrediente in Item.IngredientesPersonalizados do
    begin
      if not Ingrediente.Removido then
        Result := Result + (Ingrediente.ValorIngrediente * Item.Quantidade);
    end;
  end;
end;

function TPedido.GetTotalDesconto: Double;
var
  Item: TItemPedido;
  Ingrediente: TItemPedidoIngrediente;
begin
  Result := 0;
  for Item in FItens do
  begin
    for Ingrediente in Item.IngredientesPersonalizados do
    begin
      if Ingrediente.Removido then
        Result := Result + (Ingrediente.ValorIngrediente * Item.Quantidade);
    end;
  end;
end;

end.
