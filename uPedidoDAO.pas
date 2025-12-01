unit uPedidoDAO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, Data.DB,
  System.Generics.Collections, uPedido;

type
  TPedidoDAO = class
  private
    function InserirPedidoCabecalho(Pedido: TPedido): Integer;
    procedure InserirItensPedido(IDPedido: Integer; Pedido: TPedido);
    procedure InserirIngredientesAdicionais(IDPedido: Integer; Item: TItemPedido);

  public
    function SalvarPedido(Pedido: TPedido): Integer;
    function BuscarPedido(ID: Integer): TPedido;
    function ListarPedidosCliente(IDCliente: Integer): TObjectList<TPedido>;
    procedure AplicaPromocoes(ANPedido: Integer);
    function BuscaPromocoesDesconto(ANPedido: Integer): Real;
  end;

implementation

{ TPedidoDAO }

uses UDMDTO;

function TPedidoDAO.SalvarPedido(Pedido: TPedido): Integer;
var
  IDPedido: Integer;
begin
  Result := 0;
  
  DMDTO.FDConexao.StartTransaction;
  try

    IDPedido := InserirPedidoCabecalho(Pedido);
    
    if IDPedido > 0 then
    begin

      InserirItensPedido(IDPedido, Pedido);
      
      DMDTO.FDConexao.Commit;
      Result := IDPedido;
    end
    else
    begin
      DMDTO.FDConexao.Rollback;
    end;
  except
    on E: Exception do
    begin
      DMDTO.FDConexao.Rollback;
      raise Exception.Create('Erro ao salvar pedido: ' + E.Message);
    end;
  end;
end;

function TPedidoDAO.InserirPedidoCabecalho(Pedido: TPedido): Integer;
var
  Query: TFDQuery;
  TotalItens, TotalAdicional, TotalDesconto: Double;
  Item: TItemPedido;
  Ingrediente: TItemPedidoIngrediente;
begin
  Result := 0;

  TotalItens := 0;
  TotalAdicional := 0;
  TotalDesconto := 0;
  
  for Item in Pedido.Itens do
  begin
    TotalItens := TotalItens + (Item.ValorUnitarioOriginal * Item.Quantidade);

    for Ingrediente in Item.IngredientesPersonalizados do
    begin
      if Ingrediente.Removido then
        TotalDesconto := TotalDesconto + (Ingrediente.ValorIngrediente * Item.Quantidade)
      else
        TotalAdicional := TotalAdicional + (Ingrediente.ValorIngrediente * Item.Quantidade);
    end;
  end;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    
    Query.SQL.Clear;
    Query.SQL.Add('INSERT INTO TB_PEDIDOS (');
    Query.SQL.Add('  ID_CLIENTE,');
    Query.SQL.Add('  TOTAL_ITENS,');
    Query.SQL.Add('  TOTAL_ADICIONAL,');
    Query.SQL.Add('  TOTAL_DESCONTO,');
    Query.SQL.Add('  TOTAL_PEDIDO,');
    Query.SQL.Add('  FORMA_PAGAMENTO,');
    Query.SQL.Add('  END_ENTREGA,');
    Query.SQL.Add('  OBS,');
    Query.SQL.Add('  DATA_CADASTRO,');
    Query.SQL.Add('  ATIVO');
    Query.SQL.Add(') VALUES (');
    Query.SQL.Add('  :ID_CLIENTE,');
    Query.SQL.Add('  :TOTAL_ITENS,');
    Query.SQL.Add('  :TOTAL_ADICIONAL,');
    Query.SQL.Add('  :TOTAL_DESCONTO,');
    Query.SQL.Add('  :TOTAL_PEDIDO,');
    Query.SQL.Add('  :FORMA_PAGAMENTO,');
    Query.SQL.Add('  :END_ENTREGA,');
    Query.SQL.Add('  :OBS,');
    Query.SQL.Add('  CURRENT_TIMESTAMP,');
    Query.SQL.Add('  1');
    Query.SQL.Add(')');
    
    Query.ParamByName('ID_CLIENTE').AsInteger := Pedido.IDCliente;
    Query.ParamByName('TOTAL_ITENS').AsFloat := TotalItens;
    Query.ParamByName('TOTAL_ADICIONAL').AsFloat := TotalAdicional;
    Query.ParamByName('TOTAL_DESCONTO').AsFloat := TotalDesconto;
    Query.ParamByName('TOTAL_PEDIDO').AsFloat := Pedido.ValorTotal;
    Query.ParamByName('FORMA_PAGAMENTO').AsString := Pedido.FormaPagamento;
    Query.ParamByName('END_ENTREGA').AsString := Pedido.EnderecoEntrega;
    Query.ParamByName('OBS').AsString := Pedido.Observacao;
    
    Query.ExecSQL;

    Query.SQL.Clear;
    Query.SQL.Add('SELECT last_insert_rowid() AS ID');
    Query.Open;
    
    if not Query.IsEmpty then
      Result := Query.FieldByName('ID').AsInteger;

    AplicaPromocoes(Query.FieldByName('ID').AsInteger);

  finally
    Query.Free;
  end;
end;

procedure TPedidoDAO.InserirItensPedido(IDPedido: Integer; Pedido: TPedido);
var
  Query: TFDQuery;
  Item: TItemPedido;
  IDItem: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    
    for Item in Pedido.Itens do
    begin
      Query.SQL.Clear;
      Query.SQL.Add('INSERT INTO TB_PEDIDOS_ITENS (');
      Query.SQL.Add('  ID_PEDIDO,');
      Query.SQL.Add('  ID_PRODUTO,');
      Query.SQL.Add('  QUANTIDADE,');
      Query.SQL.Add('  VALOR_ITEM,');
      Query.SQL.Add('  TOTAL_VALOR,');
      Query.SQL.Add('  OBSERVACAO,');
      Query.SQL.Add('  DATA_CADASTRO,');
      Query.SQL.Add('  ATIVO');
      Query.SQL.Add(') VALUES (');
      Query.SQL.Add('  :ID_PEDIDO,');
      Query.SQL.Add('  :ID_PRODUTO,');
      Query.SQL.Add('  :QUANTIDADE,');
      Query.SQL.Add('  :VALOR_ITEM,');
      Query.SQL.Add('  :TOTAL_VALOR,');
      Query.SQL.Add('  :OBSERVACAO,');
      Query.SQL.Add('  CURRENT_TIMESTAMP,');
      Query.SQL.Add('  1');
      Query.SQL.Add(')');
      
      Query.ParamByName('ID_PEDIDO').AsInteger := IDPedido;
      Query.ParamByName('ID_PRODUTO').AsInteger := Item.IDProduto;
      Query.ParamByName('QUANTIDADE').AsInteger := Item.Quantidade;
      Query.ParamByName('VALOR_ITEM').AsFloat := Item.ValorUnitarioFinal;
      Query.ParamByName('TOTAL_VALOR').AsFloat := Item.GetValorTotal;
      Query.ParamByName('OBSERVACAO').AsString := Item.Observacao;
      
      Query.ExecSQL;

      Query.SQL.Clear;
      Query.SQL.Add('SELECT last_insert_rowid() AS ID');
      Query.Open;
      Query.Close;

      if Item.IngredientesPersonalizados.Count > 0 then
        InserirIngredientesAdicionais(IDPedido, Item);
    end;
  finally
    Query.Free;
  end;
end;

procedure TPedidoDAO.InserirIngredientesAdicionais(IDPedido: Integer; Item: TItemPedido);
var
  Query: TFDQuery;
  Ingrediente: TItemPedidoIngrediente;
  ValorFinal: Double;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    
    for Ingrediente in Item.IngredientesPersonalizados do
    begin
      if Ingrediente.Removido then
        ValorFinal := -Ingrediente.ValorIngrediente
      else
        ValorFinal := Ingrediente.ValorIngrediente;

      Query.SQL.Clear;
      Query.SQL.Add('INSERT INTO TB_PEDIDOS_ITENS_AD (');
      Query.SQL.Add('  ID_PEDIDO,');
      Query.SQL.Add('  ID_PRODUTO,');
      Query.SQL.Add('  ID_INGREDIENTE,');
      Query.SQL.Add('  VALOR,');
      Query.SQL.Add('  DATA_CADASTRO,');
      Query.SQL.Add('  ATIVO');
      Query.SQL.Add(') VALUES (');
      Query.SQL.Add('  :ID_PEDIDO,');
      Query.SQL.Add('  :ID_PRODUTO,');
      Query.SQL.Add('  :ID_INGREDIENTE,');
      Query.SQL.Add('  :VALOR,');
      Query.SQL.Add('  CURRENT_TIMESTAMP,');
      Query.SQL.Add('  1');
      Query.SQL.Add(')');
      
      Query.ParamByName('ID_PEDIDO').AsInteger := IDPedido;
      Query.ParamByName('ID_PRODUTO').AsInteger := Item.IDProduto;
      Query.ParamByName('ID_INGREDIENTE').AsInteger := Ingrediente.IDIngrediente;
      Query.ParamByName('VALOR').AsFloat := ValorFinal;
      
      Query.ExecSQL;
    end;
  finally
    Query.Free;
  end;
end;

procedure TPedidoDAO.AplicaPromocoes(ANPedido: Integer);
var
  LDesconto: Double;
  Query: TFDQuery;
begin
  LDesconto := BuscaPromocoesDesconto(ANPedido);
  if LDesconto > 0 then
  begin
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := DMDTO.FDConexao;
      Query.SQL.Clear;
      Query.SQL.Add('UPDATE TB_PEDIDOS ');
      Query.SQL.Add('   SET TOTAL_DESCONTO = (TOTAL_PEDIDO * :PERC_DESCONTO) / 100, ');
      Query.SQL.Add('       TOTAL_PEDIDO = TOTAL_ITENS + TOTAL_ADICIONAL - TOTAL_DESCONTO ');
      Query.SQL.Add(' WHERE ID = :NPEDIDO ');
      Query.ParamByName('PERC_DESCONTO').AsFloat := LDesconto;
      Query.ParamByName('NPEDIDO').AsInteger := ANPedido;

    finally
      Query.Free;
    end;
  end;
end;

function TPedidoDAO.BuscaPromocoesDesconto(ANPedido: Integer): Real;
var
  LDesconto: Real;
  Query: TFDQuery;
  QueryRegra: TFDQuery;
begin
  Result := 0;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;

    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_PROMOCAO WHERE ATIVO = 1 AND TIPO = ''Desconto'' ');
    Query.Open;

    if not(Query.IsEmpty) then
    begin
      LDesconto := 0;

      QueryRegra := TFDQuery.Create(nil);
      try
        QueryRegra.Connection := DMDTO.FDConexao;
        Query.First;
        while not Query.Eof do
        begin
          var LSql: String;
          LSql := Query.FieldByName('REGRA').AsString + IntToStr( ANPedido );

          QueryRegra.SQL.Clear;
          QueryRegra.SQL.Add(LSql);
          QueryRegra.Open;

          if not QueryRegra.IsEmpty then
            LDesconto := LDesconto + Query.FieldByName('PERC_DESCONTO').AsFloat;

          Query.Next;
        end;
      finally
        QueryRegra.Free;
      end;
    end;

  finally
    Query.Free;
  end;
end;

function TPedidoDAO.BuscarPedido(ID: Integer): TPedido;
var
  Query: TFDQuery;
begin
  Result := TPedido.Create;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;

    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_PEDIDOS WHERE ID = :ID AND ATIVO = 1');
    Query.ParamByName('ID').AsInteger := ID;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.IDCliente := Query.FieldByName('ID_CLIENTE').AsInteger;
      Result.FormaPagamento := Query.FieldByName('FORMA_PAGAMENTO').AsString;
      Result.EnderecoEntrega := Query.FieldByName('END_ENTREGA').AsString;
      Result.Observacao := Query.FieldByName('OBS').AsString;
      Result.DataPedido := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      
    end;
  finally
    Query.Free;
  end;
end;

function TPedidoDAO.ListarPedidosCliente(IDCliente: Integer): TObjectList<TPedido>;
var
  Query: TFDQuery;
  Pedido: TPedido;
begin
  Result := TObjectList<TPedido>.Create;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_PEDIDOS');
    Query.SQL.Add('WHERE ID_CLIENTE = :ID_CLIENTE AND ATIVO = 1');
    Query.SQL.Add('ORDER BY DATA_CADASTRO DESC');
    Query.ParamByName('ID_CLIENTE').AsInteger := IDCliente;
    Query.Open;
    
    while not Query.Eof do
    begin
      Pedido := TPedido.Create;
      Pedido.ID := Query.FieldByName('ID').AsInteger;
      Pedido.IDCliente := Query.FieldByName('ID_CLIENTE').AsInteger;
      Pedido.FormaPagamento := Query.FieldByName('FORMA_PAGAMENTO').AsString;
      Pedido.EnderecoEntrega := Query.FieldByName('END_ENTREGA').AsString;
      Pedido.Observacao := Query.FieldByName('OBS').AsString;
      Pedido.DataPedido := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      
      Result.Add(Pedido);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
