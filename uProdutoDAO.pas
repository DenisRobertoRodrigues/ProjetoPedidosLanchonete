unit uProdutoDAO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  Data.DB, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, uProduto, System.Generics.Collections;

type
  TProdutoDAO = class
  private
    FConnection: TFDConnection;
    procedure SalvarIngredientes(Produto: TProduto);
    procedure CarregarIngredientes(Produto: TProduto);
    procedure RemoverTodosIngredientes(IDProduto: Integer);
  public
    constructor Create(AConnection: TFDConnection);
    
    function Inserir(Produto: TProduto): Boolean;
    function Atualizar(Produto: TProduto): Boolean;
    function Excluir(ID: Integer): Boolean;
    function BuscarPorID(ID: Integer): TProduto;
    function BuscarPorNome(const Nome: string): TProduto;
    function ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TProduto>;
    
    property Connection: TFDConnection read FConnection write FConnection;
  end;

implementation

{ TProdutoDAO }

uses UDMDTO;

constructor TProdutoDAO.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TProdutoDAO.Inserir(Produto: TProduto): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Produto.IsValid(MsgErro) then
    raise Exception.Create(MsgErro);
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;

    DMDTO.FDConexao.StartTransaction;
    try

      Query.SQL.Clear;
      Query.SQL.Add('INSERT INTO TB_PRODUTOS (NOME, IMAGEM, VALOR, ATIVO)');
      Query.SQL.Add('VALUES (:NOME, :IMAGEM, :VALOR, :ATIVO)');
      
      Query.ParamByName('NOME').AsString := Produto.Nome;

      if Produto.TemImagem then
      begin
        Produto.Imagem.Position := 0;
        Query.ParamByName('IMAGEM').LoadFromStream(Produto.Imagem, ftBlob);
      end
      else
        Query.ParamByName('IMAGEM').Clear;
      
      Query.ParamByName('VALOR').AsFloat := Produto.Valor;
      Query.ParamByName('ATIVO').AsInteger := Integer(Produto.Ativo);
      
      Query.ExecSQL;

      Query.SQL.Clear;
      Query.SQL.Add('SELECT last_insert_rowid() AS ID');
      Query.Open;
      Produto.ID := Query.FieldByName('ID').AsInteger;
      Query.Close;

      SalvarIngredientes(Produto);

      DMDTO.FDConexao.Commit;
      Result := True;
      
    except
      on E: Exception do
      begin
        DMDTO.FDConexao.Rollback;
        raise Exception.Create('Erro ao inserir produto: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

function TProdutoDAO.Atualizar(Produto: TProduto): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Produto.IsValid(MsgErro) then
    raise Exception.Create(MsgErro);
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;

    DMDTO.FDConexao.StartTransaction;
    try
      Query.SQL.Clear;
      Query.SQL.Add('UPDATE TB_PRODUTOS SET');
      Query.SQL.Add('  NOME = :NOME,');
      Query.SQL.Add('  IMAGEM = :IMAGEM,');
      Query.SQL.Add('  VALOR = :VALOR,');
      Query.SQL.Add('  ATIVO = :ATIVO');
      Query.SQL.Add('WHERE ID = :ID');
      
      Query.ParamByName('NOME').AsString := Produto.Nome;

      if Produto.TemImagem then
      begin
        Produto.Imagem.Position := 0;
        Query.ParamByName('IMAGEM').LoadFromStream(Produto.Imagem, ftBlob);
      end
      else
        Query.ParamByName('IMAGEM').Clear;
      
      Query.ParamByName('VALOR').AsFloat := Produto.Valor;
      Query.ParamByName('ATIVO').AsInteger := Integer(Produto.Ativo);
      Query.ParamByName('ID').AsInteger := Produto.ID;
      
      Query.ExecSQL;

      RemoverTodosIngredientes(Produto.ID);

      SalvarIngredientes(Produto);

      DMDTO.FDConexao.Commit;
      Result := True;
      
    except
      on E: Exception do
      begin
        DMDTO.FDConexao.Rollback;
        raise Exception.Create('Erro ao atualizar produto: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

function TProdutoDAO.Excluir(ID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;

    DMDTO.FDConexao.StartTransaction;
    try
      RemoverTodosIngredientes(ID);

      Query.SQL.Clear;
      Query.SQL.Add('DELETE FROM TB_PRODUTOS WHERE ID = :ID');
      Query.ParamByName('ID').AsInteger := ID;
      Query.ExecSQL;

      DMDTO.FDConexao.Commit;
      Result := True;
      
    except
      on E: Exception do
      begin
        DMDTO.FDConexao.Rollback;
        raise Exception.Create('Erro ao excluir produto: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TProdutoDAO.SalvarIngredientes(Produto: TProduto);
var
  Query: TFDQuery;
  ProdutoIngrediente: TProdutoIngrediente;
begin
  if Produto.Ingredientes.Count = 0 then
    Exit;
    
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('INSERT INTO TB_PRODUTOS_INGREDIENTES');
    Query.SQL.Add('(ID_PRODUTO, ID_INGREDIENTE, ATIVO)');
    Query.SQL.Add('VALUES (:ID_PRODUTO, :ID_INGREDIENTE, :ATIVO)');
    
    for ProdutoIngrediente in Produto.Ingredientes do
    begin
      Query.ParamByName('ID_PRODUTO').AsInteger := Produto.ID;
      Query.ParamByName('ID_INGREDIENTE').AsInteger := ProdutoIngrediente.IDIngrediente;
      Query.ParamByName('ATIVO').AsInteger := Integer(ProdutoIngrediente.Ativo);
      Query.ExecSQL;
    end;
  finally
    Query.Free;
  end;
end;

procedure TProdutoDAO.CarregarIngredientes(Produto: TProduto);
var
  Query: TFDQuery;
  ProdutoIngrediente: TProdutoIngrediente;
begin
  Produto.Ingredientes.Clear;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT PI.*, I.NOME AS NOME_INGREDIENTE');
    Query.SQL.Add('FROM TB_PRODUTOS_INGREDIENTES PI');
    Query.SQL.Add('INNER JOIN TB_INGREDIENTES I ON I.ID = PI.ID_INGREDIENTE');
    Query.SQL.Add('WHERE PI.ID_PRODUTO = :ID_PRODUTO');
    Query.SQL.Add('AND PI.ATIVO = 1');
    Query.SQL.Add('ORDER BY I.NOME');
    
    Query.ParamByName('ID_PRODUTO').AsInteger := Produto.ID;
    Query.Open;
    
    while not Query.Eof do
    begin
      ProdutoIngrediente := TProdutoIngrediente.Create;
      ProdutoIngrediente.ID := Query.FieldByName('ID').AsInteger;
      ProdutoIngrediente.IDProduto := Query.FieldByName('ID_PRODUTO').AsInteger;
      ProdutoIngrediente.IDIngrediente := Query.FieldByName('ID_INGREDIENTE').AsInteger;
      ProdutoIngrediente.NomeIngrediente := Query.FieldByName('NOME_INGREDIENTE').AsString;
      ProdutoIngrediente.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      ProdutoIngrediente.Ativo := Query.FieldByName('ATIVO').AsBoolean;

      Produto.Ingredientes.Add(ProdutoIngrediente);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TProdutoDAO.RemoverTodosIngredientes(IDProduto: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('DELETE FROM TB_PRODUTOS_INGREDIENTES');
    Query.SQL.Add('WHERE ID_PRODUTO = :ID_PRODUTO');
    Query.ParamByName('ID_PRODUTO').AsInteger := IDProduto;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TProdutoDAO.BuscarPorID(ID: Integer): TProduto;
var
  Query: TFDQuery;
  BlobStream: TStream;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_PRODUTOS WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TProduto.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Nome := Query.FieldByName('NOME').AsString;
      Result.Valor := Query.FieldByName('VALOR').AsFloat;
      Result.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Result.Ativo := Query.FieldByName('ATIVO').AsBoolean;

      if not Query.FieldByName('IMAGEM').IsNull then
      begin
        BlobStream := Query.CreateBlobStream(Query.FieldByName('IMAGEM'), bmRead);
        try
          Result.Imagem.Clear;
          Result.Imagem.CopyFrom(BlobStream, BlobStream.Size);
        finally
          BlobStream.Free;
        end;
      end;

      CarregarIngredientes(Result);
    end;
  finally
    Query.Free;
  end;
end;

function TProdutoDAO.BuscarPorNome(const Nome: string): TProduto;
var
  Query: TFDQuery;
  BlobStream: TStream;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_PRODUTOS WHERE NOME = :NOME');
    Query.ParamByName('NOME').AsString := Nome;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TProduto.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Nome := Query.FieldByName('NOME').AsString;
      Result.Valor := Query.FieldByName('VALOR').AsFloat;
      Result.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Result.Ativo := Query.FieldByName('ATIVO').AsBoolean;

      if not Query.FieldByName('IMAGEM').IsNull then
      begin
        BlobStream := Query.CreateBlobStream(Query.FieldByName('IMAGEM'), bmRead);
        try
          Result.Imagem.Clear;
          Result.Imagem.CopyFrom(BlobStream, BlobStream.Size);
        finally
          BlobStream.Free;
        end;
      end;

      CarregarIngredientes(Result);
    end;
  finally
    Query.Free;
  end;
end;

function TProdutoDAO.ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TProduto>;
var
  Query: TFDQuery;
  Produto: TProduto;
  BlobStream: TStream;
begin
  Result := TObjectList<TProduto>.Create;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_PRODUTOS');
    
    if ApenasAtivos then
      Query.SQL.Add('WHERE ATIVO = 1');
      
    Query.SQL.Add('ORDER BY NOME');
    Query.Open;
    
    while not Query.Eof do
    begin
      Produto := TProduto.Create;
      Produto.ID := Query.FieldByName('ID').AsInteger;
      Produto.Nome := Query.FieldByName('NOME').AsString;
      Produto.Valor := Query.FieldByName('VALOR').AsFloat;
      Produto.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Produto.Ativo := Query.FieldByName('ATIVO').AsBoolean;

      if not Query.FieldByName('IMAGEM').IsNull then
      begin
        BlobStream := Query.CreateBlobStream(Query.FieldByName('IMAGEM'), bmRead);
        try
          if BlobStream.Size > 0 then
          begin
            Produto.Imagem.Clear;
            Produto.Imagem.CopyFrom(BlobStream, BlobStream.Size);
          end;
        finally
          BlobStream.Free;
        end;
      end;

      CarregarIngredientes(Produto);
      
      Result.Add(Produto);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
