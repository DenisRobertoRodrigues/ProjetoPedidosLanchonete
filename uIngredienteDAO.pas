unit uIngredienteDAO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  Data.DB, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, uIngrediente, System.Generics.Collections;

type
  TIngredienteDAO = class
  private

  public
    function Inserir(Ingrediente: TIngrediente): Boolean;
    function Atualizar(Ingrediente: TIngrediente): Boolean;
    function Excluir(ID: Integer): Boolean;
    function BuscarPorID(ID: Integer): TIngrediente;
    function BuscarPorNome(const Nome: string): TIngrediente;
    function ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TIngrediente>;

  end;

implementation

{ TIngredienteDAO }

uses UDMDTO;

function TIngredienteDAO.Inserir(Ingrediente: TIngrediente): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Ingrediente.IsValid(MsgErro) then
    raise Exception.Create(MsgErro);
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('INSERT INTO TB_INGREDIENTES (NOME, IMAGEM, VALOR, ATIVO)');
    Query.SQL.Add('VALUES (:NOME, :IMAGEM, :VALOR, :ATIVO)');
    
    Query.ParamByName('NOME').AsString := Ingrediente.Nome;

    if Ingrediente.TemImagem then
    begin
      Ingrediente.Imagem.Position := 0;
      Query.ParamByName('IMAGEM').LoadFromStream(Ingrediente.Imagem, ftBlob);
    end
    else
      Query.ParamByName('IMAGEM').Clear;
    
    Query.ParamByName('VALOR').AsFloat := Ingrediente.Valor;
    Query.ParamByName('ATIVO').AsInteger := Integer(Ingrediente.Ativo);
    
    try
      Query.ExecSQL;

      Query.SQL.Clear;
      Query.SQL.Add('SELECT last_insert_rowid() AS ID');
      Query.Open;
      Ingrediente.ID := Query.FieldByName('ID').AsInteger;
      
      Result := True;
    except
      on E: Exception do
        raise Exception.Create('Erro ao inserir ingrediente: ' + E.Message);
    end;
  finally
    Query.Free;
  end;
end;

function TIngredienteDAO.Atualizar(Ingrediente: TIngrediente): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Ingrediente.IsValid(MsgErro) then
    raise Exception.Create(MsgErro);
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('UPDATE TB_INGREDIENTES SET');
    Query.SQL.Add('  NOME = :NOME,');
    Query.SQL.Add('  IMAGEM = :IMAGEM,');
    Query.SQL.Add('  VALOR = :VALOR,');
    Query.SQL.Add('  ATIVO = :ATIVO');
    Query.SQL.Add('WHERE ID = :ID');
    
    Query.ParamByName('NOME').AsString := Ingrediente.Nome;

    if Ingrediente.TemImagem then
    begin
      Ingrediente.Imagem.Position := 0;
      Query.ParamByName('IMAGEM').LoadFromStream(Ingrediente.Imagem, ftBlob);
    end
    else
      Query.ParamByName('IMAGEM').Clear;
    
    Query.ParamByName('VALOR').AsFloat := Ingrediente.Valor;
    Query.ParamByName('ATIVO').AsInteger := Integer(Ingrediente.Ativo);
    Query.ParamByName('ID').AsInteger := Ingrediente.ID;
    
    try
      Query.ExecSQL;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create('Erro ao atualizar ingrediente: ' + E.Message);
    end;
  finally
    Query.Free;
  end;
end;

function TIngredienteDAO.Excluir(ID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('DELETE FROM TB_INGREDIENTES WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    
    try
      Query.ExecSQL;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create('Erro ao excluir ingrediente: ' + E.Message);
    end;
  finally
    Query.Free;
  end;
end;

function TIngredienteDAO.BuscarPorID(ID: Integer): TIngrediente;
var
  Query: TFDQuery;
  BlobStream: TStream;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_INGREDIENTES WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TIngrediente.Create;
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
    end;
  finally
    Query.Free;
  end;
end;

function TIngredienteDAO.BuscarPorNome(const Nome: string): TIngrediente;
var
  Query: TFDQuery;
  BlobStream: TStream;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_INGREDIENTES WHERE NOME = :NOME');
    Query.ParamByName('NOME').AsString := Nome;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TIngrediente.Create;
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
    end;
  finally
    Query.Free;
  end;
end;

function TIngredienteDAO.ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TIngrediente>;
var
  Query: TFDQuery;
  Ingrediente: TIngrediente;
  BlobStream: TStream;
begin
  Result := TObjectList<TIngrediente>.Create;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_INGREDIENTES');
    
    if ApenasAtivos then
      Query.SQL.Add('WHERE ATIVO = 1');
      
    Query.SQL.Add('ORDER BY NOME');
    Query.Open;
    
    while not Query.Eof do
    begin
      Ingrediente := TIngrediente.Create;
      Ingrediente.ID := Query.FieldByName('ID').AsInteger;
      Ingrediente.Nome := Query.FieldByName('NOME').AsString;
      Ingrediente.Valor := Query.FieldByName('VALOR').AsFloat;
      Ingrediente.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Ingrediente.Ativo := Query.FieldByName('ATIVO').AsBoolean;

      if not Query.FieldByName('IMAGEM').IsNull then
      begin
        BlobStream := Query.CreateBlobStream(Query.FieldByName('IMAGEM'), bmRead);
        try
          if BlobStream.Size > 0 then
          begin
            Ingrediente.Imagem.Clear;
            Ingrediente.Imagem.CopyFrom(BlobStream, BlobStream.Size);
          end;
        finally
          BlobStream.Free;
        end;
      end;
      
      Result.Add(Ingrediente);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
