unit uClienteDAO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  Data.DB, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, uCliente, System.Generics.Collections;

type
  TClienteDAO = class
  private

  public
    function Inserir(Cliente: TCliente): Boolean;
    function Atualizar(Cliente: TCliente): Boolean;
    function Excluir(ID: Integer): Boolean;
    function BuscarPorID(ID: Integer): TCliente;
    function BuscarPorDocumento(const Documento: string): TCliente;
    function ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TCliente>;
  end;

implementation

{ TClienteDAO }

uses UDMDTO, ULog;

function TClienteDAO.Inserir(Cliente: TCliente): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Cliente.IsValid(MsgErro) then
    raise Exception.Create(AppLog.RegistraLogEx(MsgErro));
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('INSERT INTO TB_CLIENTES (NOME, CONTATO, DOCUMENTO, ATIVO)');
    Query.SQL.Add('VALUES (:NOME, :CONTATO, :DOCUMENTO, :ATIVO)');
    
    Query.ParamByName('NOME').AsString := Cliente.Nome;
    Query.ParamByName('CONTATO').AsString := Cliente.Contato;
    Query.ParamByName('DOCUMENTO').AsString := Cliente.Documento;
    Query.ParamByName('ATIVO').AsInteger := Integer(Cliente.Ativo);
    
    try
      Query.ExecSQL;

      Query.SQL.Clear;
      Query.SQL.Add('SELECT last_insert_rowid() AS ID');
      Query.Open;
      Cliente.ID := Query.FieldByName('ID').AsInteger;
      
      Result := True;
    except
      on E: Exception do
        raise Exception.Create(AppLog.RegistraLogEx('Erro ao inserir cliente: ' + E.Message));
    end;
  finally
    Query.Free;
  end;
end;

function TClienteDAO.Atualizar(Cliente: TCliente): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Cliente.IsValid(MsgErro) then
    raise Exception.Create(AppLog.RegistraLogEx(MsgErro));
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('UPDATE TB_CLIENTES SET');
    Query.SQL.Add('  NOME = :NOME,');
    Query.SQL.Add('  CONTATO = :CONTATO,');
    Query.SQL.Add('  DOCUMENTO = :DOCUMENTO,');
    Query.SQL.Add('  ATIVO = :ATIVO');
    Query.SQL.Add('WHERE ID = :ID');
    
    Query.ParamByName('NOME').AsString := Cliente.Nome;
    Query.ParamByName('CONTATO').AsString := Cliente.Contato;
    Query.ParamByName('DOCUMENTO').AsString := Cliente.Documento;
    Query.ParamByName('ATIVO').AsInteger := Integer(Cliente.Ativo);
    Query.ParamByName('ID').AsInteger := Cliente.ID;
    
    try
      Query.ExecSQL;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create(AppLog.RegistraLogEx('Erro ao atualizar cliente: ' + E.Message));
    end;
  finally
    Query.Free;
  end;
end;

function TClienteDAO.Excluir(ID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('DELETE FROM TB_CLIENTES WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    
    try
      Query.ExecSQL;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create(AppLog.RegistraLogEx('Erro ao excluir cliente: ' + E.Message));
    end;
  finally
    Query.Free;
  end;
end;

function TClienteDAO.BuscarPorID(ID: Integer): TCliente;
var
  Query: TFDQuery;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_CLIENTES WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TCliente.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Nome := Query.FieldByName('NOME').AsString;
      Result.Contato := Query.FieldByName('CONTATO').AsString;
      Result.Documento := Query.FieldByName('DOCUMENTO').AsString;
      Result.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Result.Ativo := Query.FieldByName('ATIVO').AsBoolean;
    end;
  finally
    Query.Free;
  end;
end;

function TClienteDAO.BuscarPorDocumento(const Documento: string): TCliente;
var
  Query: TFDQuery;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_CLIENTES WHERE DOCUMENTO = :DOCUMENTO');
    Query.ParamByName('DOCUMENTO').AsString := Documento;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TCliente.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Nome := Query.FieldByName('NOME').AsString;
      Result.Contato := Query.FieldByName('CONTATO').AsString;
      Result.Documento := Query.FieldByName('DOCUMENTO').AsString;
      Result.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Result.Ativo := Query.FieldByName('ATIVO').AsBoolean;
    end;
  finally
    Query.Free;
  end;
end;

function TClienteDAO.ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TCliente>;
var
  Query: TFDQuery;
  Cliente: TCliente;
begin
  Result := TObjectList<TCliente>.Create;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_CLIENTES');
    
    if ApenasAtivos then
      Query.SQL.Add('WHERE ATIVO = 1');
      
    Query.SQL.Add('ORDER BY NOME');
    Query.Open;
    
    while not Query.Eof do
    begin
      Cliente := TCliente.Create;
      Cliente.ID := Query.FieldByName('ID').AsInteger;
      Cliente.Nome := Query.FieldByName('NOME').AsString;
      Cliente.Contato := Query.FieldByName('CONTATO').AsString;
      Cliente.Documento := Query.FieldByName('DOCUMENTO').AsString;
      Cliente.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Cliente.Ativo := Query.FieldByName('ATIVO').AsBoolean;
      
      Result.Add(Cliente);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
