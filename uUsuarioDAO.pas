unit uUsuarioDAO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, Data.DB, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  uUsuario, uCriptografia, System.Generics.Collections;

type
  TUsuarioDAO = class
  private

  public
    function Inserir(Usuario: TUsuario): Boolean;
    function Atualizar(Usuario: TUsuario): Boolean;
    function Excluir(ID: Integer): Boolean;
    function BuscarPorID(ID: Integer): TUsuario;
    function BuscarPorEmail(const Email: string): TUsuario;
    function ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TUsuario>;
  end;

implementation

{ TUsuarioDAO }

uses UDMDTO, ULog;

function TUsuarioDAO.Inserir(Usuario: TUsuario): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Usuario.IsValid(MsgErro) then
    raise Exception.Create( AppLog.RegistraLogEx( MsgErro) );

  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('INSERT INTO TB_USUARIOS (NOME, EMAIL, SENHA, TIPO, ATIVO)');
    Query.SQL.Add('VALUES (:NOME, :EMAIL, :SENHA, :TIPO, :ATIVO)');
    
    Query.ParamByName('NOME').AsString := Usuario.Nome;
    Query.ParamByName('EMAIL').AsString := Usuario.Email;
    Query.ParamByName('SENHA').AsString := TCriptografia.CriptografarSenha(Usuario.Senha);
    Query.ParamByName('TIPO').AsString := Usuario.TipoStr;
    Query.ParamByName('ATIVO').AsInteger := Integer(Usuario.Ativo);
    
    try
      Query.ExecSQL;

      Query.SQL.Clear;
      Query.SQL.Add('SELECT last_insert_rowid() AS ID');
      Query.Open;
      Usuario.ID := Query.FieldByName('ID').AsInteger;
      
      Result := True;
    except
      on E: Exception do
        raise Exception.Create( AppLog.RegistraLogEx( 'Erro ao inserir usuário: ' + E.Message) );
    end;
  finally
    Query.Free;
  end;
end;

function TUsuarioDAO.Atualizar(Usuario: TUsuario): Boolean;
var
  Query: TFDQuery;
  MsgErro: string;
begin
  Result := False;
  
  if not Usuario.IsValid(MsgErro) then
    raise Exception.Create(AppLog.RegistraLogEx(MsgErro));
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;

    if Trim(Usuario.Senha) <> '' then
    begin
      Query.SQL.Add('UPDATE TB_USUARIOS SET');
      Query.SQL.Add('  NOME = :NOME,');
      Query.SQL.Add('  EMAIL = :EMAIL,');
      Query.SQL.Add('  SENHA = :SENHA,');
      Query.SQL.Add('  TIPO = :TIPO,');
      Query.SQL.Add('  ATIVO = :ATIVO');
      Query.SQL.Add('WHERE ID = :ID');
      
      Query.ParamByName('SENHA').AsString := TCriptografia.CriptografarSenha(Usuario.Senha);
    end
    else
    begin
      Query.SQL.Add('UPDATE TB_USUARIOS SET');
      Query.SQL.Add('  NOME = :NOME,');
      Query.SQL.Add('  EMAIL = :EMAIL,');
      Query.SQL.Add('  TIPO = :TIPO,');
      Query.SQL.Add('  ATIVO = :ATIVO');
      Query.SQL.Add('WHERE ID = :ID');
    end;
    
    Query.ParamByName('NOME').AsString := Usuario.Nome;
    Query.ParamByName('EMAIL').AsString := Usuario.Email;
    Query.ParamByName('TIPO').AsString := Usuario.TipoStr;
    Query.ParamByName('ATIVO').AsInteger := Integer(Usuario.Ativo);
    Query.ParamByName('ID').AsInteger := Usuario.ID;
    
    try
      Query.ExecSQL;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create(AppLog.RegistraLogEx('Erro ao atualizar usuário: ' + E.Message));
    end;
  finally
    Query.Free;
  end;
end;

function TUsuarioDAO.Excluir(ID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('DELETE FROM TB_USUARIOS WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    
    try
      Query.ExecSQL;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create(AppLog.RegistraLogEx('Erro ao excluir usuário: ' + E.Message));
    end;
  finally
    Query.Free;
  end;
end;

function TUsuarioDAO.BuscarPorID(ID: Integer): TUsuario;
var
  Query: TFDQuery;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_USUARIOS WHERE ID = :ID');
    Query.ParamByName('ID').AsInteger := ID;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TUsuario.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Nome := Query.FieldByName('NOME').AsString;
      Result.Email := Query.FieldByName('EMAIL').AsString;
      Result.TipoStr := Query.FieldByName('TIPO').AsString;
      Result.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Result.Ativo := Query.FieldByName('ATIVO').AsBoolean;
    end;
  finally
    Query.Free;
  end;
end;

function TUsuarioDAO.BuscarPorEmail(const Email: string): TUsuario;
var
  Query: TFDQuery;
begin
  Result := nil;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_USUARIOS WHERE EMAIL = :EMAIL');
    Query.ParamByName('EMAIL').AsString := Email;
    Query.Open;
    
    if not Query.IsEmpty then
    begin
      Result := TUsuario.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Nome := Query.FieldByName('NOME').AsString;
      Result.Email := Query.FieldByName('EMAIL').AsString;
      Result.TipoStr := Query.FieldByName('TIPO').AsString;
      Result.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Result.Ativo := Query.FieldByName('ATIVO').AsBoolean;
    end;
  finally
    Query.Free;
  end;
end;

function TUsuarioDAO.ListarTodos(ApenasAtivos: Boolean = True): TObjectList<TUsuario>;
var
  Query: TFDQuery;
  Usuario: TUsuario;
begin
  Result := TObjectList<TUsuario>.Create;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_USUARIOS');
    
    if ApenasAtivos then
      Query.SQL.Add('WHERE ATIVO = 1');
      
    Query.SQL.Add('ORDER BY NOME');
    Query.Open;
    
    while not Query.Eof do
    begin
      Usuario := TUsuario.Create;
      Usuario.ID := Query.FieldByName('ID').AsInteger;
      Usuario.Nome := Query.FieldByName('NOME').AsString;
      Usuario.Email := Query.FieldByName('EMAIL').AsString;
      Usuario.TipoStr := Query.FieldByName('TIPO').AsString;
      Usuario.DataCadastro := Query.FieldByName('DATA_CADASTRO').AsDateTime;
      Usuario.Ativo := Query.FieldByName('ATIVO').AsBoolean;
      
      Result.Add(Usuario);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
