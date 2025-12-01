unit UInicializacao;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles, System.SyncObjs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Mask, FireDAC.Comp.Client, Data.DB;

type
  TFmInicializacao = class(TForm)
    PnMenu: TPanel;
    PnMenuGeral: TPanel;
    PnMenuGerencial: TPanel;
    BtnGerenciamentoUsuarios: TBitBtn;
    BtnGerenciamentoClientes: TBitBtn;
    BtnGerenciamentoIngredientes: TBitBtn;
    BtnGerenciamentoProduto: TBitBtn;
    BtnMenu: TBitBtn;
    BtnPedidos: TBitBtn;
    PnDadosGerenciais: TPanel;
    PnLogin: TPanel;
    lblUsuario: TLabel;
    lblSenha: TLabel;
    EdtUsuario: TEdit;
    EdtSenha: TMaskEdit;
    BtnLogin: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnGerenciamentoUsuariosClick(Sender: TObject);
    procedure BtnLoginClick(Sender: TObject);
    procedure BtnGerenciamentoClientesClick(Sender: TObject);
    procedure BtnGerenciamentoIngredientesClick(Sender: TObject);
    procedure BtnGerenciamentoProdutoClick(Sender: TObject);
    procedure BtnMenuClick(Sender: TObject);
    procedure BtnPedidosClick(Sender: TObject);
  private
    caminhoArqConf: String;
    caminhoArqConfBD: String;
    procedure inicializarConf;
    function primeiraExecucao: Boolean;
    procedure configuraAplicacao;
  public
    { Public declarations }
  end;

var
  FmInicializacao: TFmInicializacao;

implementation

{$R *.dfm}

uses SQLiteInstaller, UDMDTO, ULog, uFormUsuario, uFormCliente,
  uFormIngrediente, uFormProduto, uFormMenu, uFormPedido, uUsuario, uUsuarioDAO,
  uCriptografia;

procedure TFmInicializacao.FormCreate(Sender: TObject);
begin
  inicializarConf;
end;

procedure TFmInicializacao.FormShow(Sender: TObject);
begin
  AppLog := TLog.Create;
  AppLog.SetaCaminho( ExtractFilePath(Application.ExeName) );
  AppLog.SetaUserLog('Sys');
  AppLog.RegistraLog('### Inicio Aplicação ###');

  if primeiraExecucao then
    configuraAplicacao
  else DMDTO.carregarConfBD(caminhoArqConf);

  try
    DMDTO.FDConexao.Connected := True;
    AppLog.RegistraLog('Conexão com Banco de Dados realizada com sucesso!')
  Except
    on e: exception do
    begin
      AppLog.RegistraLog('Não foi possível conectar ao banco de dados - '+ e.Message );
      Application.Terminate;
    end;
  end;

  PnLogin.Align := alClient;

end;

procedure TFmInicializacao.inicializarConf;
begin
  caminhoArqConf := ExtractFilePath(Application.ExeName)+'pedidos.ini';
  caminhoArqConfBD := ExtractFilePath(Application.ExeName)+'conf';
end;

function TFmInicializacao.primeiraExecucao: Boolean;
begin
  Result := not FileExists(caminhoArqConf);
end;

procedure TFmInicializacao.BtnGerenciamentoIngredientesClick(Sender: TObject);
begin
  Application.CreateForm(TFormIngrediente, FormIngrediente);
  FormIngrediente.Show;
end;

procedure TFmInicializacao.BtnGerenciamentoProdutoClick(Sender: TObject);
begin
  Application.CreateForm(TFormProduto, FormProduto);
  FormProduto.Show;
end;

procedure TFmInicializacao.BtnGerenciamentoClientesClick(Sender: TObject);
begin
  Application.CreateForm(TFormCliente, FormCliente);
  FormCliente.Show;
end;

procedure TFmInicializacao.BtnGerenciamentoUsuariosClick(Sender: TObject);
begin
  Application.CreateForm(TFormUsuario, FormUsuario);
  FormUsuario.Show;
end;

procedure TFmInicializacao.BtnMenuClick(Sender: TObject);
begin
  Application.CreateForm(TFormMenu, FormMenu);
  FormMenu.Show;
end;

procedure TFmInicializacao.BtnPedidosClick(Sender: TObject);
begin
  Application.CreateForm(TFormPedido, FormPedido);
  FormPedido.Show;
end;

procedure TFmInicializacao.BtnLoginClick(Sender: TObject);
var
  Query: TFDQuery;
  SenhaHash: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDTO.FDConexao;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT * FROM TB_USUARIOS');
    Query.SQL.Add('WHERE EMAIL = :EMAIL AND ATIVO = 1');
    Query.ParamByName('EMAIL').AsString := EdtUsuario.Text;
    Query.Open;

    if not Query.IsEmpty then
    begin
      SenhaHash := Query.FieldByName('SENHA').AsString;

      if TCriptografia.VerificarSenha(EdtSenha.Text, SenhaHash) then
      begin
        PnLogin.Visible := False;
        if Query.FieldByName('TIPO').AsString = 'N' then
        begin
          PnMenuGeral.Visible := True;
          PnMenuGerencial.Visible := False;
          PnDadosGerenciais.Visible := False;
        end else
        begin
          PnMenuGeral.Visible := True;
          PnMenuGerencial.Visible := True;
          PnDadosGerenciais.Visible := True;
        end;
      end else
        MessageDlg('Senha incorreta', TMsgDlgType.mtInformation, [mbOk], 1);
    end else
    begin
      EdtUsuario.Clear;
      EdtSenha.Clear;
      MessageDlg('Nenhum usuário encontrado com o login inserido', TMsgDlgType.mtInformation, [mbOk], 1);
    end;
  finally
    Query.Free;
  end;
end;

procedure TFmInicializacao.configuraAplicacao;
var
  LCriticalSectionTabelas: TCriticalSection;
begin
 AppLog.RegistraLog('Iniciando configuração do sistema');

 {$REGION 'Instala SQLite'}

 var Installer: TSQLiteInstaller;
 Installer := TSQLiteInstaller.Create(caminhoArqConfBD);
 try
   if not Installer.TestSQLiteConnection then
   begin
     AppLog.RegistraLog('Não encontrou SQLlite');

     if Installer.InstallSQLite then
       AppLog.RegistraLog('SQLite instalado com sucesso!')
     else AppLog.RegistraLog('Erro na Instalação do SQLite');

     if not Installer.ConfigureSQLite then
       AppLog.RegistraLog('não conseguiu configurar');

     if Installer.TestSQLiteConnection then
       AppLog.RegistraLog('SQLite está funcionando!');

   end else
     AppLog.RegistraLog('SQLite já Instalado');

 finally
   Installer.Free;
 end;
 {$ENDREGION}

 {$REGION 'Cria Banco de dados e tabelas'}

  LCriticalSectionTabelas := TCriticalSection.Create;
  LCriticalSectionTabelas.Enter;

  var Ini: TIniFile;
  Ini := TIniFile.Create(caminhoArqConf);
  try
    Ini.WriteString('SQLITE', 'Database', ExtractFilePath(Application.ExeName)+'DBPedidos.db');
    Ini.WriteString('SQLITE', 'VendorLib', caminhoArqConfBD);
  finally
    Ini.Free;
  end;

  DMDTO.carregarConfBD(caminhoArqConf);
  DMDTO.FDConexao.Connected := True;

  AppLog.RegistraLog('Iniciando criação das tabelas');

  var LSQL: String;
  LSQL := 'CREATE TABLE IF NOT EXISTS TB_INGREDIENTES ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  NOME VARCHAR(100) NOT NULL, '+
          '  IMAGEM BLOB, '+
          '  VALOR REAL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_PRODUTOS ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  NOME VARCHAR(100) NOT NULL, '+
          '  IMAGEM BLOB, '+
          '  VALOR REAL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_PRODUTOS_INGREDIENTES ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  ID_PRODUTO INTEGER NOT NULL, '+
          '  ID_INGREDIENTE INTEGER NOT NULL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1, '+
          'FOREIGN KEY (ID_PRODUTO) REFERENCES TB_PRODUTOS(ID) '+
          '  ON DELETE CASCADE '+
          '  ON UPDATE CASCADE, '+
          'FOREIGN KEY (ID_INGREDIENTE) REFERENCES TB_INGREDIENTES(ID) '+
          '  ON DELETE CASCADE '+
          '  ON UPDATE CASCADE, '+
          'UNIQUE(ID_PRODUTO, ID_INGREDIENTE) ) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_CLIENTES ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  NOME VARCHAR(100) NOT NULL, '+
          '  CONTATO VARCHAR(50) NOT NULL, '+
          '  DOCUMENTO VARCHAR(30) NOT NULL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_PEDIDOS ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  ID_CLIENTE INTEGER NOT NULL, '+
          '  TOTAL_ITENS REAL, '+
          '  TOTAL_ADICIONAL REAL, '+
          '  TOTAL_DESCONTO REAL, '+
          '  TOTAL_PEDIDO REAL, '+
          '  FORMA_PAGAMENTO VARCHAR(100), '+
          '  END_ENTREGA VARCHAR(255), '+
          '  OBS VARCHAR(255), '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_PEDIDOS_ITENS ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  ID_PEDIDO INTEGER NOT NULL, '+
          '  ID_PRODUTO INTEGER NOT NULL, '+
          '  QUANTIDADE INTEGER, '+
          '  OBSERVACAO TEXT(255), '+
          '  VALOR_ITEM REAL, '+
          '  TOTAL_VALOR REAL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1, '+
          'FOREIGN KEY (ID_PEDIDO) REFERENCES TB_PEDIDOS(ID) '+
          '    ON DELETE CASCADE '+
          '    ON UPDATE CASCADE, '+
          'FOREIGN KEY (ID_PRODUTO) REFERENCES TB_PRODUTOS(ID) '+
          '    ON DELETE RESTRICT '+
          '    ON UPDATE CASCADE) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_PEDIDOS_ITENS_AD ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  ID_PEDIDO INTEGER NOT NULL, '+
          '  ID_PRODUTO INTEGER NOT NULL, '+
          '  ID_INGREDIENTE INTEGER NOT NULL, '+
          '  VALOR REAL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1, '+
          'FOREIGN KEY (ID_PEDIDO) REFERENCES TB_PEDIDOS(ID) '+
          '  ON DELETE CASCADE '+
          '  ON UPDATE CASCADE, '+
          'FOREIGN KEY (ID_PRODUTO) REFERENCES TB_PRODUTOS(ID) '+
          '  ON DELETE RESTRICT '+
          '  ON UPDATE CASCADE, '+
          'FOREIGN KEY (ID_INGREDIENTE) REFERENCES TB_INGREDIENTES(ID) '+
          '  ON DELETE RESTRICT '+
          '  ON UPDATE CASCADE) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_PROMOCAO ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  TIPO VARCHAR(100) NOT NULL, '+
          '  REGRA BLOB NOT NULL, '+
          '  PERC_DESCONTO REAL, '+
          '  BRINDE INTEGER, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1, '+
          'FOREIGN KEY (BRINDE) REFERENCES TB_PRODUTOS(ID) '+
          '  ON DELETE SET NULL '+
          '  ON UPDATE CASCADE) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LSQL := 'CREATE TABLE IF NOT EXISTS TB_USUARIOS ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  NOME VARCHAR(100) NOT NULL, '+
          '  EMAIL VARCHAR(150) NOT NULL, '+
          '  SENHA VARCHAR(200) NOT NULL, '+
          '  TIPO VARCHAR(1) NOT NULL, '+
          '  DATA_CADASTRO DATETIME DEFAULT CURRENT_TIMESTAMP, '+
          '  ATIVO BOOLEAN DEFAULT 1) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  //erro forçado para simular log
  LSQL := 'CREATE TABLE IS NOT EXISTS TB_USUARIOS ( '+
          '  ID INTEGER PRIMARY KEY AUTOINCREMENT, '+
          '  ATIVO BOOLEAN DEFAULT 1) ';
  DMDTO.FDConexao.ExecutarSQL(LSQL);

  LCriticalSectionTabelas.Leave;
 {$ENDREGION}

 {$REGION 'Criação de dados básicos'}

 LSQL := 'INSERT INTO TB_USUARIOS '+
         ' (ID, NOME, EMAIL, SENHA, TIPO, DATA_CADASTRO, ATIVO) '+
         ' VALUES(1, ''ADMIN'', ''ADMIN@admin.com.br'', ''8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92'', ''A'', CURRENT_TIMESTAMP, 1) ';
 DMDTO.FDConexao.ExecutarSQL(LSQL);

 LSQL := 'INSERT INTO TB_USUARIOS '+
         ' (ID, NOME, EMAIL, SENHA, TIPO, DATA_CADASTRO, ATIVO) '+
         ' VALUES(2, ''USUARIO'', ''USUARIO@usuario.com.br'', ''481f6cc0511143ccdd7e2d1b1b94faf0a700a8b49cd13922a70b5ae28acaa8c5'', ''N'', CURRENT_TIMESTAMP, 1) ';
 DMDTO.FDConexao.ExecutarSQL(LSQL);

 LSQL := 'INSERT INTO TB_CLIENTES (ID, NOME, CONTATO, DOCUMENTO, DATA_CADASTRO, ATIVO) VALUES(1, ''CONSUMIDOR'', ''111111111'', ''1111111111'', CURRENT_TIMESTAMP, 1) ';
 DMDTO.FDConexao.ExecutarSQL(LSQL);

 LSQL := 'INSERT INTO TB_PROMOCAO '+
         ' (ID, TIPO, REGRA, PERC_DESCONTO, BRINDE, DATA_CADASTRO, ATIVO) '+
         ' VALUES(1, ''Desconto'', ''SELECT 1 FROM TB_PEDIDOS A WHERE A.TOTAL_PEDIDO > 50 AND A.ID = '', 3, 0, CURRENT_TIMESTAMP, 1) ';
 DMDTO.FDConexao.ExecutarSQL(LSQL);

 LSQL := 'INSERT INTO TB_PROMOCAO '+
         ' (ID, TIPO, REGRA, PERC_DESCONTO, BRINDE, DATA_CADASTRO, ATIVO) '+
         ' VALUES(1, ''Desconto'', ''SELECT 1 FROM TB_PEDIDOS A, TB_CLIENTES B '+
         '                          WHERE B.ID  = A.ID_CLIENTE '+
         '                            AND B.DATA_CADASTRO < DATE(''now'', ''-6 months'') '+
         '                            AND A.ID = '', '+
         '  5, 0, CURRENT_TIMESTAMP, 1) ';
 DMDTO.FDConexao.ExecutarSQL(LSQL);
 {$ENDREGION}

 AppLog.RegistraLog('Finalizado configuração do sistema');
end;

end.
