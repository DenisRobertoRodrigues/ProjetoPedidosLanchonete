program PedidosLanchonete;

uses
  Vcl.Forms,
  UInicializacao in 'UInicializacao.pas' {FmInicializacao},
  ULog in 'ULog.pas',
  UDMDTO in 'UDMDTO.pas' {DMDTO: TDataModule},
  SQLiteInstaller in 'conf\SQLiteInstaller.pas',
  uCriptografia in 'conf\uCriptografia.pas',
  uFormUsuario in 'uFormUsuario.pas' {FormUsuario},
  uUsuario in 'uUsuario.pas',
  uUsuarioDAO in 'uUsuarioDAO.pas',
  uCliente in 'uCliente.pas',
  uClienteDAO in 'uClienteDAO.pas',
  uFormCliente in 'uFormCliente.pas' {FormCliente},
  uFormIngrediente in 'uFormIngrediente.pas' {FormIngrediente},
  uIngrediente in 'uIngrediente.pas',
  uIngredienteDAO in 'uIngredienteDAO.pas',
  uFormProduto in 'uFormProduto.pas' {FormProduto},
  uProduto in 'uProduto.pas',
  uProdutoDAO in 'uProdutoDAO.pas',
  Vcl.Themes,
  Vcl.Styles,
  uFormMenu in 'uFormMenu.pas' {FormMenu},
  uPedido in 'uPedido.pas',
  uPedidoDAO in 'uPedidoDAO.pas',
  uFormPersonalizarProduto in 'uFormPersonalizarProduto.pas' {FormPersonalizarProduto},
  uFormPedido in 'uFormPedido.pas' {FormPedido};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Turquoise Gray');
  Application.CreateForm(TFmInicializacao, FmInicializacao);
  Application.CreateForm(TDMDTO, DMDTO);
  Application.Run;
end.
