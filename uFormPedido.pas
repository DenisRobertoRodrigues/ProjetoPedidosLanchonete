unit uFormPedido;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, System.Generics.Collections, Vcl.ComCtrls,
  uPedido, uProduto, uProdutoDAO, uIngrediente, uIngredienteDAO,
  uCliente, uClienteDAO, uPedidoDAO, FireDAC.Comp.Client, uFormPersonalizarProduto;

type
  TFormPedido = class(TForm)
    pnlTop: TPanel;
    lblTitulo: TLabel;
    pnlBottom: TPanel;
    PageControl1: TPageControl;
    tsProdutos: TTabSheet;
    tsCarrinho: TTabSheet;
    pnlProdutos: TPanel;
    lstProdutos: TListBox;
    pnlDetalhesProduto: TPanel;
    lblNomeProduto: TLabel;
    lblValorProduto: TLabel;
    memoIngredientesProduto: TMemo;
    btnAdicionarProduto: TButton;
    GridCarrinho: TStringGrid;
    pnlTotais: TPanel;
    lblTotalPedido: TLabel;
    lblTotalPedidoValor: TLabel;
    btnRemoverItem: TButton;
    btnLimparCarrinho: TButton;
    btnFinalizarPedido: TButton;
    pnlCliente: TPanel;
    lblCliente: TLabel;
    cmbClientes: TComboBox;
    btnNovoCliente: TButton;
    pnlCotacao: TPanel;
    lblTotalDolar: TLabel;
    lblTotalDolarValor: TLabel;
    lblFiltroProduto: TLabel;
    edtFiltroProduto: TEdit;
    lblDetalhesProduto: TLabel;
    pnlBotoesCarrinho: TPanel;
    lblIngredientesProduto: TLabel;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstProdutosClick(Sender: TObject);
    procedure btnAdicionarProdutoClick(Sender: TObject);
    procedure btnRemoverItemClick(Sender: TObject);
    procedure btnLimparCarrinhoClick(Sender: TObject);
    procedure btnFinalizarPedidoClick(Sender: TObject);
    procedure edtFiltroProdutoChange(Sender: TObject);
    
  private
    FPedidoAtual: TPedido;
    FProdutoDAO: TProdutoDAO;
    FIngredienteDAO: TIngredienteDAO;
    FClienteDAO: TClienteDAO;
    FPedidoDAO: TPedidoDAO;
    FListaProdutos: TObjectList<TProduto>;
    FListaIngredientes: TObjectList<TIngrediente>;
    FListaClientes: TObjectList<TCliente>;
    
    procedure CarregarProdutos;
    procedure CarregarIngredientes;
    procedure CarregarClientes;
    procedure ExibirProdutos;
    procedure ExibirDetalhesProduto(Produto: TProduto);
    procedure AtualizarCarrinho;
    procedure AtualizarTotais;
    procedure ConfigurarGridCarrinho;
    procedure LimparPedido;
    function GetProdutoSelecionado: TProduto;
    
  public
    { Public declarations }
  end;

var
  FormPedido: TFormPedido;

implementation

uses
  uDMDTO;

{$R *.dfm}

procedure TFormPedido.FormCreate(Sender: TObject);
begin
  FListaProdutos := TObjectList<TProduto>.Create;
  FListaIngredientes := TObjectList<TIngrediente>.Create;
  FListaClientes := TObjectList<TCliente>.Create;
  
  FPedidoAtual := TPedido.Create;
  FPedidoAtual.CotacaoDolar := 5.50;

  ConfigurarGridCarrinho;
  CarregarProdutos;
  CarregarIngredientes;
  CarregarClientes;
  ExibirProdutos;
  
  PageControl1.ActivePage := tsProdutos;
end;

procedure TFormPedido.FormDestroy(Sender: TObject);
begin
  FListaClientes.Free;
  FListaIngredientes.Free;
  FListaProdutos.Free;
  FPedidoAtual.Free;
  FPedidoDAO.Free;
  FClienteDAO.Free;
  FIngredienteDAO.Free;
  FProdutoDAO.Free;
end;

procedure TFormPedido.ConfigurarGridCarrinho;
begin
  GridCarrinho.ColCount := 7;
  GridCarrinho.RowCount := 1;
  
  GridCarrinho.Cells[0, 0] := 'Item';
  GridCarrinho.Cells[1, 0] := 'Produto';
  GridCarrinho.Cells[2, 0] := 'Personalização';
  GridCarrinho.Cells[3, 0] := 'Vlr. Unit.';
  GridCarrinho.Cells[4, 0] := 'Qtd';
  GridCarrinho.Cells[5, 0] := 'Vlr. Total';
  GridCarrinho.Cells[6, 0] := 'Obs';
  
  GridCarrinho.ColWidths[0] := 40;
  GridCarrinho.ColWidths[1] := 200;
  GridCarrinho.ColWidths[2] := 250;
  GridCarrinho.ColWidths[3] := 80;
  GridCarrinho.ColWidths[4] := 50;
  GridCarrinho.ColWidths[5] := 80;
  GridCarrinho.ColWidths[6] := 150;
end;

procedure TFormPedido.CarregarProdutos;
begin
  FListaProdutos.Clear;
  FListaProdutos.Free;
  FListaProdutos := FProdutoDAO.ListarTodos(True);
end;

procedure TFormPedido.CarregarIngredientes;
begin
  FListaIngredientes.Clear;
  FListaIngredientes.Free;
  FListaIngredientes := FIngredienteDAO.ListarTodos(True);
end;

procedure TFormPedido.CarregarClientes;
var
  Cliente: TCliente;
begin
  FListaClientes.Clear;
  FListaClientes.Free;
  FListaClientes := FClienteDAO.ListarTodos(True);
  
  cmbClientes.Items.Clear;
  cmbClientes.Items.Add('-- Selecione um cliente --');
  
  for Cliente in FListaClientes do
  begin
    cmbClientes.Items.AddObject(
      Format('%s - %s', [Cliente.Nome, Cliente.Contato]),
      TObject(Cliente.ID)
    );
  end;
  
  cmbClientes.ItemIndex := 0;
end;

procedure TFormPedido.ExibirProdutos;
var
  I: Integer;
  Produto: TProduto;
  Filtro: string;
begin
  lstProdutos.Items.Clear;
  Filtro := UpperCase(Trim(edtFiltroProduto.Text));
  
  for I := 0 to FListaProdutos.Count - 1 do
  begin
    Produto := FListaProdutos[I];
    
    if (Filtro <> '') and (Pos(Filtro, UpperCase(Produto.Nome)) = 0) then
      Continue;
    
    lstProdutos.Items.AddObject(
      Format('%s - R$ %.2f', [Produto.Nome, Produto.Valor]),
      TObject(I)
    );
  end;
end;

procedure TFormPedido.ExibirDetalhesProduto(Produto: TProduto);
var
  ProdutoIngrediente: TProdutoIngrediente;
  Ingredientes: string;
begin
  lblNomeProduto.Caption := Produto.Nome;
  lblValorProduto.Caption := FormatFloat('R$ #,##0.00', Produto.Valor);

  // Montar lista de ingredientes
  Ingredientes := '';
  for ProdutoIngrediente in Produto.Ingredientes do
  begin
    if Ingredientes <> '' then
      Ingredientes := Ingredientes + #13#10;
    Ingredientes := Ingredientes + '• ' + ProdutoIngrediente.NomeIngrediente;
  end;
  
  if Ingredientes = '' then
    Ingredientes := 'Nenhum ingrediente cadastrado';
  
  memoIngredientesProduto.Text := Ingredientes;
  
  btnAdicionarProduto.Enabled := True;
  
  if Produto.Ingredientes.Count > 0 then
    btnAdicionarProduto.Caption := 'Personalizar e Adicionar'
  else
    btnAdicionarProduto.Caption := 'Adicionar ao Pedido';
end;

function TFormPedido.GetProdutoSelecionado: TProduto;
var
  Index: Integer;
begin
  Result := nil;
  
  if lstProdutos.ItemIndex < 0 then
    Exit;
  
  Index := Integer(lstProdutos.Items.Objects[lstProdutos.ItemIndex]);
  
  if (Index >= 0) and (Index < FListaProdutos.Count) then
    Result := FListaProdutos[Index];
end;

procedure TFormPedido.lstProdutosClick(Sender: TObject);
var
  Produto: TProduto;
begin
  Produto := GetProdutoSelecionado;
  
  if Assigned(Produto) then
    ExibirDetalhesProduto(Produto);
end;

procedure TFormPedido.btnAdicionarProdutoClick(Sender: TObject);
var
  Produto: TProduto;
  FormPersonalizar: TFormPersonalizarProduto;
  ItemPedido: TItemPedido;
begin
  Produto := GetProdutoSelecionado;
  
  if not Assigned(Produto) then
  begin
    ShowMessage('Selecione um produto!');
    Exit;
  end;
  
  // Se o produto tem ingredientes, abrir tela de personalização
  if Produto.Ingredientes.Count > 0 then
  begin
    FormPersonalizar := TFormPersonalizarProduto.Create(Self);
    try
      FormPersonalizar.Inicializar(Produto, FListaIngredientes);
      
      if FormPersonalizar.ShowModal = mrOk then
      begin

        ItemPedido := TItemPedido.Create;
        ItemPedido.IDProduto := FormPersonalizar.ItemPedido.IDProduto;
        ItemPedido.NomeProduto := FormPersonalizar.ItemPedido.NomeProduto;
        ItemPedido.ValorUnitarioOriginal := FormPersonalizar.ItemPedido.ValorUnitarioOriginal;
        ItemPedido.Quantidade := FormPersonalizar.ItemPedido.Quantidade;
        ItemPedido.Observacao := FormPersonalizar.ItemPedido.Observacao;

        var Ing: TItemPedidoIngrediente;
        for Ing in FormPersonalizar.ItemPedido.IngredientesPersonalizados do
        begin
          var NovoIng := TItemPedidoIngrediente.Create;
          NovoIng.IDIngrediente := Ing.IDIngrediente;
          NovoIng.NomeIngrediente := Ing.NomeIngrediente;
          NovoIng.ValorIngrediente := Ing.ValorIngrediente;
          NovoIng.Removido := Ing.Removido;
          ItemPedido.IngredientesPersonalizados.Add(NovoIng);
        end;
        
        ItemPedido.AtualizarValorFinal;
        
        FPedidoAtual.AdicionarItem(ItemPedido);
        AtualizarCarrinho;
        AtualizarTotais;
        
        PageControl1.ActivePage := tsCarrinho;
      end;
    finally
      FormPersonalizar.Free;
    end;
  end
  else
  begin
    // Produto sem ingredientes - adicionar direto
    ItemPedido := TItemPedido.Create;
    ItemPedido.IDProduto := Produto.ID;
    ItemPedido.NomeProduto := Produto.Nome;
    ItemPedido.ValorUnitarioOriginal := Produto.Valor;
    ItemPedido.Quantidade := 1;
    ItemPedido.AtualizarValorFinal;
    
    FPedidoAtual.AdicionarItem(ItemPedido);
    AtualizarCarrinho;
    AtualizarTotais;
    
    PageControl1.ActivePage := tsCarrinho;
  end;
end;

procedure TFormPedido.AtualizarCarrinho;
var
  I: Integer;
  Item: TItemPedido;
begin
  GridCarrinho.RowCount := 1;
  
  for I := 0 to FPedidoAtual.Itens.Count - 1 do
  begin
    Item := FPedidoAtual.Itens[I];
    
    GridCarrinho.RowCount := GridCarrinho.RowCount + 1;
    GridCarrinho.Cells[0, GridCarrinho.RowCount - 1] := IntToStr(I + 1);
    GridCarrinho.Cells[1, GridCarrinho.RowCount - 1] := Item.NomeProduto;
    GridCarrinho.Cells[2, GridCarrinho.RowCount - 1] := Item.GetDescricaoPersonalizacao;
    GridCarrinho.Cells[3, GridCarrinho.RowCount - 1] := FormatFloat('#,##0.00', Item.ValorUnitarioFinal);
    GridCarrinho.Cells[4, GridCarrinho.RowCount - 1] := IntToStr(Item.Quantidade);
    GridCarrinho.Cells[5, GridCarrinho.RowCount - 1] := FormatFloat('#,##0.00', Item.GetValorTotal);
    GridCarrinho.Cells[6, GridCarrinho.RowCount - 1] := Item.Observacao;
  end;
  
  btnRemoverItem.Enabled := FPedidoAtual.Itens.Count > 0;
  btnLimparCarrinho.Enabled := FPedidoAtual.Itens.Count > 0;
  btnFinalizarPedido.Enabled := FPedidoAtual.Itens.Count > 0;
end;

procedure TFormPedido.AtualizarTotais;
begin
  lblTotalPedidoValor.Caption := FormatFloat('R$ #,##0.00', FPedidoAtual.ValorTotal);
  lblTotalDolarValor.Caption := FormatFloat('US$ #,##0.00', FPedidoAtual.ValorTotalDolar);
end;

procedure TFormPedido.btnRemoverItemClick(Sender: TObject);
var
  Index: Integer;
begin
  if GridCarrinho.Row < 1 then
  begin
    ShowMessage('Selecione um item para remover!');
    Exit;
  end;
  
  Index := GridCarrinho.Row - 1;
  
  if MessageDlg('Deseja remover este item do pedido?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FPedidoAtual.RemoverItem(Index);
    AtualizarCarrinho;
    AtualizarTotais;
  end;
end;

procedure TFormPedido.btnLimparCarrinhoClick(Sender: TObject);
begin
  if MessageDlg('Deseja limpar todo o carrinho?', 
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FPedidoAtual.LimparItens;
    AtualizarCarrinho;
    AtualizarTotais;
  end;
end;

procedure TFormPedido.LimparPedido;
begin
  FPedidoAtual.Free;
  FPedidoAtual := TPedido.Create;

  cmbClientes.ItemIndex := 0;
  AtualizarCarrinho;
  AtualizarTotais;
end;

procedure TFormPedido.btnFinalizarPedidoClick(Sender: TObject);
var
  Mensagem: string;
  I: Integer;
  Item: TItemPedido;
  FormaPagamento, EnderecoEntrega, Obs: string;
  IDPedido: Integer;
begin
  if FPedidoAtual.Itens.Count = 0 then
  begin
    ShowMessage('Adicione itens ao pedido!');
    Exit;
  end;
  
  if cmbClientes.ItemIndex <= 0 then
  begin
    ShowMessage('Selecione um cliente!');
    cmbClientes.SetFocus;
    Exit;
  end;

  Mensagem := '═══════════════════════════════════' + #13#10;
  Mensagem := Mensagem + '     RESUMO DO PEDIDO' + #13#10;
  Mensagem := Mensagem + '═══════════════════════════════════' + #13#10 + #13#10;
  Mensagem := Mensagem + 'Cliente: ' + cmbClientes.Text + #13#10 + #13#10;
  Mensagem := Mensagem + '───────────────────────────────────' + #13#10;
  Mensagem := Mensagem + 'ITENS DO PEDIDO:' + #13#10;
  Mensagem := Mensagem + '───────────────────────────────────' + #13#10;
  
  for I := 0 to FPedidoAtual.Itens.Count - 1 do
  begin
    Item := FPedidoAtual.Itens[I];
    Mensagem := Mensagem + Format('%d. %s', [I + 1, Item.NomeProduto]) + #13#10;
    Mensagem := Mensagem + Format('   Qtd: %d × R$ %.2f = R$ %.2f', [
      Item.Quantidade,
      Item.ValorUnitarioFinal,
      Item.GetValorTotal
    ]) + #13#10;
    
    if Item.GetDescricaoPersonalizacao <> '' then
      Mensagem := Mensagem + '   • ' + Item.GetDescricaoPersonalizacao + #13#10;
      
    if Trim(Item.Observacao) <> '' then
      Mensagem := Mensagem + '   Obs: ' + Item.Observacao + #13#10;
      
    Mensagem := Mensagem + #13#10;
  end;
  
  Mensagem := Mensagem + '───────────────────────────────────' + #13#10;
  Mensagem := Mensagem + Format('Total Itens:      R$ %10.2f', [FPedidoAtual.TotalItens]) + #13#10;
  Mensagem := Mensagem + Format('Total Adicionais: R$ %10.2f', [FPedidoAtual.TotalAdicional]) + #13#10;
  Mensagem := Mensagem + Format('Total Descontos:  R$ %10.2f', [FPedidoAtual.TotalDesconto]) + #13#10;
  Mensagem := Mensagem + '───────────────────────────────────' + #13#10;
  Mensagem := Mensagem + Format('TOTAL DO PEDIDO:  R$ %10.2f', [FPedidoAtual.ValorTotal]) + #13#10;
  Mensagem := Mensagem + Format('Em Dólar (US$):   US$ %9.2f', [FPedidoAtual.ValorTotalDolar]) + #13#10;
  Mensagem := Mensagem + '═══════════════════════════════════' + #13#10 + #13#10;
  Mensagem := Mensagem + 'Confirmar e salvar pedido?';
  
  if MessageDlg(Mensagem, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Solicitar forma de pagamento
    FormaPagamento := InputBox('Forma de Pagamento', 
      'Informe a forma de pagamento:', 'Dinheiro');
    
    if Trim(FormaPagamento) = '' then
      FormaPagamento := 'Não informado';
    
    // Solicitar endereço de entrega
    EnderecoEntrega := InputBox('Endereço de Entrega', 
      'Informe o endereço de entrega (deixe em branco para retirada):', '');
    
    if Trim(EnderecoEntrega) = '' then
      EnderecoEntrega := 'Retirada no local';

    try
      FPedidoAtual.IDCliente := Integer(cmbClientes.Items.Objects[cmbClientes.ItemIndex]);
      FPedidoAtual.NomeCliente := cmbClientes.Text;
      FPedidoAtual.FormaPagamento := FormaPagamento;
      FPedidoAtual.EnderecoEntrega := EnderecoEntrega;
      FPedidoAtual.DataPedido := Now;
      FPedidoAtual.Status := 'Novo';

      IDPedido := FPedidoDAO.SalvarPedido(FPedidoAtual);
      
      if IDPedido > 0 then
      begin
        ShowMessage(Format(
          '╔═══════════════════════════════════╗' + #13#10 +
          '║   PEDIDO SALVO COM SUCESSO!       ║' + #13#10 +
          '╚═══════════════════════════════════╝' + #13#10 + #13#10 +
          'Número do Pedido: %d' + #13#10 + #13#10 +
          'Total: R$ %.2f' + #13#10 +
          'Forma de Pagamento: %s' + #13#10 +
          'Endereço: %s',
          [IDPedido, 
           FPedidoAtual.ValorTotal,
           FormaPagamento,
           EnderecoEntrega]
        ));
        
        LimparPedido;
        PageControl1.ActivePage := tsProdutos;
      end
      else
      begin
        ShowMessage('Erro ao salvar pedido. Tente novamente.');
      end;
    except
      on E: Exception do
        ShowMessage('Erro ao salvar pedido: ' + E.Message);
    end;
  end;
end;

procedure TFormPedido.edtFiltroProdutoChange(Sender: TObject);
begin
  ExibirProdutos;
end;

end.
