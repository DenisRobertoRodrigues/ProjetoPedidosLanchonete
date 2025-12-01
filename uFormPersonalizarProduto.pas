unit uFormPersonalizarProduto;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.CheckLst, System.Generics.Collections, uProduto, uIngrediente, uPedido;

type
  TFormPersonalizarProduto = class(TForm)
    pnlTop: TPanel;
    lblTitulo: TLabel;
    pnlBottom: TPanel;
    btnConfirmar: TButton;
    btnCancelar: TButton;
    pnlCenter: TPanel;
    gbIngredientesOriginais: TGroupBox;
    chkListIngredientesOriginais: TCheckListBox;
    gbIngredientesAdicionais: TGroupBox;
    chkListIngredientesAdicionais: TCheckListBox;
    lblInfo: TLabel;
    pnlValores: TPanel;
    lblValorOriginal: TLabel;
    lblValorFinal: TLabel;
    lblValorOriginalTexto: TLabel;
    lblValorFinalTexto: TLabel;
    gbObservacao: TGroupBox;
    memoObservacao: TMemo;
    lblQuantidade: TLabel;
    edtQuantidade: TEdit;
    btnMenos: TButton;
    btnMais: TButton;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConfirmarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure chkListIngredientesOriginaisClickCheck(Sender: TObject);
    procedure chkListIngredientesAdicionaisClickCheck(Sender: TObject);
    procedure btnMenosClick(Sender: TObject);
    procedure btnMaisClick(Sender: TObject);
    procedure edtQuantidadeChange(Sender: TObject);
    
  private
    FProduto: TProduto;
    FItemPedido: TItemPedido;
    FListaIngredientesDisponiveis: TObjectList<TIngrediente>;
    
    procedure CarregarIngredientesOriginais;
    procedure CarregarIngredientesAdicionais;
    procedure CalcularValorFinal;
    procedure AtualizarQuantidade;
    
  public
    property Produto: TProduto read FProduto write FProduto;
    property ItemPedido: TItemPedido read FItemPedido;
    
    procedure Inicializar(AProduto: TProduto; AListaIngredientes: TObjectList<TIngrediente>);
  end;

var
  FormPersonalizarProduto: TFormPersonalizarProduto;

implementation

{$R *.dfm}

procedure TFormPersonalizarProduto.FormCreate(Sender: TObject);
begin
  FItemPedido := TItemPedido.Create;
end;

procedure TFormPersonalizarProduto.FormDestroy(Sender: TObject);
begin
  FItemPedido.Free;
end;

procedure TFormPersonalizarProduto.Inicializar(AProduto: TProduto; AListaIngredientes: TObjectList<TIngrediente>);
begin
  FProduto := AProduto;
  FListaIngredientesDisponiveis := AListaIngredientes;

  FItemPedido.IDProduto := FProduto.ID;
  FItemPedido.NomeProduto := FProduto.Nome;
  FItemPedido.ValorUnitarioOriginal := FProduto.Valor;
  FItemPedido.Quantidade := 1;
  FItemPedido.AtualizarValorFinal;

  lblTitulo.Caption := 'Personalizar: ' + FProduto.Nome;

  CarregarIngredientesOriginais;
  CarregarIngredientesAdicionais;

  CalcularValorFinal;
  AtualizarQuantidade;
end;

procedure TFormPersonalizarProduto.CarregarIngredientesOriginais;
var
  ProdutoIngrediente: TProdutoIngrediente;
  Ingrediente: TIngrediente;
  Index: Integer;
begin
  chkListIngredientesOriginais.Items.Clear;
  
  if FProduto.Ingredientes.Count = 0 then
  begin
    gbIngredientesOriginais.Enabled := False;
    chkListIngredientesOriginais.Items.Add('(Nenhum ingrediente cadastrado)');
    Exit;
  end;

  for ProdutoIngrediente in FProduto.Ingredientes do
  begin
    for Ingrediente in FListaIngredientesDisponiveis do
    begin
      if Ingrediente.ID = ProdutoIngrediente.IDIngrediente then
      begin
        Index := chkListIngredientesOriginais.Items.AddObject(
          Format('%s (R$ %.2f)', [Ingrediente.Nome, Ingrediente.Valor]),
          TObject(Ingrediente.ID)
        );

        chkListIngredientesOriginais.Checked[Index] := True;
        chkListIngredientesOriginais.ItemEnabled[Index] := True;
        
        Break;
      end;
    end;
  end;
end;

procedure TFormPersonalizarProduto.CarregarIngredientesAdicionais;
var
  Ingrediente: TIngrediente;
  ProdutoIngrediente: TProdutoIngrediente;
  JaExiste: Boolean;
begin
  chkListIngredientesAdicionais.Items.Clear;

  for Ingrediente in FListaIngredientesDisponiveis do
  begin
    JaExiste := False;

    for ProdutoIngrediente in FProduto.Ingredientes do
    begin
      if ProdutoIngrediente.IDIngrediente = Ingrediente.ID then
      begin
        JaExiste := True;
        Break;
      end;
    end;
    
    if not JaExiste then
    begin
      chkListIngredientesAdicionais.Items.AddObject(
        Format('%s (+ R$ %.2f)', [Ingrediente.Nome, Ingrediente.Valor]),
        TObject(Ingrediente.ID)
      );
    end;
  end;
  
  if chkListIngredientesAdicionais.Items.Count = 0 then
  begin
    chkListIngredientesAdicionais.Items.Add('(Nenhum ingrediente disponível)');
    chkListIngredientesAdicionais.ItemEnabled[0] := False;
  end;
end;

procedure TFormPersonalizarProduto.chkListIngredientesOriginaisClickCheck(Sender: TObject);
var
  Index: Integer;
  IDIngrediente: Integer;
  Ingrediente: TIngrediente;
  ProdutoIngrediente: TProdutoIngrediente;
begin
  Index := chkListIngredientesOriginais.ItemIndex;
  if Index < 0 then Exit;
  
  IDIngrediente := Integer(chkListIngredientesOriginais.Items.Objects[Index]);

  for Ingrediente in FListaIngredientesDisponiveis do
  begin
    if Ingrediente.ID = IDIngrediente then
    begin
      if not chkListIngredientesOriginais.Checked[Index] then
      begin
        FItemPedido.RemoverIngrediente(
          Ingrediente.ID,
          Ingrediente.Nome,
          Ingrediente.Valor
        );
      end
      else
      begin
        var I: Integer;
        for I := FItemPedido.IngredientesPersonalizados.Count - 1 downto 0 do
        begin
          if (FItemPedido.IngredientesPersonalizados[I].IDIngrediente = IDIngrediente) and
             (FItemPedido.IngredientesPersonalizados[I].Removido) then
          begin
            FItemPedido.IngredientesPersonalizados.Delete(I);
            Break;
          end;
        end;
        FItemPedido.AtualizarValorFinal;
      end;
      
      Break;
    end;
  end;
  
  CalcularValorFinal;
end;

procedure TFormPersonalizarProduto.chkListIngredientesAdicionaisClickCheck(Sender: TObject);
var
  Index: Integer;
  IDIngrediente: Integer;
  Ingrediente: TIngrediente;
begin
  Index := chkListIngredientesAdicionais.ItemIndex;
  if Index < 0 then Exit;
  
  IDIngrediente := Integer(chkListIngredientesAdicionais.Items.Objects[Index]);

  for Ingrediente in FListaIngredientesDisponiveis do
  begin
    if Ingrediente.ID = IDIngrediente then
    begin
      if chkListIngredientesAdicionais.Checked[Index] then
      begin
        FItemPedido.AdicionarIngrediente(
          Ingrediente.ID,
          Ingrediente.Nome,
          Ingrediente.Valor
        );
      end
      else
      begin
        var I: Integer;
        for I := FItemPedido.IngredientesPersonalizados.Count - 1 downto 0 do
        begin
          if (FItemPedido.IngredientesPersonalizados[I].IDIngrediente = IDIngrediente) and
             (not FItemPedido.IngredientesPersonalizados[I].Removido) then
          begin
            FItemPedido.IngredientesPersonalizados.Delete(I);
            Break;
          end;
        end;
        FItemPedido.AtualizarValorFinal;
      end;
      
      Break;
    end;
  end;
  
  CalcularValorFinal;
end;

procedure TFormPersonalizarProduto.CalcularValorFinal;
begin
  lblValorOriginal.Caption := FormatFloat('R$ #,##0.00', FItemPedido.ValorUnitarioOriginal);
  lblValorFinal.Caption := FormatFloat('R$ #,##0.00', FItemPedido.ValorUnitarioFinal);

  if FItemPedido.ValorUnitarioFinal <> FItemPedido.ValorUnitarioOriginal then
  begin
    lblValorFinal.Font.Color := clGreen;
    lblValorFinal.Font.Style := [fsBold];
  end
  else
  begin
    lblValorFinal.Font.Color := clWindowText;
    lblValorFinal.Font.Style := [];
  end;
end;

procedure TFormPersonalizarProduto.btnMaisClick(Sender: TObject);
begin
  FItemPedido.Quantidade := FItemPedido.Quantidade + 1;
  AtualizarQuantidade;
end;

procedure TFormPersonalizarProduto.btnMenosClick(Sender: TObject);
begin
  if FItemPedido.Quantidade > 1 then
  begin
    FItemPedido.Quantidade := FItemPedido.Quantidade - 1;
    AtualizarQuantidade;
  end;
end;

procedure TFormPersonalizarProduto.edtQuantidadeChange(Sender: TObject);
var
  Qtd: Integer;
begin
  Qtd := StrToIntDef(edtQuantidade.Text, 1);
  if Qtd < 1 then
    Qtd := 1;
  
  FItemPedido.Quantidade := Qtd;
  edtQuantidade.Text := IntToStr(Qtd);
end;

procedure TFormPersonalizarProduto.AtualizarQuantidade;
begin
  edtQuantidade.Text := IntToStr(FItemPedido.Quantidade);
  btnMenos.Enabled := FItemPedido.Quantidade > 1;
end;

procedure TFormPersonalizarProduto.btnConfirmarClick(Sender: TObject);
begin
  FItemPedido.Observacao := Trim(memoObservacao.Text);
  ModalResult := mrOk;
end;

procedure TFormPersonalizarProduto.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
